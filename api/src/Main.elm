module Main exposing (Effect(..), Model, Msg(..), init, main, update)

import Aaguid
import Api
import Data.CredentialId as CredentialId exposing (CredentialId)
import Data.Email as Email exposing (Email)
import Data.LinkToken as LinkToken exposing (LinkToken)
import Data.Origin as Origin exposing (Origin)
import Data.SessionToken as SessionToken exposing (SessionToken)
import Data.SignUp as SignUp
import Data.UserId as UserId
import Data.Username as Username
import Db
import Dict
import InteropDefinitions as Interop exposing (Request, Response)
import InteropPorts
import Json.Decode as Decode
import Json.Encode as Encode
import Path
import Services.Brevo as Brevo
import Services.Mailer as Mailer
import Services.Mailer.Layout as Layout
import Services.Mailer.PasskeyAdded
import Services.Mailer.PasskeyRemoved
import Services.Mailer.SignIn
import Services.Mailer.SignUp
import Services.Neon as Neon
import Task
import Time
import WebAuthn


main : Program Decode.Value Model Msg
main =
    Platform.worker
        { init =
            \flags ->
                init flags
                    |> Tuple.mapSecond perform
        , update =
            \msg model ->
                update msg model
                    |> Tuple.mapSecond perform
        , subscriptions = \_ -> Sub.map GotInterop InteropPorts.toElm
        }



-- MODEL


type alias Model =
    { db : Result String Neon.Connection
    , email : Result String Mailer.Mailer
    , origin : Origin
    , release : Api.Release
    }


init : Decode.Value -> ( Model, Effect )
init flags =
    case InteropPorts.decodeFlags flags of
        Ok config ->
            let
                release =
                    { releaseId = config.releaseId, releaseUrl = config.releaseUrl, region = config.region }
            in
            case Origin.parse config.origin of
                Ok origin ->
                    let
                        db =
                            Db.connect config

                        email =
                            Mailer.configure config
                    in
                    ( { db = db
                      , email = email
                      , origin = origin
                      , release = release
                      }
                    , Batch
                        [ case db of
                            Ok connection ->
                                log Interop.Info ("Sending queries to " ++ Neon.endpoint connection)

                            Err problem ->
                                log Interop.Warn ("No database, so database routes answer 503: " ++ problem)
                        , case email of
                            Ok _ ->
                                NoEffect

                            Err problem ->
                                log Interop.Warn ("No email, so nothing is sent: " ++ problem)
                        ]
                    )

                Err problem ->
                    ( broken problem release
                    , log Interop.Error ("Bad origin: " ++ problem)
                    )

        Err problem ->
            ( broken "The server's flags did not decode." { releaseId = Nothing, releaseUrl = Nothing, region = Nothing }
            , log Interop.Error ("Bad flags: " ++ Decode.errorToString problem)
            )


broken : String -> Api.Release -> Model
broken problem release =
    { db = Err problem
    , email = Err problem
    , origin = Origin.local
    , release = release
    }


log : Interop.Level -> String -> Effect
log level message =
    SendToJs (Interop.Log level message)


respond : Interop.Resolver -> Response -> Effect
respond resolver response =
    SendToJs (Interop.Respond resolver response)



-- UPDATE


type Msg
    = GotInterop (Result Decode.Error Interop.ToElm)
    | DatabaseChecked Interop.Resolver ( Int, Result Neon.Error Db.RateLimit )
    | DatabaseFailed Interop.Resolver String Neon.Error
    | LinkLimited Interop.Resolver Ask ( Db.RateLimit, Db.RateLimit )
    | SignUpLinkCreated Interop.Resolver SignUp.SignUp Db.SignUpLink
    | SignInLinkCreated Interop.Resolver Email Db.SignInLink
    | LinkSent Interop.Resolver (Result Brevo.Error Brevo.Sent)
    | SessionFound Interop.Resolver (Maybe Api.Account)
    | SessionEnded Interop.Resolver
    | VerificationFound Interop.Resolver (Maybe Api.Verification)
    | VerificationConsumed Interop.Resolver Db.Consumed
    | PasskeysFound Interop.Resolver (Maybe (List Api.Passkey))
    | PasskeyStarted Interop.Resolver (Maybe Db.Ceremony)
    | PasskeyAdded Interop.Resolver (Maybe Db.Added)
    | PasskeyRemoved Interop.Resolver CredentialId (Maybe Db.Removed)
    | Notified Interop.Resolver Response (Result Brevo.Error Brevo.Sent)
    | SignInStarted Interop.Resolver Db.SignInChallenge
    | KeyFound Interop.Resolver { signed : String, signature : String, signIn : Interop.SignIn } (Maybe String)
    | PasskeySignedIn Interop.Resolver Db.PasskeySignIn


type Ask
    = SignUpAsked SignUp.SignUp
    | SignInAsked Email


askedBy : Ask -> Email
askedBy ask =
    case ask of
        SignUpAsked signUp ->
            signUp.email

        SignInAsked email ->
            email


type Reply
    = Now Response
    | Later (Interop.Resolver -> Effect)


update : Msg -> Model -> ( Model, Effect )
update msg model =
    case msg of
        GotInterop (Ok (Interop.GotRequest resolver request)) ->
            ( model
            , case route model request of
                Now response ->
                    respond resolver response

                Later toEffect ->
                    toEffect resolver
            )

        DatabaseChecked resolver ( _, Ok (Db.Limited limited) ) ->
            ( model, respond resolver (tooManyRequests limited.retryAfter) )

        DatabaseChecked resolver ( latency, Ok (Db.Allowed _) ) ->
            if latency > slowAfter then
                ( model
                , Batch
                    [ respond resolver
                        (health model
                            { status = Api.Warn
                            , latencyMs = Just latency
                            , output = Just ("Slower than " ++ String.fromInt slowAfter ++ " ms.")
                            }
                        )
                    , log Interop.Warn ("Health check slow: the database took " ++ String.fromInt latency ++ " ms.")
                    ]
                )

            else
                ( model
                , respond resolver
                    (health model { status = Api.Pass, latencyMs = Just latency, output = Nothing })
                )

        DatabaseChecked resolver ( latency, Err problem ) ->
            ( model
            , Batch
                [ respond resolver
                    (health model { status = Api.Fail, latencyMs = Just latency, output = Just (failure problem) })
                , log Interop.Error ("Health check failed: " ++ Neon.errorToString problem)
                ]
            )

        DatabaseFailed resolver step problem ->
            ( model
            , Batch
                [ respond resolver unavailable
                , log Interop.Error (step ++ ": " ++ Neon.errorToString problem)
                ]
            )

        LinkLimited resolver ask ( byAddress, byEmail ) ->
            ( model
            , case ( retryAfter [ byAddress, byEmail ], model.db ) of
                ( Just seconds, _ ) ->
                    respond resolver (tooManyRequests seconds)

                ( Nothing, Ok db ) ->
                    case ask of
                        SignUpAsked signUp ->
                            query db
                                resolver
                                "Magic link failed at its link"
                                (Db.createSignUpLink
                                    { email = signUp.email
                                    , username = signUp.username
                                    , ttlSeconds = linkMinutes * 60
                                    }
                                )
                                (SignUpLinkCreated resolver signUp)

                        SignInAsked email ->
                            query db
                                resolver
                                "Magic link failed at its link"
                                (Db.createSignInLink { email = email, ttlSeconds = linkMinutes * 60 })
                                (SignInLinkCreated resolver email)

                ( Nothing, Err _ ) ->
                    respond resolver unavailable
            )

        SignUpLinkCreated resolver signUp link ->
            ( model
            , case link of
                Db.UsernameTaken ->
                    respond resolver (error 409 "That username is taken.")

                Db.SignUpLink token ->
                    sendLink model
                        resolver
                        { to = signUp.email, tag = "sign-up" }
                        (Services.Mailer.SignUp.email
                            { url = verifyUrl model token
                            , username = signUp.username
                            , expiresInMinutes = linkMinutes
                            }
                        )

                Db.SignInInstead token ->
                    sendLink model resolver { to = signUp.email, tag = "sign-in" } (signInEmail model token)
            )

        SignInLinkCreated resolver email link ->
            ( model
            , case link of
                Db.NoAccount ->
                    respond resolver accepted

                Db.SignInLink token ->
                    sendLink model resolver { to = email, tag = "sign-in" } (signInEmail model token)
            )

        LinkSent resolver (Ok _) ->
            ( model, respond resolver accepted )

        LinkSent resolver (Err problem) ->
            ( model
            , Batch
                [ respond resolver (error 502 "The email could not be sent. Try again.")
                , log Interop.Error ("Magic link failed at its email: " ++ Brevo.errorToString problem)
                ]
            )

        SessionFound resolver (Just account) ->
            ( model, respond resolver (json 200 (Api.encodeResponse Api.session account)) )

        SessionFound resolver Nothing ->
            ( model, respond resolver notSignedIn )

        SessionEnded resolver ->
            ( model, respond resolver (signedOut model) )

        VerificationFound resolver (Just found) ->
            ( model, respond resolver (json 200 (Api.encodeResponse Api.verification found)) )

        VerificationFound resolver Nothing ->
            ( model, respond resolver invalidLink )

        VerificationConsumed resolver (Db.Verified { sessionToken, account }) ->
            let
                response =
                    json 200 (Api.encodeResponse Api.verify account)
            in
            ( model
            , respond resolver
                { response | headers = ( "set-cookie", sessionCookie model sessionToken ) :: response.headers }
            )

        VerificationConsumed resolver Db.InvalidLink ->
            ( model, respond resolver invalidLink )

        VerificationConsumed resolver Db.UsernameTakenMeanwhile ->
            ( model, respond resolver (error 409 "That username was taken in the meantime. Sign up again with another.") )

        PasskeysFound resolver (Just found) ->
            ( model, respond resolver (json 200 (Api.encodeResponse Api.passkeys found)) )

        PasskeysFound resolver Nothing ->
            ( model, respond resolver notSignedIn )

        PasskeyStarted resolver (Just ceremony) ->
            ( model
            , respond resolver
                (json 200
                    (Api.encodeResponse Api.passkeyOptions
                        { challenge = ceremony.challenge
                        , rp = { id = Origin.rpId model.origin, name = "pixo" }
                        , user =
                            { id = UserId.toString ceremony.userId
                            , name = Email.toString ceremony.account.email
                            , displayName = Username.toString ceremony.account.username
                            }
                        , algorithms = WebAuthn.algorithms
                        , excludeCredentials = ceremony.passkeys
                        }
                    )
                )
            )

        PasskeyStarted resolver Nothing ->
            ( model, respond resolver notSignedIn )

        PasskeyAdded resolver (Just (Db.Added passkey account)) ->
            ( model
            , notify model
                resolver
                (json 201 (Api.encodeResponse Api.addPasskey passkey))
                { to = account.email, tag = "passkey-added" }
                (Services.Mailer.PasskeyAdded.email
                    { url = dashboardUrl model
                    , username = account.username
                    , passkey = passkey.name
                    }
                )
            )

        PasskeyAdded resolver (Just Db.NotAdded) ->
            ( model, respond resolver (error 409 "This passkey request expired or was used already. Try again.") )

        PasskeyAdded resolver Nothing ->
            ( model, respond resolver notSignedIn )

        PasskeyRemoved resolver id (Just (Db.Removed { name, account })) ->
            ( model
            , notify model
                resolver
                (json 200 (Api.encodeResponse (Api.deletePasskey id) ()))
                { to = account.email, tag = "passkey-removed" }
                (Services.Mailer.PasskeyRemoved.email
                    { url = dashboardUrl model
                    , username = account.username
                    , passkey = name
                    }
                )
            )

        PasskeyRemoved resolver _ (Just Db.NotRemoved) ->
            ( model, respond resolver (error 404 "This account has no such passkey.") )

        PasskeyRemoved resolver _ Nothing ->
            ( model, respond resolver notSignedIn )

        Notified resolver response (Ok _) ->
            ( model, respond resolver response )

        Notified resolver response (Err problem) ->
            ( model
            , Batch
                [ respond resolver response
                , log Interop.Error ("Passkey notice failed at its email: " ++ Brevo.errorToString problem)
                ]
            )

        SignInStarted resolver (Db.SignInChallenge challenge) ->
            ( model
            , respond resolver
                (json 200 (Api.encodeResponse Api.passkeySignInOptions { challenge = challenge, rpId = Origin.rpId model.origin }))
            )

        SignInStarted resolver (Db.TooManyChallenges limited) ->
            ( model, respond resolver (tooManyRequests limited.retryAfter) )

        KeyFound resolver assertion (Just publicKey) ->
            ( model
            , case
                WebAuthn.key publicKey
                    |> Result.andThen
                        (\key ->
                            WebAuthn.signature key assertion.signature
                                |> Result.map (Tuple.pair key)
                        )
              of
                Ok ( key, signature ) ->
                    SendToJs
                        (Interop.VerifySignature resolver
                            { key = key
                            , signed = assertion.signed
                            , signature = signature
                            , signIn = assertion.signIn
                            }
                        )

                Err problem ->
                    refuse resolver problem
            )

        KeyFound resolver _ Nothing ->
            ( model, refuse resolver "No passkey has this credential id." )

        GotInterop (Ok (Interop.SignatureChecked resolver signIn valid)) ->
            ( model
            , case ( valid, model.db ) of
                ( True, Ok db ) ->
                    query db
                        resolver
                        "Passkey sign-in failed at its session"
                        (Db.signInWithPasskey
                            { challenge = signIn.challenge
                            , credentialId = signIn.credentialId
                            , signCount = signIn.signCount
                            , backedUp = signIn.backedUp
                            , sessionSeconds = sessionSeconds
                            }
                        )
                        (PasskeySignedIn resolver)

                ( True, Err _ ) ->
                    respond resolver unavailable

                ( False, _ ) ->
                    refuse resolver "The signature does not match the passkey."
            )

        PasskeySignedIn resolver (Db.PasskeySignedIn { sessionToken, account }) ->
            let
                response =
                    json 200 (Api.encodeResponse Api.passkeySignIn account)
            in
            ( model
            , respond resolver
                { response | headers = ( "set-cookie", sessionCookie model sessionToken ) :: response.headers }
            )

        PasskeySignedIn resolver Db.ChallengeSpent ->
            ( model, refuse resolver "The challenge is expired, used or unknown." )

        PasskeySignedIn resolver Db.PasskeyStale ->
            ( model, refuse resolver "The passkey is gone, or its counter did not move forward." )

        GotInterop (Err _) ->
            ( model, NoEffect )


sendLink : Model -> Interop.Resolver -> { to : Email, tag : String } -> Layout.Message -> Effect
sendLink model resolver envelope email =
    case model.email of
        Ok mailer ->
            Mail (Mailer.send mailer envelope email (LinkSent resolver))

        Err _ ->
            respond resolver unavailable


signInEmail : Model -> LinkToken -> Layout.Message
signInEmail model token =
    Services.Mailer.SignIn.email
        { url = verifyUrl model token
        , expiresInMinutes = linkMinutes
        }


query : Neon.Connection -> Interop.Resolver -> String -> Neon.Query a -> (a -> Msg) -> Effect
query db resolver step wanted toMsg =
    Database (Neon.run db wanted (\result _ -> answered resolver step toMsg result))


transact : Neon.Connection -> Interop.Resolver -> String -> Neon.Transaction a -> (a -> Msg) -> Effect
transact db resolver step wanted toMsg =
    Database (Neon.commit db wanted (\result _ -> answered resolver step toMsg result))


answered : Interop.Resolver -> String -> (a -> Msg) -> Result Neon.Error a -> Msg
answered resolver step toMsg result =
    case result of
        Ok value ->
            toMsg value

        Err problem ->
            DatabaseFailed resolver step problem


notify : Model -> Interop.Resolver -> Response -> { to : Email, tag : String } -> Layout.Message -> Effect
notify model resolver response envelope email =
    case model.email of
        Ok mailer ->
            Mail (Mailer.send mailer envelope email (Notified resolver response))

        Err _ ->
            respond resolver response


refuse : Interop.Resolver -> String -> Effect
refuse resolver problem =
    Batch
        [ respond resolver (error 401 "This passkey could not be verified. Try again, or sign in with an emailed link.")
        , log Interop.Warn ("Passkey sign-in refused: " ++ String.left 200 problem)
        ]



-- EFFECTS


type Effect
    = NoEffect
    | Batch (List Effect)
    | SendToJs Interop.FromElm
    | Database (Neon.Request (Int -> Msg))
    | Mail (Brevo.Request Msg)


perform : Effect -> Cmd Msg
perform effect =
    case effect of
        NoEffect ->
            Cmd.none

        Batch effects ->
            Cmd.batch (List.map perform effects)

        SendToJs value ->
            InteropPorts.fromElm value

        Database request ->
            Time.now
                |> Task.andThen
                    (\start ->
                        Neon.send request
                            |> Task.andThen
                                (\toMsg ->
                                    Time.now
                                        |> Task.map (\end -> toMsg (Time.posixToMillis end - Time.posixToMillis start))
                                )
                    )
                |> Task.perform identity

        Mail request ->
            Brevo.send request
                |> Task.perform identity



-- ROUTES


route : Model -> Request -> Reply
route model request =
    case Api.parse request.method request.path of
        Ok r ->
            handle model request r

        Err Api.NotFound ->
            Now (error 404 "Not found.")

        Err (Api.MethodNotAllowed allowed) ->
            let
                response =
                    error 405 "Method not allowed."
            in
            Now
                { response
                    | headers =
                        ( "allow", String.join ", " (List.map Api.methodToString allowed) )
                            :: response.headers
                }


handle : Model -> Request -> Api.Route -> Reply
handle model request r =
    case r of
        Api.GetHealth ->
            case model.db of
                Ok db ->
                    Later
                        (\resolver ->
                            Database
                                (Neon.commit (Neon.withTimeout healthTimeout db)
                                    (Neon.transaction always
                                        |> Neon.with
                                            (Db.rateLimit
                                                { key = "health:ip:" ++ clientAddress request
                                                , limit = 60
                                                , windowSeconds = 60
                                                }
                                            )
                                        |> Neon.with Db.ping
                                    )
                                    (\result latency -> DatabaseChecked resolver ( latency, result ))
                                )
                        )

                Err problem ->
                    Now (health model { status = Api.Fail, latencyMs = Nothing, output = Just ("Not configured: " ++ problem) })

        Api.PostSignUp ->
            askForLink model
                request
                (Decode.decodeString (Api.bodyDecoder Api.signUp) request.body
                    |> Result.mapError (\_ -> error 400 "Expected a JSON object with an email and a username.")
                    |> Result.andThen (SignUp.parse >> Result.mapError (error 422))
                    |> Result.map SignUpAsked
                )

        Api.PostSignIn ->
            askForLink model
                request
                (Decode.decodeString (Api.bodyDecoder Api.signIn) request.body
                    |> Result.mapError (\_ -> error 400 "Expected a JSON object with an email.")
                    |> Result.andThen (.email >> Email.parse >> Result.mapError (error 422))
                    |> Result.map SignInAsked
                )

        Api.GetSession ->
            withSession model
                request
                (\db token resolver ->
                    transact db resolver "Session lookup failed" (Db.account token) (SessionFound resolver)
                )

        Api.DeleteSession ->
            case ( model.db, cookieToken request ) of
                ( Ok db, Just token ) ->
                    Later
                        (\resolver ->
                            query db resolver "Sign-out failed" (Db.signOut token) (\() -> SessionEnded resolver)
                        )

                ( Ok _, Nothing ) ->
                    Now (signedOut model)

                ( Err _, _ ) ->
                    Now unavailable

        Api.GetVerification ->
            withLink model
                request
                (\db token resolver ->
                    query db resolver "Verification failed at its link" (Db.magicLink token) (VerificationFound resolver)
                )

        Api.PostVerification ->
            withLink model
                request
                (\db token resolver ->
                    query db
                        resolver
                        "Verification failed at its session"
                        (Db.consumeMagicLink { token = token, sessionSeconds = sessionSeconds })
                        (VerificationConsumed resolver)
                )

        Api.GetPasskeys ->
            withSession model
                request
                (\db token resolver ->
                    transact db resolver "Passkey list failed" (Db.passkeys token) (PasskeysFound resolver)
                )

        Api.PostPasskeyOptions ->
            withSession model
                request
                (\db token resolver ->
                    transact db
                        resolver
                        "Passkey failed at its challenge"
                        (Db.startPasskey { token = token, ttlSeconds = challengeSeconds })
                        (PasskeyStarted resolver)
                )

        Api.PostPasskey ->
            withSession model
                request
                (\db token resolver ->
                    case
                        Decode.decodeString (Api.bodyDecoder Api.addPasskey) request.body
                            |> Result.mapError (\_ -> Nothing)
                            |> Result.andThen
                                (WebAuthn.register model.origin
                                    >> Result.mapError Just
                                )
                    of
                        Ok registration ->
                            transact db
                                resolver
                                "Passkey failed at its credential"
                                (Db.addPasskey
                                    { token = token
                                    , registration = registration
                                    , name = Aaguid.name registration.aaguid
                                    }
                                )
                                (PasskeyAdded resolver)

                        Err Nothing ->
                            respond resolver
                                (error 400 "Expected a JSON object with a clientDataJSON, an attestationObject and transports.")

                        Err (Just problem) ->
                            Batch
                                [ respond resolver (error 422 "This passkey could not be verified. Try again.")
                                , log Interop.Warn ("Passkey refused: " ++ String.left 200 problem)
                                ]
                )

        Api.DeletePasskey id ->
            withSession model
                request
                (\db token resolver ->
                    transact db
                        resolver
                        "Passkey removal failed"
                        (Db.removePasskey { token = token, id = id })
                        (PasskeyRemoved resolver id)
                )

        Api.PostPasskeySignInOptions ->
            case model.db of
                Ok db ->
                    Later
                        (\resolver ->
                            query db
                                resolver
                                "Passkey sign-in failed at its challenge"
                                (Db.signInChallenge
                                    { key = "passkey:ip:" ++ clientAddress request
                                    , limit = 20
                                    , windowSeconds = 900
                                    , ttlSeconds = challengeSeconds
                                    }
                                )
                                (SignInStarted resolver)
                        )

                Err _ ->
                    Now unavailable

        Api.PostPasskeySignIn ->
            case ( model.db, Decode.decodeString (Api.bodyDecoder Api.passkeySignIn) request.body ) of
                ( Ok db, Ok assertion ) ->
                    Later
                        (\resolver ->
                            case ( WebAuthn.authenticate model.origin assertion, CredentialId.fromString assertion.id ) of
                                ( Ok authentication, Just credentialId ) ->
                                    query db
                                        resolver
                                        "Passkey sign-in failed at its key"
                                        (Db.passkeyPublicKey credentialId)
                                        (KeyFound resolver
                                            { signed = authentication.signed
                                            , signature = assertion.signature
                                            , signIn =
                                                { challenge = authentication.challenge
                                                , credentialId = credentialId
                                                , signCount = authentication.signCount
                                                , backedUp = authentication.backedUp
                                                }
                                            }
                                        )

                                ( Err problem, _ ) ->
                                    refuse resolver problem

                                ( Ok _, Nothing ) ->
                                    refuse resolver "The assertion names no credential."
                        )

                ( Ok _, Err _ ) ->
                    Now (error 400 "Expected a JSON object with an id, a clientDataJSON, an authenticatorData and a signature.")

                ( Err _, _ ) ->
                    Now unavailable


askForLink : Model -> Request -> Result Response Ask -> Reply
askForLink model request parsed =
    case ( model.db, model.email, parsed ) of
        ( Ok db, Ok _, Ok ask ) ->
            Later
                (\resolver ->
                    transact db
                        resolver
                        "Magic link failed at its rate limits"
                        (Neon.transaction Tuple.pair
                            |> Neon.with
                                (Db.rateLimit
                                    { key = "magic-link:ip:" ++ clientAddress request
                                    , limit = 10
                                    , windowSeconds = 900
                                    }
                                )
                            |> Neon.with
                                (Db.rateLimit
                                    { key = "magic-link:email:" ++ Email.toString (askedBy ask)
                                    , limit = 3
                                    , windowSeconds = 900
                                    }
                                )
                        )
                        (LinkLimited resolver ask)
                )

        ( Ok _, Ok _, Err response ) ->
            Now response

        _ ->
            Now unavailable


withSession : Model -> Request -> (Neon.Connection -> SessionToken -> Interop.Resolver -> Effect) -> Reply
withSession model request toEffect =
    case ( model.db, cookieToken request ) of
        ( Ok db, Just token ) ->
            Later (toEffect db token)

        ( Ok _, Nothing ) ->
            Now notSignedIn

        ( Err _, _ ) ->
            Now unavailable


cookieToken : Request -> Maybe SessionToken
cookieToken request =
    Dict.get "cookie" request.headers
        |> Maybe.withDefault ""
        |> String.split ";"
        |> List.map String.trim
        |> List.filter (String.startsWith "session=")
        |> List.head
        |> Maybe.map (String.dropLeft 8)
        |> Maybe.andThen SessionToken.fromString


withLink : Model -> Request -> (Neon.Connection -> LinkToken -> Interop.Resolver -> Effect) -> Reply
withLink model request toEffect =
    case ( model.db, bearer request ) of
        ( Ok db, Just token ) ->
            Later (toEffect db token)

        ( Ok _, Nothing ) ->
            let
                response =
                    error 401 "Send the link's token as a bearer token."
            in
            Now { response | headers = ( "www-authenticate", "Bearer" ) :: response.headers }

        ( Err _, _ ) ->
            Now unavailable


bearer : Request -> Maybe LinkToken
bearer request =
    Dict.get "authorization" request.headers
        |> Maybe.andThen
            (\value ->
                if String.toLower (String.left 7 value) == "bearer " then
                    Just (String.trim (String.dropLeft 7 value))

                else
                    Nothing
            )
        |> Maybe.andThen LinkToken.fromString


verifyUrl : Model -> LinkToken -> String
verifyUrl model token =
    Origin.toString model.origin ++ Path.toString (Path.Verify (Just token))


dashboardUrl : Model -> String
dashboardUrl model =
    Origin.toString model.origin ++ Path.toString Path.Dashboard


sessionSeconds : Int
sessionSeconds =
    30 * 24 * 60 * 60


sessionCookie : Model -> SessionToken -> String
sessionCookie model token =
    cookie model (SessionToken.toString token) sessionSeconds


cookie : Model -> String -> Int -> String
cookie model token maxAge =
    String.join "; "
        (List.concat
            [ [ "session=" ++ token
              , "Path=/"
              , "Max-Age=" ++ String.fromInt maxAge
              , "HttpOnly"
              , "SameSite=Lax"
              ]
            , if Origin.isSecure model.origin then
                [ "Secure" ]

              else
                []
            ]
        )


linkMinutes : Int
linkMinutes =
    15


challengeSeconds : Int
challengeSeconds =
    300


clientAddress : Request -> String
clientAddress request =
    case Dict.get "x-real-ip" request.headers of
        Just address ->
            String.trim address

        Nothing ->
            Dict.get "x-forwarded-for" request.headers
                |> Maybe.andThen (String.split "," >> List.head)
                |> Maybe.map String.trim
                |> Maybe.withDefault "unknown"


retryAfter : List Db.RateLimit -> Maybe Int
retryAfter limits =
    limits
        |> List.filterMap
            (\limit ->
                case limit of
                    Db.Allowed _ ->
                        Nothing

                    Db.Limited limited ->
                        Just limited.retryAfter
            )
        |> List.maximum


healthTimeout : Float
healthTimeout =
    5000


slowAfter : Int
slowAfter =
    1000


failure : Neon.Error -> String
failure problem =
    case problem of
        Neon.Timeout ->
            "Timed out after " ++ String.fromFloat healthTimeout ++ " ms."

        Neon.NetworkError ->
            "Unreachable."

        Neon.BadUrl _ ->
            "Unreachable."

        Neon.ServerError status _ ->
            "Neon answered with HTTP " ++ String.fromInt status ++ "."

        Neon.QueryFailed _ ->
            "The query failed."

        Neon.UnexpectedResult _ ->
            "Unexpected result."



-- RESPONSES


json : Int -> Encode.Value -> Response
json status value =
    { status = status
    , headers = [ ( "content-type", "application/json" ), ( "cache-control", "no-store" ) ]
    , body = Encode.encode 0 value
    }


health : Model -> Api.Check -> Response
health model check =
    let
        report =
            Api.healthOf model.release check
    in
    { status =
        case report.status of
            Api.Pass ->
                200

            Api.Warn ->
                200

            Api.Fail ->
                503
    , headers = [ ( "content-type", "application/health+json" ), ( "cache-control", "no-store" ) ]
    , body = Encode.encode 0 (Api.encodeResponse Api.health report)
    }


unavailable : Response
unavailable =
    error 503 "Not available right now. Try again later."


accepted : Response
accepted =
    json 202 (Api.encodeResponse Api.signUp ())


notSignedIn : Response
notSignedIn =
    error 401 "Not signed in."


signedOut : Model -> Response
signedOut model =
    let
        response =
            json 200 (Api.encodeResponse Api.signOut ())
    in
    { response | headers = ( "set-cookie", cookie model "" 0 ) :: response.headers }


invalidLink : Response
invalidLink =
    error 404 "This link is not valid any more. Sign up again to get a new one."


tooManyRequests : Int -> Response
tooManyRequests seconds =
    let
        response =
            error 429 "Too many attempts. Try again later."
    in
    { response | headers = ( "retry-after", String.fromInt seconds ) :: response.headers }


error : Int -> String -> Response
error status message =
    json status (Api.encodeError message)
