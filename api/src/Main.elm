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


type Model
    = Running Server
    | Broken { problem : String, release : Api.Release }


type alias Server =
    { db : Neon.Connection
    , mailer : Result String Mailer.Mailer
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

                mailer =
                    Mailer.configure config

                noMailer =
                    case mailer of
                        Ok _ ->
                            NoEffect

                        Err problem ->
                            log Interop.Warn ("No email, so nothing is sent: " ++ problem)
            in
            case ( Origin.parse config.origin, Db.connect config ) of
                ( Ok origin, Ok db ) ->
                    ( Running { db = db, mailer = mailer, origin = origin, release = release }
                    , Batch [ log Interop.Info ("Sending queries to " ++ Neon.endpoint db), noMailer ]
                    )

                ( Ok _, Err problem ) ->
                    ( Broken { problem = problem, release = release }
                    , Batch [ log Interop.Warn ("No database, so database routes answer 503: " ++ problem), noMailer ]
                    )

                ( Err problem, _ ) ->
                    ( Broken { problem = problem, release = release }
                    , log Interop.Error ("Bad origin: " ++ problem)
                    )

        Err problem ->
            ( Broken
                { problem = "The server's flags did not decode."
                , release = { releaseId = Nothing, releaseUrl = Nothing, region = Nothing }
                }
            , log Interop.Error ("Bad flags: " ++ Decode.errorToString problem)
            )


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
    | LinkLimited Interop.Resolver Mailer.Mailer Ask ( Db.RateLimit, Db.RateLimit )
    | SignUpLinkCreated Interop.Resolver Mailer.Mailer SignUp.SignUp Db.SignUpLink
    | SignInLinkCreated Interop.Resolver Mailer.Mailer Email Db.SignInLink
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
    ( model
    , case model of
        Running server ->
            running server msg

        Broken broken ->
            whenBroken broken msg
    )


whenBroken : { problem : String, release : Api.Release } -> Msg -> Effect
whenBroken broken msg =
    case msg of
        GotInterop (Ok (Interop.GotRequest resolver request)) ->
            respond resolver
                (case Api.parse request.method request.path of
                    Ok found ->
                        brokenAnswer broken found

                    Err refused ->
                        notRouted refused
                )

        _ ->
            NoEffect


brokenAnswer : { problem : String, release : Api.Release } -> Api.Route -> Response
brokenAnswer { problem, release } found =
    case found of
        Api.GetHealth ->
            health release { status = Api.Fail, latencyMs = Nothing, output = Just ("Not configured: " ++ problem) }

        Api.PostSignUp ->
            unavailable

        Api.PostSignIn ->
            unavailable

        Api.GetSession ->
            unavailable

        Api.DeleteSession ->
            unavailable

        Api.GetVerification ->
            unavailable

        Api.PostVerification ->
            unavailable

        Api.GetPasskeys ->
            unavailable

        Api.PostPasskey ->
            unavailable

        Api.PostPasskeyOptions ->
            unavailable

        Api.DeletePasskey _ ->
            unavailable

        Api.PostPasskeySignIn ->
            unavailable

        Api.PostPasskeySignInOptions ->
            unavailable


reply : Interop.Resolver -> Reply -> Effect
reply resolver answer =
    case answer of
        Now response ->
            respond resolver response

        Later toEffect ->
            toEffect resolver


running : Server -> Msg -> Effect
running server msg =
    case msg of
        GotInterop (Ok (Interop.GotRequest resolver request)) ->
            reply resolver (route server request)

        DatabaseChecked resolver ( _, Ok (Db.Limited limited) ) ->
            respond resolver (tooManyRequests limited.retryAfter)

        DatabaseChecked resolver ( latency, Ok (Db.Allowed _) ) ->
            if latency > slowAfter then
                Batch
                    [ respond resolver
                        (health server.release
                            { status = Api.Warn
                            , latencyMs = Just latency
                            , output = Just ("Slower than " ++ String.fromInt slowAfter ++ " ms.")
                            }
                        )
                    , log Interop.Warn ("Health check slow: the database took " ++ String.fromInt latency ++ " ms.")
                    ]

            else
                respond resolver
                    (health server.release { status = Api.Pass, latencyMs = Just latency, output = Nothing })

        DatabaseChecked resolver ( latency, Err problem ) ->
            Batch
                [ respond resolver
                    (health server.release { status = Api.Fail, latencyMs = Just latency, output = Just (failure problem) })
                , log Interop.Error ("Health check failed: " ++ Neon.errorToString problem)
                ]

        DatabaseFailed resolver step problem ->
            Batch
                [ respond resolver unavailable
                , log Interop.Error (step ++ ": " ++ Neon.errorToString problem)
                ]

        LinkLimited resolver mailer ask ( byAddress, byEmail ) ->
            case retryAfter [ byAddress, byEmail ] of
                Just seconds ->
                    respond resolver (tooManyRequests seconds)

                Nothing ->
                    case ask of
                        SignUpAsked signUp ->
                            query server.db
                                resolver
                                "Magic link failed at its link"
                                (Db.createSignUpLink
                                    { email = signUp.email
                                    , username = signUp.username
                                    , ttlSeconds = linkMinutes * 60
                                    }
                                )
                                (SignUpLinkCreated resolver mailer signUp)

                        SignInAsked email ->
                            query server.db
                                resolver
                                "Magic link failed at its link"
                                (Db.createSignInLink { email = email, ttlSeconds = linkMinutes * 60 })
                                (SignInLinkCreated resolver mailer email)

        SignUpLinkCreated resolver mailer signUp link ->
            case link of
                Db.UsernameTaken ->
                    respond resolver (error 409 "That username is taken.")

                Db.SignUpLink token ->
                    sendLink mailer
                        resolver
                        { to = signUp.email, tag = "sign-up" }
                        (Services.Mailer.SignUp.email
                            { url = verifyUrl server token
                            , username = signUp.username
                            , expiresInMinutes = linkMinutes
                            }
                        )

                Db.SignInInstead token ->
                    sendLink mailer resolver { to = signUp.email, tag = "sign-in" } (signInEmail server token)

        SignInLinkCreated resolver mailer email link ->
            case link of
                Db.NoAccount ->
                    respond resolver accepted

                Db.SignInLink token ->
                    sendLink mailer resolver { to = email, tag = "sign-in" } (signInEmail server token)

        LinkSent resolver (Ok _) ->
            respond resolver accepted

        LinkSent resolver (Err problem) ->
            Batch
                [ respond resolver (error 502 "The email could not be sent. Try again.")
                , log Interop.Error ("Magic link failed at its email: " ++ Brevo.errorToString problem)
                ]

        SessionFound resolver (Just account) ->
            respond resolver (json 200 (Api.encodeResponse Api.session account))

        SessionFound resolver Nothing ->
            respond resolver notSignedIn

        SessionEnded resolver ->
            respond resolver (signedOut server)

        VerificationFound resolver (Just found) ->
            respond resolver (json 200 (Api.encodeResponse Api.verification found))

        VerificationFound resolver Nothing ->
            respond resolver invalidLink

        VerificationConsumed resolver (Db.Verified { sessionToken, account }) ->
            let
                response =
                    json 200 (Api.encodeResponse Api.verify account)
            in
            respond resolver
                { response | headers = ( "set-cookie", sessionCookie server sessionToken ) :: response.headers }

        VerificationConsumed resolver Db.InvalidLink ->
            respond resolver invalidLink

        VerificationConsumed resolver Db.UsernameTakenMeanwhile ->
            respond resolver (error 409 "That username was taken in the meantime. Sign up again with another.")

        PasskeysFound resolver (Just found) ->
            respond resolver (json 200 (Api.encodeResponse Api.passkeys found))

        PasskeysFound resolver Nothing ->
            respond resolver notSignedIn

        PasskeyStarted resolver (Just ceremony) ->
            respond resolver
                (json 200
                    (Api.encodeResponse Api.passkeyOptions
                        { challenge = ceremony.challenge
                        , rp = { id = Origin.rpId server.origin, name = "pixo" }
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

        PasskeyStarted resolver Nothing ->
            respond resolver notSignedIn

        PasskeyAdded resolver (Just (Db.Added passkey account)) ->
            notify server
                resolver
                (json 201 (Api.encodeResponse Api.addPasskey passkey))
                { to = account.email, tag = "passkey-added" }
                (Services.Mailer.PasskeyAdded.email
                    { url = dashboardUrl server
                    , username = account.username
                    , passkey = passkey.name
                    }
                )

        PasskeyAdded resolver (Just Db.NotAdded) ->
            respond resolver (error 409 "This passkey request expired or was used already. Try again.")

        PasskeyAdded resolver Nothing ->
            respond resolver notSignedIn

        PasskeyRemoved resolver id (Just (Db.Removed { name, account })) ->
            notify server
                resolver
                (json 200 (Api.encodeResponse (Api.deletePasskey id) ()))
                { to = account.email, tag = "passkey-removed" }
                (Services.Mailer.PasskeyRemoved.email
                    { url = dashboardUrl server
                    , username = account.username
                    , passkey = name
                    }
                )

        PasskeyRemoved resolver _ (Just Db.NotRemoved) ->
            respond resolver (error 404 "This account has no such passkey.")

        PasskeyRemoved resolver _ Nothing ->
            respond resolver notSignedIn

        Notified resolver response (Ok _) ->
            respond resolver response

        Notified resolver response (Err problem) ->
            Batch
                [ respond resolver response
                , log Interop.Error ("Passkey notice failed at its email: " ++ Brevo.errorToString problem)
                ]

        SignInStarted resolver (Db.SignInChallenge challenge) ->
            respond resolver
                (json 200 (Api.encodeResponse Api.passkeySignInOptions { challenge = challenge, rpId = Origin.rpId server.origin }))

        SignInStarted resolver (Db.TooManyChallenges limited) ->
            respond resolver (tooManyRequests limited.retryAfter)

        KeyFound resolver assertion (Just publicKey) ->
            case
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

        KeyFound resolver _ Nothing ->
            refuse resolver "No passkey has this credential id."

        GotInterop (Ok (Interop.SignatureChecked resolver signIn valid)) ->
            if valid then
                query server.db
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

            else
                refuse resolver "The signature does not match the passkey."

        PasskeySignedIn resolver (Db.PasskeySignedIn { sessionToken, account }) ->
            let
                response =
                    json 200 (Api.encodeResponse Api.passkeySignIn account)
            in
            respond resolver
                { response | headers = ( "set-cookie", sessionCookie server sessionToken ) :: response.headers }

        PasskeySignedIn resolver Db.ChallengeSpent ->
            refuse resolver "The challenge is expired, used or unknown."

        PasskeySignedIn resolver Db.PasskeyStale ->
            refuse resolver "The passkey is gone, or its counter did not move forward."

        GotInterop (Err _) ->
            NoEffect


sendLink : Mailer.Mailer -> Interop.Resolver -> { to : Email, tag : String } -> Layout.Message -> Effect
sendLink mailer resolver envelope message =
    Mail (Mailer.send mailer envelope message (LinkSent resolver))


signInEmail : Server -> LinkToken -> Layout.Message
signInEmail server token =
    Services.Mailer.SignIn.email
        { url = verifyUrl server token
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


notify : Server -> Interop.Resolver -> Response -> { to : Email, tag : String } -> Layout.Message -> Effect
notify server resolver response envelope email =
    case server.mailer of
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


route : Server -> Request -> Reply
route server request =
    case Api.parse request.method request.path of
        Ok found ->
            handle server request found

        Err refused ->
            Now (notRouted refused)


notRouted : Api.Problem -> Response
notRouted refused =
    case refused of
        Api.NotFound ->
            error 404 "Not found."

        Api.MethodNotAllowed allowed ->
            let
                response =
                    error 405 "Method not allowed."
            in
            { response
                | headers =
                    ( "allow", String.join ", " (List.map Api.methodToString allowed) )
                        :: response.headers
            }


handle : Server -> Request -> Api.Route -> Reply
handle server request r =
    case r of
        Api.GetHealth ->
            Later
                (\resolver ->
                    Database
                        (Neon.commit (Neon.withTimeout healthTimeout server.db)
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

        Api.PostSignUp ->
            askForLink server
                request
                (Decode.decodeString (Api.bodyDecoder Api.signUp) request.body
                    |> Result.mapError (\_ -> error 400 "Expected a JSON object with an email and a username.")
                    |> Result.andThen (SignUp.parse >> Result.mapError (error 422))
                    |> Result.map SignUpAsked
                )

        Api.PostSignIn ->
            askForLink server
                request
                (Decode.decodeString (Api.bodyDecoder Api.signIn) request.body
                    |> Result.mapError (\_ -> error 400 "Expected a JSON object with an email.")
                    |> Result.andThen (.email >> Email.parse >> Result.mapError (error 422))
                    |> Result.map SignInAsked
                )

        Api.GetSession ->
            withSession
                request
                (\token resolver ->
                    transact server.db resolver "Session lookup failed" (Db.account token) (SessionFound resolver)
                )

        Api.DeleteSession ->
            case cookieToken request of
                Just token ->
                    Later
                        (\resolver ->
                            query server.db resolver "Sign-out failed" (Db.signOut token) (\() -> SessionEnded resolver)
                        )

                Nothing ->
                    Now (signedOut server)

        Api.GetVerification ->
            withLink
                request
                (\token resolver ->
                    query server.db resolver "Verification failed at its link" (Db.magicLink token) (VerificationFound resolver)
                )

        Api.PostVerification ->
            withLink
                request
                (\token resolver ->
                    query server.db
                        resolver
                        "Verification failed at its session"
                        (Db.consumeMagicLink { token = token, sessionSeconds = sessionSeconds })
                        (VerificationConsumed resolver)
                )

        Api.GetPasskeys ->
            withSession
                request
                (\token resolver ->
                    transact server.db resolver "Passkey list failed" (Db.passkeys token) (PasskeysFound resolver)
                )

        Api.PostPasskeyOptions ->
            withSession
                request
                (\token resolver ->
                    transact server.db
                        resolver
                        "Passkey failed at its challenge"
                        (Db.startPasskey { token = token, ttlSeconds = challengeSeconds })
                        (PasskeyStarted resolver)
                )

        Api.PostPasskey ->
            withSession
                request
                (\token resolver ->
                    case
                        Decode.decodeString (Api.bodyDecoder Api.addPasskey) request.body
                            |> Result.mapError (\_ -> Nothing)
                            |> Result.andThen
                                (WebAuthn.register server.origin
                                    >> Result.mapError Just
                                )
                    of
                        Ok registration ->
                            transact server.db
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
            withSession
                request
                (\token resolver ->
                    transact server.db
                        resolver
                        "Passkey removal failed"
                        (Db.removePasskey { token = token, id = id })
                        (PasskeyRemoved resolver id)
                )

        Api.PostPasskeySignInOptions ->
            Later
                (\resolver ->
                    query server.db
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

        Api.PostPasskeySignIn ->
            case Decode.decodeString (Api.bodyDecoder Api.passkeySignIn) request.body of
                Ok assertion ->
                    Later
                        (\resolver ->
                            case ( WebAuthn.authenticate server.origin assertion, CredentialId.fromString assertion.id ) of
                                ( Ok authentication, Just credentialId ) ->
                                    query server.db
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

                Err _ ->
                    Now (error 400 "Expected a JSON object with an id, a clientDataJSON, an authenticatorData and a signature.")


askForLink : Server -> Request -> Result Response Ask -> Reply
askForLink server request parsed =
    case ( server.mailer, parsed ) of
        ( Ok mailer, Ok ask ) ->
            Later
                (\resolver ->
                    transact server.db
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
                        (LinkLimited resolver mailer ask)
                )

        ( Ok _, Err response ) ->
            Now response

        ( Err _, _ ) ->
            Now unavailable


withSession : Request -> (SessionToken -> Interop.Resolver -> Effect) -> Reply
withSession request toEffect =
    case cookieToken request of
        Just token ->
            Later (toEffect token)

        Nothing ->
            Now notSignedIn


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


withLink : Request -> (LinkToken -> Interop.Resolver -> Effect) -> Reply
withLink request toEffect =
    case bearer request of
        Just token ->
            Later (toEffect token)

        Nothing ->
            let
                response =
                    error 401 "Send the link's token as a bearer token."
            in
            Now { response | headers = ( "www-authenticate", "Bearer" ) :: response.headers }


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


verifyUrl : Server -> LinkToken -> String
verifyUrl server token =
    Origin.toString server.origin ++ Path.toString (Path.Verify (Just token))


dashboardUrl : Server -> String
dashboardUrl server =
    Origin.toString server.origin ++ Path.toString Path.Dashboard


sessionSeconds : Int
sessionSeconds =
    30 * 24 * 60 * 60


sessionCookie : Server -> SessionToken -> String
sessionCookie server token =
    cookie server (SessionToken.toString token) sessionSeconds


cookie : Server -> String -> Int -> String
cookie server token maxAge =
    String.join "; "
        (List.concat
            [ [ "session=" ++ token
              , "Path=/"
              , "Max-Age=" ++ String.fromInt maxAge
              , "HttpOnly"
              , "SameSite=Lax"
              ]
            , if Origin.isSecure server.origin then
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


health : Api.Release -> Api.Check -> Response
health release check =
    let
        report =
            Api.healthOf release check
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


signedOut : Server -> Response
signedOut server =
    let
        response =
            json 200 (Api.encodeResponse Api.signOut ())
    in
    { response | headers = ( "set-cookie", cookie server "" 0 ) :: response.headers }


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
