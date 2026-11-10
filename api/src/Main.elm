module Main exposing (Effect(..), Model, Msg(..), init, main, update)

import Aaguid
import Api
import Brevo
import Db
import Dict
import Email
import Email.Layout
import Email.PasskeyAdded
import Email.PasskeyRemoved
import Email.SignIn
import Email.SignUp
import InteropDefinitions as Interop exposing (Request, Response)
import InteropPorts
import Json.Decode as Decode
import Json.Encode as Encode
import Neon
import SignUp
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
    , email : Result String Email.Mailer
    , origin : String
    , release : Api.Release
    }


init : Decode.Value -> ( Model, Effect )
init flags =
    case InteropPorts.decodeFlags flags of
        Ok config ->
            let
                db =
                    Db.connect config

                email =
                    Email.configure config
            in
            ( { db = db
              , email = email
              , origin = config.origin
              , release = { releaseId = config.releaseId, releaseUrl = config.releaseUrl, region = config.region }
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
            ( { db = Err "The server's flags did not decode."
              , email = Err "The server's flags did not decode."
              , origin = ""
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
    | DatabaseChecked Interop.Resolver ( Int, Result Neon.Error () )
    | LinkLimited Interop.Resolver Ask (Result Neon.Error ( Db.RateLimit, Db.RateLimit ))
    | LinkCreated Interop.Resolver Ask (Result Neon.Error Db.Link)
    | LinkSent Interop.Resolver (Result Brevo.Error Brevo.Sent)
    | SessionFound Interop.Resolver (Result Neon.Error (Maybe Api.Account))
    | SessionEnded Interop.Resolver (Result Neon.Error ())
    | VerificationFound Interop.Resolver (Result Neon.Error (Maybe Api.Verification))
    | VerificationConsumed Interop.Resolver (Result Neon.Error Db.Consumed)
    | PasskeysFound Interop.Resolver (Result Neon.Error (Maybe (List Api.Passkey)))
    | PasskeyStarted Interop.Resolver (Result Neon.Error (Maybe Db.Ceremony))
    | PasskeyAdded Interop.Resolver (Result Neon.Error (Maybe Db.Added))
    | PasskeyRemoved Interop.Resolver (Result Neon.Error (Maybe Db.Removed))
    | Notified Interop.Resolver Response (Result Brevo.Error Brevo.Sent)
    | SignInStarted Interop.Resolver (Result Neon.Error Db.SignInChallenge)
    | KeyFound Interop.Resolver { signed : String, signature : String, signIn : Interop.SignIn } (Result Neon.Error (Maybe String))
    | PasskeySignedIn Interop.Resolver (Result Neon.Error Db.PasskeySignIn)


type Ask
    = SignUpAsked Api.SignUp
    | SignInAsked String


askedBy : Ask -> String
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
            let
                ( next, reply ) =
                    route model request
            in
            case reply of
                Now response ->
                    ( next, respond resolver response )

                Later toEffect ->
                    ( next, toEffect resolver )

        DatabaseChecked resolver ( latency, Ok () ) ->
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

        LinkLimited resolver ask (Ok ( byAddress, byEmail )) ->
            ( model
            , case ( retryAfter [ byAddress, byEmail ], model.db ) of
                ( Just seconds, _ ) ->
                    respond resolver (tooManyRequests seconds)

                ( Nothing, Ok db ) ->
                    Database
                        (Neon.run db
                            (case ask of
                                SignUpAsked signUp ->
                                    Db.createSignUpLink
                                        { email = signUp.email
                                        , username = signUp.username
                                        , ttlSeconds = linkMinutes * 60
                                        }

                                SignInAsked email ->
                                    Db.createSignInLink { email = email, ttlSeconds = linkMinutes * 60 }
                            )
                            (\result _ -> LinkCreated resolver ask result)
                        )

                ( Nothing, Err _ ) ->
                    respond resolver unavailable
            )

        LinkLimited resolver _ (Err problem) ->
            ( model
            , Batch
                [ respond resolver unavailable
                , log Interop.Error ("Magic link failed at its rate limits: " ++ Neon.errorToString problem)
                ]
            )

        LinkCreated resolver ask (Ok link) ->
            ( model
            , case ( link, ask, model.email ) of
                ( Db.UsernameTaken, _, _ ) ->
                    respond resolver (error 409 "That username is taken.")

                ( Db.NoAccount, _, _ ) ->
                    respond resolver accepted

                ( Db.SignUpLink token, SignUpAsked signUp, Ok mailer ) ->
                    Email
                        (Email.send mailer
                            { to = signUp.email, tag = "sign-up" }
                            (Email.SignUp.email
                                { url = verifyUrl model token
                                , username = signUp.username
                                , expiresInMinutes = linkMinutes
                                }
                            )
                            (LinkSent resolver)
                        )

                ( Db.SignUpLink _, SignInAsked _, _ ) ->
                    respond resolver unavailable

                ( Db.SignInLink token, _, Ok mailer ) ->
                    Email
                        (Email.send mailer
                            { to = askedBy ask, tag = "sign-in" }
                            (Email.SignIn.email
                                { url = verifyUrl model token
                                , expiresInMinutes = linkMinutes
                                }
                            )
                            (LinkSent resolver)
                        )

                ( _, _, Err _ ) ->
                    respond resolver unavailable
            )

        LinkCreated resolver _ (Err problem) ->
            ( model
            , Batch
                [ respond resolver unavailable
                , log Interop.Error ("Magic link failed at its link: " ++ Neon.errorToString problem)
                ]
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

        SessionFound resolver (Ok (Just account)) ->
            ( model, respond resolver (json 200 (Api.encodeResponse Api.session account)) )

        SessionFound resolver (Ok Nothing) ->
            ( model, respond resolver notSignedIn )

        SessionFound resolver (Err problem) ->
            ( model
            , Batch
                [ respond resolver unavailable
                , log Interop.Error ("Session lookup failed: " ++ Neon.errorToString problem)
                ]
            )

        SessionEnded resolver (Ok ()) ->
            ( model, respond resolver (signedOut model) )

        SessionEnded resolver (Err problem) ->
            ( model
            , Batch
                [ respond resolver unavailable
                , log Interop.Error ("Sign-out failed: " ++ Neon.errorToString problem)
                ]
            )

        VerificationFound resolver (Ok (Just found)) ->
            ( model, respond resolver (json 200 (Api.encodeResponse Api.verification found)) )

        VerificationFound resolver (Ok Nothing) ->
            ( model, respond resolver invalidLink )

        VerificationFound resolver (Err problem) ->
            ( model
            , Batch
                [ respond resolver unavailable
                , log Interop.Error ("Verification failed at its link: " ++ Neon.errorToString problem)
                ]
            )

        VerificationConsumed resolver (Ok (Db.Verified { sessionToken, account })) ->
            let
                response =
                    json 200 (Api.encodeResponse Api.verify account)
            in
            ( model
            , respond resolver
                { response | headers = ( "set-cookie", sessionCookie model sessionToken ) :: response.headers }
            )

        VerificationConsumed resolver (Ok Db.InvalidLink) ->
            ( model, respond resolver invalidLink )

        VerificationConsumed resolver (Ok Db.UsernameTakenMeanwhile) ->
            ( model, respond resolver (error 409 "That username was taken in the meantime. Sign up again with another.") )

        VerificationConsumed resolver (Err problem) ->
            ( model
            , Batch
                [ respond resolver unavailable
                , log Interop.Error ("Verification failed at its session: " ++ Neon.errorToString problem)
                ]
            )

        PasskeysFound resolver (Ok (Just found)) ->
            ( model, respond resolver (json 200 (Api.encodeResponse Api.passkeys found)) )

        PasskeysFound resolver (Ok Nothing) ->
            ( model, respond resolver notSignedIn )

        PasskeysFound resolver (Err problem) ->
            ( model
            , Batch
                [ respond resolver unavailable
                , log Interop.Error ("Passkey list failed: " ++ Neon.errorToString problem)
                ]
            )

        PasskeyStarted resolver (Ok (Just ceremony)) ->
            ( model
            , respond resolver
                (json 200
                    (Api.encodeResponse Api.passkeyOptions
                        { challenge = ceremony.challenge
                        , rp = { id = rpId model, name = "pixo" }
                        , user =
                            { id = ceremony.userId
                            , name = ceremony.account.email
                            , displayName = ceremony.account.username
                            }
                        , algorithms = WebAuthn.algorithms
                        , excludeCredentials = ceremony.passkeys
                        }
                    )
                )
            )

        PasskeyStarted resolver (Ok Nothing) ->
            ( model, respond resolver notSignedIn )

        PasskeyStarted resolver (Err problem) ->
            ( model
            , Batch
                [ respond resolver unavailable
                , log Interop.Error ("Passkey failed at its challenge: " ++ Neon.errorToString problem)
                ]
            )

        PasskeyAdded resolver (Ok (Just (Db.Added passkey account))) ->
            ( model
            , notify model
                resolver
                (json 201 (Api.encodeResponse Api.addPasskey passkey))
                { to = account.email, tag = "passkey-added" }
                (Email.PasskeyAdded.email
                    { url = dashboardUrl model
                    , username = account.username
                    , passkey = passkey.name
                    }
                )
            )

        PasskeyAdded resolver (Ok (Just Db.NotAdded)) ->
            ( model, respond resolver (error 409 "This passkey request expired or was used already. Try again.") )

        PasskeyAdded resolver (Ok Nothing) ->
            ( model, respond resolver notSignedIn )

        PasskeyAdded resolver (Err problem) ->
            ( model
            , Batch
                [ respond resolver unavailable
                , log Interop.Error ("Passkey failed at its credential: " ++ Neon.errorToString problem)
                ]
            )

        PasskeyRemoved resolver (Ok (Just (Db.Removed { name, account }))) ->
            ( model
            , notify model
                resolver
                (json 200 (Api.encodeResponse (Api.deletePasskey "") ()))
                { to = account.email, tag = "passkey-removed" }
                (Email.PasskeyRemoved.email
                    { url = dashboardUrl model
                    , username = account.username
                    , passkey = name
                    }
                )
            )

        PasskeyRemoved resolver (Ok (Just Db.NotRemoved)) ->
            ( model, respond resolver (error 404 "This account has no such passkey.") )

        PasskeyRemoved resolver (Ok Nothing) ->
            ( model, respond resolver notSignedIn )

        PasskeyRemoved resolver (Err problem) ->
            ( model
            , Batch
                [ respond resolver unavailable
                , log Interop.Error ("Passkey removal failed: " ++ Neon.errorToString problem)
                ]
            )

        Notified resolver response (Ok _) ->
            ( model, respond resolver response )

        Notified resolver response (Err problem) ->
            ( model
            , Batch
                [ respond resolver response
                , log Interop.Error ("Passkey notice failed at its email: " ++ Brevo.errorToString problem)
                ]
            )

        SignInStarted resolver (Ok (Db.SignInChallenge challenge)) ->
            ( model
            , respond resolver
                (json 200 (Api.encodeResponse Api.passkeySignInOptions { challenge = challenge, rpId = rpId model }))
            )

        SignInStarted resolver (Ok (Db.TooManyChallenges limited)) ->
            ( model, respond resolver (tooManyRequests limited.retryAfter) )

        SignInStarted resolver (Err problem) ->
            ( model
            , Batch
                [ respond resolver unavailable
                , log Interop.Error ("Passkey sign-in failed at its challenge: " ++ Neon.errorToString problem)
                ]
            )

        KeyFound resolver assertion (Ok (Just publicKey)) ->
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

        KeyFound resolver _ (Ok Nothing) ->
            ( model, refuse resolver "No passkey has this credential id." )

        KeyFound resolver _ (Err problem) ->
            ( model
            , Batch
                [ respond resolver unavailable
                , log Interop.Error ("Passkey sign-in failed at its key: " ++ Neon.errorToString problem)
                ]
            )

        GotInterop (Ok (Interop.SignatureChecked resolver signIn valid)) ->
            ( model
            , case ( valid, model.db ) of
                ( True, Ok db ) ->
                    Database
                        (Neon.run db
                            (Db.signInWithPasskey
                                { challenge = signIn.challenge
                                , credentialId = signIn.credentialId
                                , signCount = signIn.signCount
                                , backedUp = signIn.backedUp
                                , sessionSeconds = sessionSeconds
                                }
                            )
                            (\result _ -> PasskeySignedIn resolver result)
                        )

                ( True, Err _ ) ->
                    respond resolver unavailable

                ( False, _ ) ->
                    refuse resolver "The signature does not match the passkey."
            )

        PasskeySignedIn resolver (Ok (Db.PasskeySignedIn { sessionToken, account })) ->
            let
                response =
                    json 200 (Api.encodeResponse Api.passkeySignIn account)
            in
            ( model
            , respond resolver
                { response | headers = ( "set-cookie", sessionCookie model sessionToken ) :: response.headers }
            )

        PasskeySignedIn resolver (Ok Db.ChallengeSpent) ->
            ( model, refuse resolver "The challenge is expired, used or unknown." )

        PasskeySignedIn resolver (Ok Db.PasskeyStale) ->
            ( model, refuse resolver "The passkey is gone, or its counter did not move forward." )

        PasskeySignedIn resolver (Err problem) ->
            ( model
            , Batch
                [ respond resolver unavailable
                , log Interop.Error ("Passkey sign-in failed at its session: " ++ Neon.errorToString problem)
                ]
            )

        GotInterop (Err _) ->
            ( model, NoEffect )


notify : Model -> Interop.Resolver -> Response -> { to : String, tag : String } -> Email.Layout.Email -> Effect
notify model resolver response envelope email =
    case model.email of
        Ok mailer ->
            Email (Email.send mailer envelope email (Notified resolver response))

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
    | Email (Brevo.Request Msg)


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

        Email request ->
            Brevo.send request
                |> Task.perform identity



-- ROUTES


route : Model -> Request -> ( Model, Reply )
route model request =
    case Api.parse request.method request.path of
        Ok r ->
            handle model request r

        Err Api.NotFound ->
            ( model, Now (error 404 "Not found.") )

        Err (Api.MethodNotAllowed allowed) ->
            let
                response =
                    error 405 "Method not allowed."
            in
            ( model
            , Now
                { response
                    | headers =
                        ( "allow", String.join ", " (List.map Api.methodToString allowed) )
                            :: response.headers
                }
            )


handle : Model -> Request -> Api.Route -> ( Model, Reply )
handle model request r =
    case r of
        Api.GetHealth ->
            ( model
            , case model.db of
                Ok db ->
                    Later
                        (\resolver ->
                            Database
                                (Neon.run (Neon.withTimeout healthTimeout db)
                                    Db.ping
                                    (\result latency -> DatabaseChecked resolver ( latency, result ))
                                )
                        )

                Err problem ->
                    Now (health model { status = Api.Fail, latencyMs = Nothing, output = Just ("Not configured: " ++ problem) })
            )

        Api.PostSignUp ->
            ( model
            , askForLink model
                request
                (Decode.decodeString (Api.bodyDecoder Api.signUp) request.body
                    |> Result.mapError (\_ -> error 400 "Expected a JSON object with an email and a username.")
                    |> Result.andThen (SignUp.validate >> Result.mapError (error 422))
                    |> Result.map SignUpAsked
                )
            )

        Api.PostSignIn ->
            ( model
            , askForLink model
                request
                (Decode.decodeString (Api.bodyDecoder Api.signIn) request.body
                    |> Result.mapError (\_ -> error 400 "Expected a JSON object with an email.")
                    |> Result.andThen (.email >> SignUp.validateEmail >> Result.mapError (error 422))
                    |> Result.map SignInAsked
                )
            )

        Api.GetSession ->
            ( model
            , withSession model
                request
                (\db token resolver ->
                    Database (Neon.commit db (Db.account token) (\result _ -> SessionFound resolver result))
                )
            )

        Api.DeleteSession ->
            ( model
            , case ( model.db, cookieToken request ) of
                ( Ok db, Just token ) ->
                    Later
                        (\resolver ->
                            Database (Neon.run db (Db.signOut token) (\result _ -> SessionEnded resolver result))
                        )

                ( Ok _, Nothing ) ->
                    Now (signedOut model)

                ( Err _, _ ) ->
                    Now unavailable
            )

        Api.GetVerification ->
            ( model
            , withLink model
                request
                (\db token resolver ->
                    Database (Neon.run db (Db.magicLink token) (\result _ -> VerificationFound resolver result))
                )
            )

        Api.PostVerification ->
            ( model
            , withLink model
                request
                (\db token resolver ->
                    Database
                        (Neon.run db
                            (Db.consumeMagicLink { token = token, sessionSeconds = sessionSeconds })
                            (\result _ -> VerificationConsumed resolver result)
                        )
                )
            )

        Api.GetPasskeys ->
            ( model
            , withSession model
                request
                (\db token resolver ->
                    Database (Neon.commit db (Db.passkeys token) (\result _ -> PasskeysFound resolver result))
                )
            )

        Api.PostPasskeyOptions ->
            ( model
            , withSession model
                request
                (\db token resolver ->
                    Database
                        (Neon.commit db
                            (Db.startPasskey { token = token, ttlSeconds = challengeSeconds })
                            (\result _ -> PasskeyStarted resolver result)
                        )
                )
            )

        Api.PostPasskey ->
            ( model
            , withSession model
                request
                (\db token resolver ->
                    case
                        Decode.decodeString (Api.bodyDecoder Api.addPasskey) request.body
                            |> Result.mapError (\_ -> Nothing)
                            |> Result.andThen
                                (WebAuthn.register { origin = model.origin, rpId = rpId model }
                                    >> Result.mapError Just
                                )
                    of
                        Ok registration ->
                            Database
                                (Neon.commit db
                                    (Db.addPasskey
                                        { token = token
                                        , registration = registration
                                        , name = Aaguid.name registration.aaguid
                                        }
                                    )
                                    (\result _ -> PasskeyAdded resolver result)
                                )

                        Err Nothing ->
                            respond resolver
                                (error 400 "Expected a JSON object with a clientDataJSON, an attestationObject and transports.")

                        Err (Just problem) ->
                            Batch
                                [ respond resolver (error 422 "This passkey could not be verified. Try again.")
                                , log Interop.Warn ("Passkey refused: " ++ String.left 200 problem)
                                ]
                )
            )

        Api.DeletePasskey id ->
            ( model
            , withSession model
                request
                (\db token resolver ->
                    Database
                        (Neon.commit db
                            (Db.removePasskey { token = token, id = id })
                            (\result _ -> PasskeyRemoved resolver result)
                        )
                )
            )

        Api.PostPasskeySignInOptions ->
            ( model
            , case model.db of
                Ok db ->
                    Later
                        (\resolver ->
                            Database
                                (Neon.run db
                                    (Db.signInChallenge
                                        { key = "passkey:ip:" ++ clientAddress request
                                        , limit = 20
                                        , windowSeconds = 900
                                        , ttlSeconds = challengeSeconds
                                        }
                                    )
                                    (\result _ -> SignInStarted resolver result)
                                )
                        )

                Err _ ->
                    Now unavailable
            )

        Api.PostPasskeySignIn ->
            ( model
            , case ( model.db, Decode.decodeString (Api.bodyDecoder Api.passkeySignIn) request.body ) of
                ( Ok db, Ok assertion ) ->
                    Later
                        (\resolver ->
                            case WebAuthn.authenticate { origin = model.origin, rpId = rpId model } assertion of
                                Ok authentication ->
                                    Database
                                        (Neon.run db
                                            (Db.passkeyPublicKey assertion.id)
                                            (\result _ ->
                                                KeyFound resolver
                                                    { signed = authentication.signed
                                                    , signature = assertion.signature
                                                    , signIn =
                                                        { challenge = authentication.challenge
                                                        , credentialId = assertion.id
                                                        , signCount = authentication.signCount
                                                        , backedUp = authentication.backedUp
                                                        }
                                                    }
                                                    result
                                            )
                                        )

                                Err problem ->
                                    refuse resolver problem
                        )

                ( Ok _, Err _ ) ->
                    Now (error 400 "Expected a JSON object with an id, a clientDataJSON, an authenticatorData and a signature.")

                ( Err _, _ ) ->
                    Now unavailable
            )


askForLink : Model -> Request -> Result Response Ask -> Reply
askForLink model request parsed =
    case ( model.db, model.email, parsed ) of
        ( Ok db, Ok _, Ok ask ) ->
            Later
                (\resolver ->
                    Database
                        (Neon.commit db
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
                                        { key = "magic-link:email:" ++ askedBy ask
                                        , limit = 3
                                        , windowSeconds = 900
                                        }
                                    )
                            )
                            (\result _ -> LinkLimited resolver ask result)
                        )
                )

        ( Ok _, Ok _, Err response ) ->
            Now response

        _ ->
            Now unavailable


withSession : Model -> Request -> (Neon.Connection -> String -> Interop.Resolver -> Effect) -> Reply
withSession model request toEffect =
    case ( model.db, cookieToken request ) of
        ( Ok db, Just token ) ->
            Later (toEffect db token)

        ( Ok _, Nothing ) ->
            Now notSignedIn

        ( Err _, _ ) ->
            Now unavailable


cookieToken : Request -> Maybe String
cookieToken request =
    Dict.get "cookie" request.headers
        |> Maybe.withDefault ""
        |> String.split ";"
        |> List.map String.trim
        |> List.filter (String.startsWith "session=")
        |> List.head
        |> Maybe.map (String.dropLeft 8)
        |> Maybe.andThen
            (\token ->
                if String.isEmpty token then
                    Nothing

                else
                    Just token
            )


withLink : Model -> Request -> (Neon.Connection -> String -> Interop.Resolver -> Effect) -> Reply
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


bearer : Request -> Maybe String
bearer request =
    Dict.get "authorization" request.headers
        |> Maybe.andThen
            (\value ->
                if String.toLower (String.left 7 value) == "bearer " then
                    Just (String.trim (String.dropLeft 7 value))

                else
                    Nothing
            )
        |> Maybe.andThen
            (\token ->
                if String.isEmpty token then
                    Nothing

                else
                    Just token
            )


verifyUrl : Model -> String -> String
verifyUrl model token =
    model.origin ++ "/auth/verify#" ++ token


dashboardUrl : Model -> String
dashboardUrl model =
    model.origin ++ "/dashboard"


sessionSeconds : Int
sessionSeconds =
    30 * 24 * 60 * 60


sessionCookie : Model -> String -> String
sessionCookie model token =
    cookie model token sessionSeconds


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
            , if String.startsWith "https://" model.origin then
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


rpId : Model -> String
rpId model =
    WebAuthn.rpId model.origin
        |> Maybe.withDefault ""


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
