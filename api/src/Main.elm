module Main exposing (Model, Msg(..), init, main, update)

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
import Effect exposing (Effect(..), log, query, respond, transact)
import InteropDefinitions as Interop
import InteropPorts
import Json.Decode as Decode
import Json.Encode as Encode
import Path
import RateLimit
import Request exposing (Request)
import Response exposing (Response)
import Services.Brevo as Brevo
import Services.Mailer as Mailer
import Services.Mailer.Layout as Layout
import Services.Mailer.PasskeyAdded
import Services.Mailer.PasskeyRemoved
import Services.Mailer.SignIn
import Services.Mailer.SignUp
import Services.Neon as Neon
import SessionCookie
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


init : Decode.Value -> ( Model, Effect Msg )
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



-- UPDATE


type Msg
    = GotInterop (Result Decode.Error Interop.ToElm)
    | DatabaseChecked Interop.Resolver ( Int, Result Neon.Error RateLimit.Outcome )
    | DatabaseFailed Effect.Failure
    | LinkLimited Interop.Resolver Mailer.Mailer Ask ( RateLimit.Outcome, RateLimit.Outcome )
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
    | Later (Interop.Resolver -> Effect Msg)


update : Msg -> Model -> ( Model, Effect Msg )
update msg model =
    ( model
    , case model of
        Running server ->
            running server msg

        Broken broken ->
            whenBroken broken msg
    )


whenBroken : { problem : String, release : Api.Release } -> Msg -> Effect Msg
whenBroken broken msg =
    case msg of
        GotInterop (Ok (Interop.GotRequest resolver request)) ->
            respond resolver
                (case Api.parse request.method request.path of
                    Ok found ->
                        brokenAnswer broken found

                    Err refused ->
                        Response.notRouted refused
                )

        _ ->
            NoEffect


brokenAnswer : { problem : String, release : Api.Release } -> Api.Route -> Response
brokenAnswer { problem, release } found =
    case found of
        Api.GetHealth ->
            Response.health release { status = Api.Fail, latencyMs = Nothing, output = Just ("Not configured: " ++ problem) }

        Api.PostSignUp ->
            Response.unavailable

        Api.PostSignIn ->
            Response.unavailable

        Api.GetSession ->
            Response.unavailable

        Api.DeleteSession ->
            Response.unavailable

        Api.GetVerification ->
            Response.unavailable

        Api.PostVerification ->
            Response.unavailable

        Api.GetPasskeys ->
            Response.unavailable

        Api.PostPasskey ->
            Response.unavailable

        Api.PostPasskeyOptions ->
            Response.unavailable

        Api.DeletePasskey _ ->
            Response.unavailable

        Api.PostPasskeySignIn ->
            Response.unavailable

        Api.PostPasskeySignInOptions ->
            Response.unavailable


reply : Interop.Resolver -> Reply -> Effect Msg
reply resolver answer =
    case answer of
        Now response ->
            respond resolver response

        Later toEffect ->
            toEffect resolver


running : Server -> Msg -> Effect Msg
running server msg =
    case msg of
        GotInterop (Ok (Interop.GotRequest resolver request)) ->
            reply resolver (route server request)

        DatabaseChecked resolver ( _, Ok (RateLimit.Limited limited) ) ->
            respond resolver (Response.tooManyRequests limited.retryAfter)

        DatabaseChecked resolver ( latency, Ok (RateLimit.Allowed _) ) ->
            if latency > slowAfter then
                Batch
                    [ respond resolver
                        (Response.health server.release
                            { status = Api.Warn
                            , latencyMs = Just latency
                            , output = Just ("Slower than " ++ String.fromInt slowAfter ++ " ms.")
                            }
                        )
                    , log Interop.Warn ("Health check slow: the database took " ++ String.fromInt latency ++ " ms.")
                    ]

            else
                respond resolver
                    (Response.health server.release { status = Api.Pass, latencyMs = Just latency, output = Nothing })

        DatabaseChecked resolver ( latency, Err problem ) ->
            Batch
                [ respond resolver
                    (Response.health server.release { status = Api.Fail, latencyMs = Just latency, output = Just (failure problem) })
                , log Interop.Error ("Health check failed: " ++ Neon.errorToString problem)
                ]

        DatabaseFailed { resolver, step, problem } ->
            Batch
                [ respond resolver Response.unavailable
                , log Interop.Error (step ++ ": " ++ Neon.errorToString problem)
                ]

        LinkLimited resolver mailer ask ( byAddress, byEmail ) ->
            case RateLimit.retryAfter [ byAddress, byEmail ] of
                Just seconds ->
                    respond resolver (Response.tooManyRequests seconds)

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
                    respond resolver (Response.error 409 "That username is taken.")

                Db.SignUpLink token ->
                    sendLink mailer
                        resolver
                        { to = signUp.email, tag = "sign-up" }
                        (Services.Mailer.SignUp.email
                            { url = Origin.href server.origin (Path.Verify (Just token))
                            , username = signUp.username
                            , expiresInMinutes = linkMinutes
                            }
                        )

                Db.SignInInstead token ->
                    sendLink mailer resolver { to = signUp.email, tag = "sign-in" } (signInEmail server token)

        SignInLinkCreated resolver mailer email link ->
            case link of
                Db.NoAccount ->
                    respond resolver Response.accepted

                Db.SignInLink token ->
                    sendLink mailer resolver { to = email, tag = "sign-in" } (signInEmail server token)

        LinkSent resolver (Ok _) ->
            respond resolver Response.accepted

        LinkSent resolver (Err problem) ->
            Batch
                [ respond resolver (Response.error 502 "The email could not be sent. Try again.")
                , log Interop.Error ("Magic link failed at its email: " ++ Brevo.errorToString problem)
                ]

        SessionFound resolver (Just account) ->
            respond resolver (Response.json 200 (Api.encodeResponse Api.session account))

        SessionFound resolver Nothing ->
            respond resolver Response.notSignedIn

        SessionEnded resolver ->
            respond resolver (signedOut server)

        VerificationFound resolver (Just found) ->
            respond resolver (Response.json 200 (Api.encodeResponse Api.verification found))

        VerificationFound resolver Nothing ->
            respond resolver Response.invalidLink

        VerificationConsumed resolver (Db.Verified { sessionToken, account }) ->
            respond resolver
                (Response.json 200 (Api.encodeResponse Api.verify account)
                    |> SessionCookie.set server.origin sessionToken
                )

        VerificationConsumed resolver Db.InvalidLink ->
            respond resolver Response.invalidLink

        VerificationConsumed resolver Db.UsernameTakenMeanwhile ->
            respond resolver (Response.error 409 "That username was taken in the meantime. Sign up again with another.")

        PasskeysFound resolver (Just found) ->
            respond resolver (Response.json 200 (Api.encodeResponse Api.passkeys found))

        PasskeysFound resolver Nothing ->
            respond resolver Response.notSignedIn

        PasskeyStarted resolver (Just ceremony) ->
            respond resolver
                (Response.json 200
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
            respond resolver Response.notSignedIn

        PasskeyAdded resolver (Just (Db.Added passkey account)) ->
            notify server
                resolver
                (Response.json 201 (Api.encodeResponse Api.addPasskey passkey))
                { to = account.email, tag = "passkey-added" }
                (Services.Mailer.PasskeyAdded.email
                    { url = Origin.href server.origin Path.Dashboard
                    , username = account.username
                    , passkey = passkey.name
                    }
                )

        PasskeyAdded resolver (Just Db.NotAdded) ->
            respond resolver (Response.error 409 "This passkey request expired or was used already. Try again.")

        PasskeyAdded resolver Nothing ->
            respond resolver Response.notSignedIn

        PasskeyRemoved resolver id (Just (Db.Removed { name, account })) ->
            notify server
                resolver
                (Response.json 200 (Api.encodeResponse (Api.deletePasskey id) ()))
                { to = account.email, tag = "passkey-removed" }
                (Services.Mailer.PasskeyRemoved.email
                    { url = Origin.href server.origin Path.Dashboard
                    , username = account.username
                    , passkey = name
                    }
                )

        PasskeyRemoved resolver _ (Just Db.NotRemoved) ->
            respond resolver (Response.error 404 "This account has no such passkey.")

        PasskeyRemoved resolver _ Nothing ->
            respond resolver Response.notSignedIn

        Notified resolver response (Ok _) ->
            respond resolver response

        Notified resolver response (Err problem) ->
            Batch
                [ respond resolver response
                , log Interop.Error ("Passkey notice failed at its email: " ++ Brevo.errorToString problem)
                ]

        SignInStarted resolver (Db.SignInChallenge challenge) ->
            respond resolver
                (Response.json 200 (Api.encodeResponse Api.passkeySignInOptions { challenge = challenge, rpId = Origin.rpId server.origin }))

        SignInStarted resolver (Db.TooManyChallenges limited) ->
            respond resolver (Response.tooManyRequests limited.retryAfter)

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
                        , sessionSeconds = SessionCookie.lifetimeSeconds
                        }
                    )
                    (PasskeySignedIn resolver)

            else
                refuse resolver "The signature does not match the passkey."

        PasskeySignedIn resolver (Db.PasskeySignedIn { sessionToken, account }) ->
            respond resolver
                (Response.json 200 (Api.encodeResponse Api.passkeySignIn account)
                    |> SessionCookie.set server.origin sessionToken
                )

        PasskeySignedIn resolver Db.ChallengeSpent ->
            refuse resolver "The challenge is expired, used or unknown."

        PasskeySignedIn resolver Db.PasskeyStale ->
            refuse resolver "The passkey is gone, or its counter did not move forward."

        GotInterop (Err _) ->
            NoEffect


sendLink : Mailer.Mailer -> Interop.Resolver -> { to : Email, tag : String } -> Layout.Message -> Effect Msg
sendLink mailer resolver envelope message =
    Mail (Mailer.send mailer envelope message (LinkSent resolver))


signInEmail : Server -> LinkToken -> Layout.Message
signInEmail server token =
    Services.Mailer.SignIn.email
        { url = Origin.href server.origin (Path.Verify (Just token))
        , expiresInMinutes = linkMinutes
        }


notify : Server -> Interop.Resolver -> Response -> { to : Email, tag : String } -> Layout.Message -> Effect Msg
notify server resolver response envelope email =
    case server.mailer of
        Ok mailer ->
            Mail (Mailer.send mailer envelope email (Notified resolver response))

        Err _ ->
            respond resolver response


refuse : Interop.Resolver -> String -> Effect Msg
refuse resolver problem =
    Batch
        [ respond resolver (Response.error 401 "This passkey could not be verified. Try again, or sign in with an emailed link.")
        , log Interop.Warn ("Passkey sign-in refused: " ++ String.left 200 problem)
        ]



-- EFFECTS


perform : Effect Msg -> Cmd Msg
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
                                (\toResult ->
                                    Time.now
                                        |> Task.map
                                            (\end ->
                                                case toResult (Time.posixToMillis end - Time.posixToMillis start) of
                                                    Ok msg ->
                                                        msg

                                                    Err failed ->
                                                        DatabaseFailed failed
                                            )
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
            Now (Response.notRouted refused)


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
                                    (Db.rateLimit (RateLimit.healthChecks (Request.clientAddress request)))
                                |> Neon.with Db.ping
                            )
                            (\result latency -> Ok (DatabaseChecked resolver ( latency, result )))
                        )
                )

        Api.PostSignUp ->
            askForLink server
                request
                (Decode.decodeString (Api.bodyDecoder Api.signUp) request.body
                    |> Result.mapError (\_ -> Response.error 400 "Expected a JSON object with an email and a username.")
                    |> Result.andThen (SignUp.parse >> Result.mapError (Response.error 422))
                    |> Result.map SignUpAsked
                )

        Api.PostSignIn ->
            askForLink server
                request
                (Decode.decodeString (Api.bodyDecoder Api.signIn) request.body
                    |> Result.mapError (\_ -> Response.error 400 "Expected a JSON object with an email.")
                    |> Result.andThen (.email >> Email.parse >> Result.mapError (Response.error 422))
                    |> Result.map SignInAsked
                )

        Api.GetSession ->
            withSession
                request
                (\token resolver ->
                    transact server.db resolver "Session lookup failed" (Db.account token) (SessionFound resolver)
                )

        Api.DeleteSession ->
            case SessionCookie.read request of
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
                        (Db.consumeMagicLink { token = token, sessionSeconds = SessionCookie.lifetimeSeconds })
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
                                (Response.error 400 "Expected a JSON object with a clientDataJSON, an attestationObject and transports.")

                        Err (Just problem) ->
                            Batch
                                [ respond resolver (Response.error 422 "This passkey could not be verified. Try again.")
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
                            { policy = RateLimit.passkeyChallenges (Request.clientAddress request)
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
                    Now (Response.error 400 "Expected a JSON object with an id, a clientDataJSON, an authenticatorData and a signature.")


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
                                (Db.rateLimit (RateLimit.magicLinksByAddress (Request.clientAddress request)))
                            |> Neon.with
                                (Db.rateLimit (RateLimit.magicLinksByEmail (askedBy ask)))
                        )
                        (LinkLimited resolver mailer ask)
                )

        ( Ok _, Err response ) ->
            Now response

        ( Err _, _ ) ->
            Now Response.unavailable


withSession : Request -> (SessionToken -> Interop.Resolver -> Effect Msg) -> Reply
withSession request toEffect =
    case SessionCookie.read request of
        Just token ->
            Later (toEffect token)

        Nothing ->
            Now Response.notSignedIn


withLink : Request -> (LinkToken -> Interop.Resolver -> Effect Msg) -> Reply
withLink request toEffect =
    case Request.linkToken request of
        Just token ->
            Later (toEffect token)

        Nothing ->
            Now
                (Response.error 401 "Send the link's token as a bearer token."
                    |> Response.withHeader "www-authenticate" "Bearer"
                )


linkMinutes : Int
linkMinutes =
    15


challengeSeconds : Int
challengeSeconds =
    300


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


signedOut : Server -> Response
signedOut server =
    Response.json 200 (Api.encodeResponse Api.signOut ())
        |> SessionCookie.clear server.origin
