module Main exposing (Model, Msg(..), init, main, update)

import Api
import Data.Email as Email exposing (Email)
import Data.LinkToken as LinkToken exposing (LinkToken)
import Data.Origin as Origin exposing (Origin)
import Data.SignUp as SignUp
import Db
import Effect exposing (Effect(..), log, query, respond, transact)
import Flow.Health
import Flow.PasskeySignIn
import Flow.Passkeys
import Flow.Session
import InteropDefinitions as Interop
import InteropPorts
import Json.Decode as Decode
import Json.Encode as Encode
import Path
import RateLimit
import Reply exposing (Reply(..), withSession)
import Request exposing (Request)
import Response exposing (Response)
import Server exposing (Server)
import Services.Brevo as Brevo
import Services.Mailer as Mailer
import Services.Mailer.Layout as Layout
import Services.Mailer.SignIn
import Services.Mailer.SignUp
import Services.Neon as Neon
import SessionCookie
import Task
import Time


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
    | DatabaseFailed Effect.Failure
    | Health Flow.Health.Msg
    | Session Flow.Session.Msg
    | Passkeys Flow.Passkeys.Msg
    | PasskeySignIn Flow.PasskeySignIn.Msg
    | LinkLimited Interop.Resolver Mailer.Mailer Ask ( RateLimit.Outcome, RateLimit.Outcome )
    | SignUpLinkCreated Interop.Resolver Mailer.Mailer SignUp.SignUp Db.SignUpLink
    | SignInLinkCreated Interop.Resolver Mailer.Mailer Email Db.SignInLink
    | LinkSent Interop.Resolver (Result Brevo.Error Brevo.Sent)
    | VerificationFound Interop.Resolver (Maybe Api.Verification)
    | VerificationConsumed Interop.Resolver Db.Consumed


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


reply : Interop.Resolver -> Reply Msg -> Effect Msg
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

        Health flowMsg ->
            Effect.map Health (Flow.Health.update server flowMsg)

        Session flowMsg ->
            Effect.map Session (Flow.Session.update server flowMsg)

        Passkeys flowMsg ->
            Effect.map Passkeys (Flow.Passkeys.update server flowMsg)

        GotInterop (Ok (Interop.SignatureChecked resolver signIn valid)) ->
            Effect.map PasskeySignIn (Flow.PasskeySignIn.signatureChecked server resolver signIn valid)

        PasskeySignIn flowMsg ->
            Effect.map PasskeySignIn (Flow.PasskeySignIn.update server flowMsg)

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


route : Server -> Request -> Reply Msg
route server request =
    case Api.parse request.method request.path of
        Ok found ->
            handle server request found

        Err refused ->
            Now (Response.notRouted refused)


handle : Server -> Request -> Api.Route -> Reply Msg
handle server request r =
    case r of
        Api.GetHealth ->
            Reply.map Health (Flow.Health.check server request)

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
            Reply.map Session (Flow.Session.current server request)

        Api.DeleteSession ->
            Reply.map Session (Flow.Session.end server request)

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
            Reply.map Passkeys (Flow.Passkeys.list server request)

        Api.PostPasskeyOptions ->
            Reply.map Passkeys (Flow.Passkeys.options server request)

        Api.PostPasskey ->
            Reply.map Passkeys (Flow.Passkeys.add server request)

        Api.DeletePasskey id ->
            Reply.map Passkeys (Flow.Passkeys.remove id server request)

        Api.PostPasskeySignInOptions ->
            Reply.map PasskeySignIn (Flow.PasskeySignIn.options server request)

        Api.PostPasskeySignIn ->
            Reply.map PasskeySignIn (Flow.PasskeySignIn.authenticate server request)


askForLink : Server -> Request -> Result Response Ask -> Reply Msg
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


withLink : Request -> (LinkToken -> Interop.Resolver -> Effect Msg) -> Reply Msg
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
