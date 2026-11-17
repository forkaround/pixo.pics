module Main exposing (Model, Msg(..), init, main, update)

import Api
import Data.ClientAddress exposing (ClientAddress)
import Data.Email as Email
import Data.Origin as Origin
import Db
import Effect exposing (Effect(..), log, respond)
import Flow.Billing
import Flow.Feed
import Flow.Health
import Flow.MagicLink
import Flow.PasskeySignIn
import Flow.Passkeys
import Flow.Pictures
import Flow.Session
import InteropDefinitions as Interop
import InteropPorts
import Json.Decode as Decode
import RateLimit
import Reply exposing (Reply(..))
import Request exposing (Request)
import Response exposing (Response)
import Server exposing (Server)
import Services.BrevoMailer as BrevoMailer
import Services.LogMailer as LogMailer
import Services.Neon as Neon
import Services.Paddle as Paddle
import Services.R2 as R2
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
    = Running Server RateLimit.Limits
    | Broken { problem : String, release : Api.Release }


init : Decode.Value -> ( Model, Effect Msg )
init flags =
    case InteropPorts.decodeFlags flags of
        Ok config ->
            let
                release =
                    { releaseId = config.releaseId, releaseUrl = config.releaseUrl, region = config.region }

                mailer =
                    case ( config.brevoApiKey, config.emailFrom ) of
                        ( Just apiKey, Just from ) ->
                            Email.parse from
                                |> Result.mapError (\_ -> "EMAIL_FROM is not an email address.")
                                |> Result.map (\sender -> ( BrevoMailer.mailer { apiKey = apiKey, from = sender }, NoEffect ))

                        _ ->
                            if config.local then
                                Ok
                                    ( LogMailer.mailer
                                    , log Interop.Warn "No BREVO_API_KEY, so emails are printed here instead of sent."
                                    )

                            else
                                Err "BREVO_API_KEY and EMAIL_FROM must both be set."
            in
            case ( Origin.parse config.origin, Db.connect config, mailer ) of
                ( Ok origin, Ok db, Ok ( chosen, notice ) ) ->
                    let
                        ( paddle, billingNotice ) =
                            case Paddle.config config of
                                Ok Nothing ->
                                    ( Nothing, log Interop.Warn "No PADDLE_* settings, so billing routes answer 503." )

                                Ok found ->
                                    ( found, NoEffect )

                                Err problem ->
                                    ( Nothing, log Interop.Error problem )

                        ( storage, storageNotice ) =
                            case R2.config config of
                                Ok Nothing ->
                                    ( Nothing, log Interop.Warn "No R2_* settings, so publishing answers 503." )

                                Ok found ->
                                    ( found, NoEffect )

                                Err problem ->
                                    ( Nothing, log Interop.Error problem )

                        ( moderators, moderationNotice ) =
                            case config.moderationEmail of
                                Nothing ->
                                    ( Nothing, log Interop.Warn "No MODERATION_EMAIL, so reports send no email." )

                                Just raw ->
                                    case Email.parse raw of
                                        Ok found ->
                                            ( Just found, NoEffect )

                                        Err _ ->
                                            ( Nothing, log Interop.Error "MODERATION_EMAIL is not an email address." )
                    in
                    ( Running { db = db, mailer = chosen, moderators = moderators, origin = origin, paddle = paddle, release = release, storage = storage } RateLimit.empty
                    , Batch [ log Interop.Info ("Sending queries to " ++ Neon.endpoint db), notice, billingNotice, storageNotice, moderationNotice ]
                    )

                ( Err problem, _, _ ) ->
                    ( Broken { problem = problem, release = release }
                    , log Interop.Error ("Bad origin: " ++ problem)
                    )

                ( Ok _, Err problem, _ ) ->
                    ( Broken { problem = problem, release = release }
                    , log Interop.Warn ("No database, so database routes answer 503: " ++ problem)
                    )

                ( Ok _, Ok _, Err problem ) ->
                    ( Broken { problem = problem, release = release }
                    , log Interop.Error ("No email, so every route but health answers 503: " ++ problem)
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
    | Arrived Interop.Resolver Request Time.Posix
    | DatabaseFailed Effect.Failure
    | Health Flow.Health.Msg
    | MagicLink Flow.MagicLink.Msg
    | Session Flow.Session.Msg
    | Passkeys Flow.Passkeys.Msg
    | PasskeySignIn Flow.PasskeySignIn.Msg
    | Billing Flow.Billing.Msg
    | Pictures Flow.Pictures.Msg
    | Feed Flow.Feed.Msg


update : Msg -> Model -> ( Model, Effect Msg )
update msg model =
    case model of
        Running server limits ->
            running server limits msg
                |> Tuple.mapFirst (Running server)

        Broken broken ->
            ( model, whenBroken broken msg )


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

        Api.DeleteAccount ->
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

        Api.PostBillingWebhook ->
            Response.unavailable

        Api.PostCheckout ->
            Response.unavailable

        Api.PostPortal ->
            Response.unavailable

        Api.PostPicture ->
            Response.unavailable

        Api.PostPicturePublished _ ->
            Response.unavailable

        Api.DeletePicture _ ->
            Response.unavailable

        Api.GetFeed _ _ ->
            Response.unavailable

        Api.GetFeedTags ->
            Response.unavailable

        Api.GetProfile _ _ ->
            Response.unavailable

        Api.PostReport _ ->
            Response.unavailable


reply : Time.Posix -> Interop.Resolver -> Reply Msg -> RateLimit.Limits -> ( RateLimit.Limits, Effect Msg )
reply time resolver answer limits =
    case answer of
        Now response ->
            ( limits, respond resolver response )

        Later toEffect ->
            ( limits, toEffect resolver )

        Limit policies next ->
            case RateLimit.check time policies limits of
                Ok counted ->
                    reply time resolver next counted

                Err seconds ->
                    ( limits, respond resolver (Response.tooManyRequests seconds) )


running : Server -> RateLimit.Limits -> Msg -> ( RateLimit.Limits, Effect Msg )
running server limits msg =
    case msg of
        GotInterop (Ok (Interop.GotRequest resolver request)) ->
            ( limits, Clock (Arrived resolver request) )

        Arrived resolver request time ->
            reply time
                resolver
                (Limit [ policyFor request ] (route server request))
                (RateLimit.sweep time limits)

        Health flowMsg ->
            ( limits, Effect.map Health (Flow.Health.update server flowMsg) )

        MagicLink flowMsg ->
            ( limits, Effect.map MagicLink (Flow.MagicLink.update server flowMsg) )

        Session flowMsg ->
            ( limits, Effect.map Session (Flow.Session.update server flowMsg) )

        Passkeys flowMsg ->
            ( limits, Effect.map Passkeys (Flow.Passkeys.update server flowMsg) )

        GotInterop (Ok (Interop.SignatureChecked resolver signIn valid)) ->
            ( limits, Effect.map PasskeySignIn (Flow.PasskeySignIn.signatureChecked server resolver signIn valid) )

        PasskeySignIn flowMsg ->
            ( limits, Effect.map PasskeySignIn (Flow.PasskeySignIn.update server flowMsg) )

        GotInterop (Ok (Interop.WebhookChecked resolver body valid)) ->
            ( limits, Effect.map Billing (Flow.Billing.webhookChecked server resolver body valid) )

        Billing flowMsg ->
            ( limits, Effect.map Billing (Flow.Billing.update server flowMsg) )

        Pictures flowMsg ->
            ( limits, Effect.map Pictures (Flow.Pictures.update server flowMsg) )

        Feed flowMsg ->
            ( limits, Effect.map Feed (Flow.Feed.update server flowMsg) )

        DatabaseFailed { resolver, step, problem } ->
            ( limits
            , Batch
                [ respond resolver Response.unavailable
                , log Interop.Error (step ++ ": " ++ Neon.errorToString problem)
                ]
            )

        GotInterop (Err _) ->
            ( limits, NoEffect )



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
            Task.map3
                (\start toResult end ->
                    case toResult (Time.posixToMillis end - Time.posixToMillis start) of
                        Ok msg ->
                            msg

                        Err failed ->
                            DatabaseFailed failed
                )
                Time.now
                (Neon.send request)
                Time.now
                |> Task.perform identity

        Http request ->
            Effect.toTask request
                |> Task.perform identity

        Succeed msg ->
            Task.succeed msg
                |> Task.perform identity

        Clock gotTime ->
            Task.perform gotTime Time.now



-- ROUTES


policyFor : Request -> RateLimit.Policy
policyFor request =
    case Api.parse request.method request.path of
        Ok found ->
            routePolicy found (Request.clientAddress request)

        Err _ ->
            RateLimit.requests (Request.clientAddress request)


routePolicy : Api.Route -> ClientAddress -> RateLimit.Policy
routePolicy found address =
    case found of
        Api.PostBillingWebhook ->
            RateLimit.webhooks address

        Api.GetHealth ->
            RateLimit.requests address

        Api.PostSignUp ->
            RateLimit.requests address

        Api.PostSignIn ->
            RateLimit.requests address

        Api.GetSession ->
            RateLimit.requests address

        Api.DeleteSession ->
            RateLimit.requests address

        Api.DeleteAccount ->
            RateLimit.requests address

        Api.GetVerification ->
            RateLimit.requests address

        Api.PostVerification ->
            RateLimit.requests address

        Api.GetPasskeys ->
            RateLimit.requests address

        Api.PostPasskey ->
            RateLimit.requests address

        Api.PostPasskeyOptions ->
            RateLimit.requests address

        Api.DeletePasskey _ ->
            RateLimit.requests address

        Api.PostPasskeySignIn ->
            RateLimit.requests address

        Api.PostPasskeySignInOptions ->
            RateLimit.requests address

        Api.PostCheckout ->
            RateLimit.requests address

        Api.PostPortal ->
            RateLimit.requests address

        Api.PostPicture ->
            RateLimit.requests address

        Api.PostPicturePublished _ ->
            RateLimit.requests address

        Api.DeletePicture _ ->
            RateLimit.requests address

        Api.GetFeed _ _ ->
            RateLimit.requests address

        Api.GetFeedTags ->
            RateLimit.requests address

        Api.GetProfile _ _ ->
            RateLimit.requests address

        Api.PostReport _ ->
            RateLimit.requests address


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
            Reply.map MagicLink (Flow.MagicLink.askSignUp server request)

        Api.PostSignIn ->
            Reply.map MagicLink (Flow.MagicLink.askSignIn server request)

        Api.GetSession ->
            Reply.map Session (Flow.Session.current server request)

        Api.DeleteAccount ->
            Reply.map Session (Flow.Session.deleteAccount server request)

        Api.DeleteSession ->
            Reply.map Session (Flow.Session.end server request)

        Api.GetVerification ->
            Reply.map MagicLink (Flow.MagicLink.verification server request)

        Api.PostVerification ->
            Reply.map MagicLink (Flow.MagicLink.verify server request)

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

        Api.PostBillingWebhook ->
            Reply.map Billing (Flow.Billing.webhook server request)

        Api.PostCheckout ->
            Reply.map Billing (Flow.Billing.checkout server request)

        Api.PostPortal ->
            Reply.map Billing (Flow.Billing.portal server request)

        Api.PostPicture ->
            Reply.map Pictures (Flow.Pictures.publish server request)

        Api.PostPicturePublished id ->
            Reply.map Pictures (Flow.Pictures.confirm id server request)

        Api.DeletePicture id ->
            Reply.map Pictures (Flow.Pictures.unpublish id server request)

        Api.GetFeed tag before ->
            Reply.map Feed (Flow.Feed.feed tag before server request)

        Api.GetFeedTags ->
            Reply.map Feed (Flow.Feed.tags server request)

        Api.GetProfile name before ->
            Reply.map Feed (Flow.Feed.profile name before server request)

        Api.PostReport id ->
            Reply.map Pictures (Flow.Pictures.report id server request)
