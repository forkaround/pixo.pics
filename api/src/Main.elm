module Main exposing (Model, Msg(..), init, main, update)

import Api
import Data.Email as Email
import Data.Origin as Origin
import Db
import Effect exposing (Effect(..), log, respond)
import Flow.Health
import Flow.MagicLink
import Flow.PasskeySignIn
import Flow.Passkeys
import Flow.Session
import InteropDefinitions as Interop
import InteropPorts
import Json.Decode as Decode
import Reply exposing (Reply(..))
import Request exposing (Request)
import Response exposing (Response)
import Server exposing (Server)
import Services.BrevoMailer as BrevoMailer
import Services.LogMailer as LogMailer
import Services.Neon as Neon
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
                    ( Running { db = db, mailer = chosen, origin = origin, release = release }
                    , Batch [ log Interop.Info ("Sending queries to " ++ Neon.endpoint db), notice ]
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
    | DatabaseFailed Effect.Failure
    | Health Flow.Health.Msg
    | MagicLink Flow.MagicLink.Msg
    | Session Flow.Session.Msg
    | Passkeys Flow.Passkeys.Msg
    | PasskeySignIn Flow.PasskeySignIn.Msg


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

        MagicLink flowMsg ->
            Effect.map MagicLink (Flow.MagicLink.update server flowMsg)

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

        GotInterop (Err _) ->
            NoEffect



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
            Reply.map MagicLink (Flow.MagicLink.askSignUp server request)

        Api.PostSignIn ->
            Reply.map MagicLink (Flow.MagicLink.askSignIn server request)

        Api.GetSession ->
            Reply.map Session (Flow.Session.current server request)

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
