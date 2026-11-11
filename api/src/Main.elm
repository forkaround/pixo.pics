module Main exposing (Model, Msg(..), init, main, update)

import Api
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
import Services.Brevo as Brevo
import Services.Mailer as Mailer
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
