module Main exposing (Effect(..), Model, Msg(..), init, main, update)

import Api
import Db
import InteropDefinitions as Interop exposing (Request, Response)
import InteropPorts
import Json.Decode as Decode
import Json.Encode as Encode
import Neon
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


type alias Model =
    { db : Result String Neon.Connection
    , release : Api.Release
    }


init : Decode.Value -> ( Model, Effect )
init flags =
    case InteropPorts.decodeFlags flags of
        Ok config ->
            let
                db =
                    Db.connect config
            in
            ( { db = db
              , release = { releaseId = config.releaseId, releaseUrl = config.releaseUrl, region = config.region }
              }
            , case db of
                Ok connection ->
                    log Interop.Info ("Sending queries to " ++ Neon.endpoint connection)

                Err problem ->
                    log Interop.Warn ("No database, so database routes answer 503: " ++ problem)
            )

        Err problem ->
            ( { db = Err "The server's flags did not decode."
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

        GotInterop (Err _) ->
            ( model, NoEffect )



-- EFFECTS


type Effect
    = NoEffect
    | Batch (List Effect)
    | SendToJs Interop.FromElm
    | Database (Neon.Request (Int -> Msg))


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
handle model _ r =
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


error : Int -> String -> Response
error status message =
    json status (Api.encodeError message)
