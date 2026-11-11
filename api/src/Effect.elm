module Effect exposing (Effect(..), Failure, log, map, query, respond, transact)

import InteropDefinitions as Interop
import Response exposing (Response)
import Services.Brevo as Brevo
import Services.Neon as Neon


type Effect msg
    = NoEffect
    | Batch (List (Effect msg))
    | SendToJs Interop.FromElm
    | Database (Neon.Request (Int -> Result Failure msg))
    | Mail (Brevo.Request msg)


type alias Failure =
    { resolver : Interop.Resolver
    , step : String
    , problem : Neon.Error
    }


map : (a -> b) -> Effect a -> Effect b
map toMsg effect =
    case effect of
        NoEffect ->
            NoEffect

        Batch effects ->
            Batch (List.map (map toMsg) effects)

        SendToJs value ->
            SendToJs value

        Database request ->
            Database
                { method = request.method
                , headers = request.headers
                , url = request.url
                , body = request.body
                , timeout = request.timeout
                , onResponse = \response latency -> Result.map toMsg (request.onResponse response latency)
                }

        Mail request ->
            Mail
                { method = request.method
                , headers = request.headers
                , url = request.url
                , body = request.body
                , timeout = request.timeout
                , onResponse = request.onResponse >> toMsg
                }


query : Neon.Connection -> Interop.Resolver -> String -> Neon.Query a -> (a -> msg) -> Effect msg
query db resolver step wanted toMsg =
    Database (Neon.run db wanted (\result _ -> answered resolver step toMsg result))


transact : Neon.Connection -> Interop.Resolver -> String -> Neon.Transaction a -> (a -> msg) -> Effect msg
transact db resolver step wanted toMsg =
    Database (Neon.commit db wanted (\result _ -> answered resolver step toMsg result))


answered : Interop.Resolver -> String -> (a -> msg) -> Result Neon.Error a -> Result Failure msg
answered resolver step toMsg result =
    case result of
        Ok value ->
            Ok (toMsg value)

        Err problem ->
            Err { resolver = resolver, step = step, problem = problem }


respond : Interop.Resolver -> Response -> Effect msg
respond resolver response =
    SendToJs (Interop.Respond resolver response)


log : Interop.Level -> String -> Effect msg
log level message =
    SendToJs (Interop.Log level message)
