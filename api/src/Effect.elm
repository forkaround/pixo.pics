module Effect exposing
    ( Body(..)
    , Effect(..)
    , Failure
    , HttpRequest
    , encodeForm
    , log
    , map
    , query
    , respond
    , toTask
    , transact
    )

import Http
import InteropDefinitions as Interop
import Json.Encode as Encode
import Response exposing (Response)
import Services.Neon as Neon
import Task exposing (Task)
import Time
import Url


type Effect msg
    = NoEffect
    | Batch (List (Effect msg))
    | SendToJs Interop.FromElm
    | Database (Neon.Request (Int -> Result Failure msg))
    | Http (HttpRequest msg)
    | Succeed msg
    | Clock (Time.Posix -> msg)


type alias Failure =
    { resolver : Interop.Resolver
    , step : String
    , problem : Neon.Error
    }


type Body
    = JsonBody Encode.Value
    | FormBody (List ( String, String ))
    | EmptyBody


type alias HttpRequest msg =
    { method : String
    , headers : List ( String, String )
    , url : String
    , body : Body
    , timeout : Maybe Float
    , onResponse : Http.Response String -> msg
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

        Http request ->
            Http
                { method = request.method
                , headers = request.headers
                , url = request.url
                , body = request.body
                , timeout = request.timeout
                , onResponse = request.onResponse >> toMsg
                }

        Succeed msg ->
            Succeed (toMsg msg)

        Clock gotTime ->
            Clock (gotTime >> toMsg)


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


toTask : HttpRequest msg -> Task x msg
toTask { method, headers, url, body, timeout, onResponse } =
    Http.task
        { method = method
        , headers = List.map (\( name, value ) -> Http.header name value) headers
        , url = url
        , body = httpBody body
        , resolver = Http.stringResolver (onResponse >> Ok)
        , timeout = timeout
        }


httpBody : Body -> Http.Body
httpBody body =
    case body of
        JsonBody value ->
            Http.jsonBody value

        FormBody pairs ->
            Http.stringBody "application/x-www-form-urlencoded" (encodeForm pairs)

        EmptyBody ->
            Http.emptyBody


encodeForm : List ( String, String ) -> String
encodeForm pairs =
    pairs
        |> List.map (\( name, value ) -> Url.percentEncode name ++ "=" ++ Url.percentEncode value)
        |> String.join "&"
