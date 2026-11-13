module Effect exposing (Effect(..), map)

import ApiClient
import InteropDefinitions as Interop
import Time


type Effect msg
    = NoEffect
    | Batch (List (Effect msg))
    | Request (ApiClient.Request msg)
    | SendToJs Interop.FromElm
    | PushUrl String
    | Load String
    | ReadTime (Time.Posix -> msg)
    | ScrollTo String


map : (a -> b) -> Effect a -> Effect b
map toMsg effect =
    case effect of
        NoEffect ->
            NoEffect

        Batch effects ->
            Batch (List.map (map toMsg) effects)

        Request request ->
            Request
                { method = request.method
                , headers = request.headers
                , url = request.url
                , body = request.body
                , onResponse = request.onResponse >> toMsg
                }

        SendToJs value ->
            SendToJs value

        PushUrl url ->
            PushUrl url

        Load url ->
            Load url

        ReadTime toTime ->
            ReadTime (toTime >> toMsg)

        ScrollTo target ->
            ScrollTo target
