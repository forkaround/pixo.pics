module Effect exposing (Effect(..), failure, map)

import ApiClient
import InteropDefinitions as Interop
import Notice exposing (Notice)
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
    | Notify (Notice msg)
    | Dismiss String
    | Sleep Float msg
    | Toaster (Cmd msg)


failure : { id : String, title : String, retry : Maybe msg } -> ApiClient.Error -> Effect msg
failure details error =
    let
        notice =
            Notice.failure details error
    in
    case notice.retry of
        Notice.RetryIn seconds again ->
            Batch [ Notify notice, Sleep (toFloat (seconds * 1000)) again ]

        _ ->
            Notify notice


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

        Notify notice ->
            Notify (Notice.map toMsg notice)

        Dismiss id ->
            Dismiss id

        Sleep milliseconds msg ->
            Sleep milliseconds (toMsg msg)

        Toaster cmd ->
            Toaster (Cmd.map toMsg cmd)
