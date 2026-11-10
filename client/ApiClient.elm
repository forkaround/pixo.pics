module ApiClient exposing (Request, request, send, withToken)

import Api exposing (Endpoint)
import Http
import Json.Decode as Decode
import Json.Encode as Encode


type alias Request msg =
    { method : String
    , headers : List ( String, String )
    , url : String
    , body : Maybe Encode.Value
    , onResponse : Http.Response String -> msg
    }


request : Endpoint body response -> body -> (Result String response -> msg) -> Request msg
request endpoint body toMsg =
    let
        route =
            Api.route endpoint
    in
    { method = Api.methodToString (Api.method route)
    , headers = []
    , url = Api.toString route
    , body = Api.encodeBody endpoint body
    , onResponse = toMsg << fromResponse (Api.responseDecoder endpoint)
    }


withToken : String -> Request msg -> Request msg
withToken token request_ =
    { request_ | headers = ( "authorization", "Bearer " ++ token ) :: request_.headers }


send : Request msg -> Cmd msg
send { method, headers, url, body, onResponse } =
    Http.request
        { method = method
        , headers = List.map (\( name, value ) -> Http.header name value) headers
        , url = url
        , body =
            case body of
                Just value ->
                    Http.jsonBody value

                Nothing ->
                    Http.emptyBody
        , expect = Http.expectStringResponse merge (onResponse >> Ok)
        , timeout = Nothing
        , tracker = Nothing
        }


merge : Result a a -> a
merge result =
    case result of
        Ok value ->
            value

        Err value ->
            value


fromResponse : Decode.Decoder a -> Http.Response String -> Result String a
fromResponse decoder response =
    case response of
        Http.BadUrl_ url ->
            Err ("Bad URL: " ++ url)

        Http.Timeout_ ->
            Err "The server took too long to answer."

        Http.NetworkError_ ->
            Err "Could not reach the server. Start both apps with `bun dev`."

        Http.BadStatus_ metadata body ->
            case Decode.decodeString Api.errorDecoder body of
                Ok message ->
                    Err message

                Err _ ->
                    Decode.decodeString decoder body
                        |> Result.mapError
                            (\_ ->
                                "The server answered with status "
                                    ++ String.fromInt metadata.statusCode
                                    ++ ". Is it running? Start both apps with `bun dev`."
                            )

        Http.GoodStatus_ _ body ->
            Decode.decodeString decoder body
                |> Result.mapError (\error -> "Unexpected response: " ++ Decode.errorToString error)
