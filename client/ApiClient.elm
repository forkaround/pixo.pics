module ApiClient exposing (Error(..), Request, errorToString, request, send, withToken)

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


type Error
    = Unauthorized String
    | Failed String


errorToString : Error -> String
errorToString error =
    case error of
        Unauthorized message ->
            message

        Failed message ->
            message


request : Endpoint body response -> body -> (Result Error response -> msg) -> Request msg
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


fromResponse : Decode.Decoder a -> Http.Response String -> Result Error a
fromResponse decoder response =
    case response of
        Http.BadUrl_ url ->
            Err (Failed ("Bad URL: " ++ url))

        Http.Timeout_ ->
            Err (Failed "The server took too long to answer.")

        Http.NetworkError_ ->
            Err (Failed "Could not reach the server. Start both apps with `bun dev`.")

        Http.BadStatus_ metadata body ->
            case Decode.decodeString Api.errorDecoder body of
                Ok message ->
                    Err (refusal metadata.statusCode message)

                Err _ ->
                    Decode.decodeString decoder body
                        |> Result.mapError
                            (\_ ->
                                refusal metadata.statusCode
                                    ("The server answered with status "
                                        ++ String.fromInt metadata.statusCode
                                        ++ ". Is it running? Start both apps with `bun dev`."
                                    )
                            )

        Http.GoodStatus_ _ body ->
            Decode.decodeString decoder body
                |> Result.mapError (\error -> Failed ("Unexpected response: " ++ Decode.errorToString error))


refusal : Int -> String -> Error
refusal status message =
    if status == 401 then
        Unauthorized message

    else
        Failed message
