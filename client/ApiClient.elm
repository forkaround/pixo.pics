module ApiClient exposing (Error(..), Request, errorToString, request, send, withToken)

import Api exposing (Endpoint)
import Data.LinkToken as LinkToken exposing (LinkToken)
import Dict exposing (Dict)
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
    | Throttled Int String
    | Refused String
    | Failed String


errorToString : Error -> String
errorToString error =
    case error of
        Unauthorized message ->
            message

        Throttled _ message ->
            message

        Refused message ->
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


withToken : LinkToken -> Request msg -> Request msg
withToken token request_ =
    { request_ | headers = ( "authorization", "Bearer " ++ LinkToken.toString token ) :: request_.headers }


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
            Err (Failed ("Invalid request URL: " ++ url))

        Http.Timeout_ ->
            Err (Failed "Server timed out")

        Http.NetworkError_ ->
            Err (Failed "Network offline")

        Http.BadStatus_ metadata body ->
            case Decode.decodeString Api.errorDecoder body of
                Ok message ->
                    Err (refusal metadata message)

                Err _ ->
                    Decode.decodeString decoder body
                        |> Result.mapError
                            (\_ ->
                                refusal metadata ("Server error HTTP " ++ String.fromInt metadata.statusCode)
                            )

        Http.GoodStatus_ _ body ->
            Decode.decodeString decoder body
                |> Result.mapError (\_ -> Failed "Unexpected server response")


refusal : Http.Metadata -> String -> Error
refusal metadata message =
    if metadata.statusCode == 401 then
        Unauthorized message

    else if metadata.statusCode == 429 then
        case retryAfter metadata.headers of
            Just seconds ->
                Throttled seconds message

            Nothing ->
                Failed message

    else if metadata.statusCode >= 400 && metadata.statusCode < 500 then
        Refused message

    else
        Failed message


retryAfter : Dict String String -> Maybe Int
retryAfter headers =
    Dict.toList headers
        |> List.filter (\( name, _ ) -> String.toLower name == "retry-after")
        |> List.head
        |> Maybe.andThen (\( _, value ) -> String.toInt (String.trim value))
        |> Maybe.map (max 0)
