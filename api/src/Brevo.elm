module Brevo exposing
    ( Address
    , Client
    , Content
    , Error(..)
    , Message
    , Request
    , Sent
    , client
    , email
    , endpoint
    , errorToString
    , send
    , withTimeout
    )

import Http
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Task exposing (Task)


type Client
    = Client
        { apiKey : String
        , timeout : Maybe Float
        }


client : String -> Client
client apiKey =
    Client { apiKey = apiKey, timeout = Nothing }


withTimeout : Float -> Client -> Client
withTimeout milliseconds (Client c) =
    Client { c | timeout = Just milliseconds }


endpoint : String
endpoint =
    "https://api.brevo.com/v3/smtp/email"


type alias Message =
    { from : Address
    , to : List Address
    , replyTo : Maybe Address
    , tags : List String
    , content : Content
    }


type alias Address =
    { email : String
    , name : Maybe String
    }


type alias Content =
    { subject : String
    , html : String
    , text : String
    }


type alias Sent =
    { messageId : Maybe String
    }


email : Client -> Message -> (Result Error Sent -> msg) -> Request msg
email (Client c) message toMsg =
    { method = "POST"
    , headers =
        [ ( "api-key", c.apiKey )
        , ( "accept", "application/json" )
        ]
    , url = endpoint
    , body = encodeMessage message
    , timeout = c.timeout
    , onResponse = toMsg << resolve
    }


encodeMessage : Message -> Encode.Value
encodeMessage message =
    Encode.object
        (List.concat
            [ [ ( "sender", encodeAddress message.from )
              , ( "to", Encode.list encodeAddress message.to )
              ]
            , message.replyTo
                |> Maybe.map (\address -> [ ( "replyTo", encodeAddress address ) ])
                |> Maybe.withDefault []
            , [ ( "subject", Encode.string message.content.subject )
              , ( "htmlContent", Encode.string message.content.html )
              , ( "textContent", Encode.string message.content.text )
              ]
            , if List.isEmpty message.tags then
                []

              else
                [ ( "tags", Encode.list Encode.string message.tags ) ]
            ]
        )


encodeAddress : Address -> Encode.Value
encodeAddress address =
    Encode.object
        (( "email", Encode.string address.email )
            :: (address.name
                    |> Maybe.map (\name -> [ ( "name", Encode.string name ) ])
                    |> Maybe.withDefault []
               )
        )


sentDecoder : Decoder Sent
sentDecoder =
    Decode.map Sent
        (Decode.maybe (Decode.field "messageId" Decode.string))


type Error
    = Rejected { status : Int, code : String, message : String }
    | ServerError Int String
    | NetworkError
    | Timeout
    | BadUrl String


errorToString : Error -> String
errorToString error =
    case error of
        Rejected { status, code, message } ->
            "Brevo refused the email with HTTP " ++ String.fromInt status ++ " (" ++ code ++ "): " ++ message

        ServerError status body ->
            "Brevo answered with HTTP " ++ String.fromInt status ++ ": " ++ body

        NetworkError ->
            "Could not reach Brevo."

        Timeout ->
            "Brevo took too long to answer."

        BadUrl url ->
            "Bad Brevo endpoint: " ++ url


rejectionDecoder : Int -> Decoder Error
rejectionDecoder status =
    Decode.map2 (\code message -> Rejected { status = status, code = code, message = message })
        (Decode.field "code" Decode.string)
        (Decode.field "message" Decode.string)


type alias Request msg =
    { method : String
    , headers : List ( String, String )
    , url : String
    , body : Encode.Value
    , timeout : Maybe Float
    , onResponse : Http.Response String -> msg
    }


send : Request msg -> Task x msg
send { method, headers, url, body, timeout, onResponse } =
    Http.task
        { method = method
        , headers = List.map (\( name, value ) -> Http.header name value) headers
        , url = url
        , body = Http.jsonBody body
        , resolver = Http.stringResolver (onResponse >> Ok)
        , timeout = timeout
        }


resolve : Http.Response String -> Result Error Sent
resolve response =
    case response of
        Http.GoodStatus_ _ body ->
            Decode.decodeString sentDecoder body
                |> Result.withDefault { messageId = Nothing }
                |> Ok

        Http.BadStatus_ metadata body ->
            Decode.decodeString (rejectionDecoder metadata.statusCode) body
                |> Result.withDefault (ServerError metadata.statusCode body)
                |> Err

        Http.NetworkError_ ->
            Err NetworkError

        Http.Timeout_ ->
            Err Timeout

        Http.BadUrl_ url ->
            Err (BadUrl url)
