module Services.BrevoMailer exposing (endpoint, mailer)

import Data.Email as Email exposing (Email)
import Effect exposing (Effect)
import Http
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Services.Mailer as Mailer exposing (Mailer)
import Services.Mailer.Layout exposing (Message)


mailer : { apiKey : String, from : Email } -> Mailer
mailer config =
    { send = send config
    }


endpoint : String
endpoint =
    "https://api.brevo.com/v3/smtp/email"


timeout : Float
timeout =
    10000


send : { apiKey : String, from : Email } -> Mailer.Envelope -> Message -> Effect Mailer.Outcome
send { apiKey, from } { to, tag } message =
    Effect.Http
        { method = "POST"
        , headers =
            [ ( "api-key", apiKey )
            , ( "accept", "application/json" )
            ]
        , url = endpoint
        , body =
            Encode.object
                [ ( "sender"
                  , Encode.object
                        [ ( "email", Encode.string (Email.toString from) )
                        , ( "name", Encode.string "pixo" )
                        ]
                  )
                , ( "to", Encode.list (\address -> Encode.object [ ( "email", Encode.string (Email.toString address) ) ]) [ to ] )
                , ( "subject", Encode.string message.subject )
                , ( "htmlContent", Encode.string message.html )
                , ( "textContent", Encode.string message.text )
                , ( "tags", Encode.list Encode.string [ tag ] )
                ]
        , timeout = Just timeout
        , onResponse = outcome
        }


outcome : Http.Response String -> Mailer.Outcome
outcome response =
    case response of
        Http.GoodStatus_ _ _ ->
            Ok ()

        Http.BadStatus_ metadata body ->
            Decode.decodeString (rejection metadata.statusCode) body
                |> Result.withDefault ("Brevo answered with HTTP " ++ String.fromInt metadata.statusCode ++ ": " ++ body)
                |> Err

        Http.NetworkError_ ->
            Err "Could not reach Brevo."

        Http.Timeout_ ->
            Err "Brevo took too long to answer."

        Http.BadUrl_ url ->
            Err ("Bad Brevo endpoint: " ++ url)


rejection : Int -> Decoder String
rejection status =
    Decode.map2 (\code reason -> "Brevo refused the email with HTTP " ++ String.fromInt status ++ " (" ++ code ++ "): " ++ reason)
        (Decode.field "code" Decode.string)
        (Decode.field "message" Decode.string)
