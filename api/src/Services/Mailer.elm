module Services.Mailer exposing (Delivery(..), Mailer, configure, mapDelivery, send, warning)

import Data.Email as Email exposing (Email)
import Services.Brevo as Brevo
import Services.LogMailer as LogMailer
import Services.Mailer.Layout exposing (Message)


type Mailer
    = BrevoMailer
        { client : Brevo.Client
        , from : Email
        }
    | LogMailer


type Delivery msg
    = Send (Brevo.Request msg)
    | Print String msg


configure : { a | brevoApiKey : Maybe String, emailFrom : Maybe String, local : Bool } -> Result String Mailer
configure config =
    case ( config.brevoApiKey, config.emailFrom ) of
        ( Just apiKey, Just from ) ->
            Email.parse from
                |> Result.mapError (\_ -> "EMAIL_FROM is not an email address.")
                |> Result.map
                    (\sender ->
                        BrevoMailer
                            { client = Brevo.client apiKey |> Brevo.withTimeout timeout
                            , from = sender
                            }
                    )

        _ ->
            if config.local then
                Ok LogMailer

            else
                Err "BREVO_API_KEY and EMAIL_FROM must both be set."


timeout : Float
timeout =
    10000


warning : Mailer -> Maybe String
warning mailer =
    case mailer of
        BrevoMailer _ ->
            Nothing

        LogMailer ->
            Just LogMailer.warning


send : Mailer -> { to : Email, tag : String } -> Message -> (Result Brevo.Error Brevo.Sent -> msg) -> Delivery msg
send mailer { to, tag } message toMsg =
    case mailer of
        BrevoMailer { client, from } ->
            Send
                (Brevo.email client
                    { from = { email = Email.toString from, name = Just "pixo" }
                    , to = [ { email = Email.toString to, name = Nothing } ]
                    , replyTo = Nothing
                    , tags = [ tag ]
                    , content = message
                    }
                    toMsg
                )

        LogMailer ->
            Print
                (LogMailer.email
                    { to = Email.toString to
                    , tag = tag
                    , subject = message.subject
                    , text = message.text
                    }
                )
                (toMsg (Ok { messageId = Nothing }))


mapDelivery : (a -> b) -> Delivery a -> Delivery b
mapDelivery toMsg delivery =
    case delivery of
        Send request ->
            Send
                { method = request.method
                , headers = request.headers
                , url = request.url
                , body = request.body
                , timeout = request.timeout
                , onResponse = request.onResponse >> toMsg
                }

        Print text msg ->
            Print text (toMsg msg)
