module Services.BrevoMailer exposing (mailer)

import Data.Email as Email exposing (Email)
import Effect exposing (Effect)
import Services.Brevo as Brevo
import Services.Mailer as Mailer exposing (Mailer)
import Services.Mailer.Layout exposing (Message)


mailer : { apiKey : String, from : Email } -> Mailer
mailer { apiKey, from } =
    { send = send (Brevo.client apiKey |> Brevo.withTimeout timeout) from
    }


send : Brevo.Client -> Email -> Mailer.Envelope -> Message -> Effect Mailer.Outcome
send client from { to, tag } message =
    Effect.Http
        (Brevo.email client
            { from = { email = Email.toString from, name = Just "pixo" }
            , to = [ { email = Email.toString to, name = Nothing } ]
            , replyTo = Nothing
            , tags = [ tag ]
            , content = message
            }
            outcome
        )


outcome : Result Brevo.Error Brevo.Sent -> Mailer.Outcome
outcome result =
    case result of
        Ok _ ->
            Ok ()

        Err problem ->
            Err (Brevo.errorToString problem)


timeout : Float
timeout =
    10000
