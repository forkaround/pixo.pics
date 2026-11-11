module Services.Mailer exposing (Mailer, configure, send)

import Data.Email as Email exposing (Email)
import Services.Brevo as Brevo
import Services.Mailer.Layout exposing (Message)


type Mailer
    = Mailer
        { client : Brevo.Client
        , from : Email
        }


configure : { a | brevoApiKey : Maybe String, emailFrom : Maybe String } -> Result String Mailer
configure config =
    case ( config.brevoApiKey, config.emailFrom ) of
        ( Just apiKey, Just from ) ->
            Email.parse from
                |> Result.mapError (\_ -> "EMAIL_FROM is not an email address.")
                |> Result.map
                    (\sender ->
                        Mailer
                            { client = Brevo.client apiKey |> Brevo.withTimeout timeout
                            , from = sender
                            }
                    )

        _ ->
            Err "BREVO_API_KEY and EMAIL_FROM must both be set."


timeout : Float
timeout =
    10000


send : Mailer -> { to : Email, tag : String } -> Message -> (Result Brevo.Error Brevo.Sent -> msg) -> Brevo.Request msg
send (Mailer mailer) { to, tag } message toMsg =
    Brevo.email mailer.client
        { from = { email = Email.toString mailer.from, name = Just "pixo" }
        , to = [ { email = Email.toString to, name = Nothing } ]
        , replyTo = Nothing
        , tags = [ tag ]
        , content = message
        }
        toMsg
