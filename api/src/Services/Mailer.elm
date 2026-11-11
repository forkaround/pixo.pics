module Services.Mailer exposing (Mailer, configure, send)

import Services.Brevo as Brevo
import Services.Mailer.Layout exposing (Message)


type alias Mailer =
    { client : Brevo.Client
    , from : Brevo.Address
    }


configure : { a | brevoApiKey : Maybe String, emailFrom : Maybe String } -> Result String Mailer
configure config =
    case ( config.brevoApiKey, config.emailFrom ) of
        ( Just apiKey, Just from ) ->
            Ok
                { client = Brevo.client apiKey |> Brevo.withTimeout timeout
                , from = { email = from, name = Just "pixo" }
                }

        _ ->
            Err "BREVO_API_KEY and EMAIL_FROM must both be set."


timeout : Float
timeout =
    10000


send : Mailer -> { to : String, tag : String } -> Message -> (Result Brevo.Error Brevo.Sent -> msg) -> Brevo.Request msg
send mailer { to, tag } email toMsg =
    Brevo.email mailer.client
        { from = mailer.from
        , to = [ { email = to, name = Nothing } ]
        , replyTo = Nothing
        , tags = [ tag ]
        , content = email
        }
        toMsg
