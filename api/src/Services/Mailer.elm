module Services.Mailer exposing (Envelope, Mailer, Outcome, send)

import Data.Email exposing (Email)
import Effect exposing (Effect)
import Services.Mailer.Layout exposing (Message)


type alias Mailer =
    { send : Envelope -> Message -> Effect Outcome
    }


type alias Envelope =
    { to : Email
    , tag : String
    }


type alias Outcome =
    Result String ()


send : Mailer -> Envelope -> Message -> (Outcome -> msg) -> Effect msg
send mailer envelope message toMsg =
    Effect.map toMsg (mailer.send envelope message)
