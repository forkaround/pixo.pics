module Services.LogMailer exposing (mailer)

import Data.Email as Email
import Effect exposing (Effect)
import InteropDefinitions as Interop
import Services.Mailer as Mailer exposing (Mailer)
import Services.Mailer.Layout exposing (Message)


mailer : Mailer
mailer =
    { send = send
    }


send : Mailer.Envelope -> Message -> Effect Mailer.Outcome
send { to, tag } message =
    Effect.Batch
        [ Effect.log Interop.Info
            (String.join "\n"
                [ "Email not sent, printed instead (" ++ tag ++ ")"
                , "To: " ++ Email.toString to
                , "Subject: " ++ message.subject
                , ""
                , message.text
                ]
            )
        , Effect.Succeed (Ok ())
        ]
