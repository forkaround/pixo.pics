module Services.Mailer.PasskeyRemoved exposing (email)

import Data.Username as Username exposing (Username)
import Services.Mailer.Layout as Layout exposing (Message)


email : { url : String, username : Username, passkey : Maybe String } -> Message
email { url, username, passkey } =
    let
        account =
            Username.toString username
    in
    Layout.render
        { subject = "A passkey was removed from your pixo account"
        , preview = "It can no longer sign in to " ++ account ++ "."
        , blocks =
            [ Layout.Heading "A passkey was removed"
            , Layout.Paragraph
                ((case passkey of
                    Just name ->
                        "A passkey from " ++ name

                    Nothing ->
                        "A passkey"
                 )
                    ++ " was removed from the account "
                    ++ account
                    ++ ". It can no longer sign in."
                )
            , Layout.Button { label = "Review your passkeys", url = url }
            , Layout.Footnote "If this wasn't you, review the passkeys left in your account settings. You can always sign in with an emailed link."
            ]
        }
