module Services.Mailer.PasskeyAdded exposing (email)

import Services.Mailer.Layout as Layout exposing (Message)


email : { url : String, username : String, passkey : Maybe String } -> Message
email { url, username, passkey } =
    Layout.render
        { subject = "A passkey was added to your pixo account"
        , preview = "You can now sign in to " ++ username ++ " with it."
        , blocks =
            [ Layout.Heading "A passkey was added"
            , Layout.Paragraph
                ((case passkey of
                    Just name ->
                        "A passkey from " ++ name

                    Nothing ->
                        "A passkey"
                 )
                    ++ " was added to the account "
                    ++ username
                    ++ ". It can now sign in without an emailed link."
                )
            , Layout.Button { label = "Review your passkeys", url = url }
            , Layout.Footnote "If this wasn't you, remove the passkey from your dashboard: whoever added it can sign in to your account."
            ]
        }
