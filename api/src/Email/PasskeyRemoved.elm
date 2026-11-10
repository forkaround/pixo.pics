module Email.PasskeyRemoved exposing (email)

import Email.Layout as Layout exposing (Email)


email : { url : String, username : String, passkey : Maybe String } -> Email
email { url, username, passkey } =
    Layout.render
        { subject = "A passkey was removed from your pixo account"
        , preview = "It can no longer sign in to " ++ username ++ "."
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
                    ++ username
                    ++ ". It can no longer sign in."
                )
            , Layout.Button { label = "Review your passkeys", url = url }
            , Layout.Footnote "If this wasn't you, review the passkeys left on your dashboard. You can always sign in with an emailed link."
            ]
        }
