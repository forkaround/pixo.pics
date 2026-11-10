module Email.SignUp exposing (email)

import Email.Layout as Layout exposing (Email)


email : { url : String, username : String, expiresInMinutes : Int } -> Email
email { url, username, expiresInMinutes } =
    Layout.render
        { subject = "Confirm your email for pixo"
        , preview = "One click to create your account, " ++ username ++ "."
        , blocks =
            [ Layout.Heading "Welcome to pixo"
            , Layout.Paragraph
                ("Confirm this email address to create the account "
                    ++ username
                    ++ ". The link works once and expires in "
                    ++ Layout.minutes expiresInMinutes
                    ++ "."
                )
            , Layout.Button { label = "Create my account", url = url }
            , Layout.Footnote "If you didn't sign up, ignore this email: no account is created without this link."
            ]
        }
