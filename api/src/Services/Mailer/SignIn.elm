module Services.Mailer.SignIn exposing (email)

import Services.Mailer.Layout as Layout exposing (Message)


email : { url : String, expiresInMinutes : Int } -> Message
email { url, expiresInMinutes } =
    Layout.render
        { subject = "Sign in to pixo"
        , preview = "Your sign-in link, good for " ++ Layout.minutes expiresInMinutes ++ "."
        , blocks =
            [ Layout.Heading "Sign in to pixo"
            , Layout.Paragraph
                ("Use this link to sign in. It works once and expires in "
                    ++ Layout.minutes expiresInMinutes
                    ++ "."
                )
            , Layout.Button { label = "Sign in", url = url }
            , Layout.Footnote "If you didn't ask for this link, ignore this email: nobody can sign in without it."
            ]
        }
