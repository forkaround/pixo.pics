module Services.Mailer.SignUp exposing (email)

import Data.Username as Username exposing (Username)
import Services.Mailer.Layout as Layout exposing (Message)


email : { url : String, username : Username, expiresInMinutes : Int } -> Message
email { url, username, expiresInMinutes } =
    let
        name =
            Username.toString username
    in
    Layout.render
        { subject = "Confirm your email for pixo"
        , preview = "One click to create your account, " ++ name ++ "."
        , blocks =
            [ Layout.Heading "Welcome to pixo"
            , Layout.Paragraph
                ("Confirm this email address to create the account "
                    ++ name
                    ++ ". The link works once and expires in "
                    ++ Layout.minutes expiresInMinutes
                    ++ "."
                )
            , Layout.Button { label = "Create my account", url = url }
            , Layout.Footnote "If you didn't sign up, ignore this email: no account is created without this link."
            ]
        }
