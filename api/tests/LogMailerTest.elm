module LogMailerTest exposing (suite)

import Expect
import Services.LogMailer as LogMailer
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "LogMailer"
        [ test "writes the tag, the recipient, the subject and the text" <|
            \() ->
                LogMailer.email
                    { to = "ada@example.com"
                    , tag = "sign-in"
                    , subject = "Sign in to pixo"
                    , text = "Sign in:\nhttps://pixo.pics/auth/verify#Zm9vYmFy\n"
                    }
                    |> Expect.equal "Email not sent, printed instead (sign-in)\nTo: ada@example.com\nSubject: Sign in to pixo\n\nSign in:\nhttps://pixo.pics/auth/verify#Zm9vYmFy\n"
        ]
