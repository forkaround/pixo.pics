module LogMailerTest exposing (suite)

import Data.Email as Email
import Effect
import Expect
import InteropDefinitions as Interop
import Services.LogMailer as LogMailer
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "LogMailer"
        [ test "sends by logging the tag, the recipient, the subject and the text" <|
            \() ->
                Email.parse "ada@example.com"
                    |> Result.map
                        (\to ->
                            LogMailer.mailer.send
                                { to = to, tag = "sign-in" }
                                { subject = "Sign in to pixo"
                                , html = "<p>Sign in</p>"
                                , text = "Sign in:\nhttps://pixo.pics/auth/verify#Zm9vYmFy\n"
                                }
                        )
                    |> Expect.equal
                        (Ok
                            (Effect.Batch
                                [ Effect.SendToJs
                                    (Interop.Log Interop.Info
                                        "Email not sent, printed instead (sign-in)\nTo: ada@example.com\nSubject: Sign in to pixo\n\nSign in:\nhttps://pixo.pics/auth/verify#Zm9vYmFy\n"
                                    )
                                , Effect.Succeed (Ok ())
                                ]
                            )
                        )
        ]
