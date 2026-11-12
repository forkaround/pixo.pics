module MailerTest exposing (suite)

import Data.Email as Email
import Data.Username as Username
import Effect
import Expect exposing (Expectation)
import Services.Mailer as Mailer
import Services.Mailer.Layout as Layout
import Services.Mailer.PasskeyAdded
import Services.Mailer.PasskeyRemoved
import Services.Mailer.SignIn
import Services.Mailer.SignUp
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "Mailer"
        [ describe "send"
            [ test "hands the mailer's outcome to the caller's message" <|
                \() ->
                    case Email.parse "ada@example.com" of
                        Ok to ->
                            Mailer.send
                                { send = \_ _ -> Effect.Succeed (Err "No luck.") }
                                { to = to, tag = "sign-in" }
                                (Services.Mailer.SignIn.email { url = "https://pixo.pics/sign-in?token=abc", expiresInMinutes = 15 })
                                (Tuple.pair "told")
                                |> Expect.equal (Effect.Succeed ( "told", Err "No luck." ))

                        Err problem ->
                            Expect.fail problem
            ]
        , describe "Layout"
            [ test "writes the text version from the same blocks" <|
                \() ->
                    Layout.render
                        { subject = "Hello"
                        , preview = "A preview"
                        , blocks =
                            [ Layout.Heading "Hello"
                            , Layout.Paragraph "Open the link."
                            , Layout.Button { label = "Open", url = "https://pixo.pics/a?b=1&c=2" }
                            , Layout.Footnote "Bye."
                            ]
                        }
                        |> .text
                        |> Expect.equal "Hello\n\nOpen the link.\n\nOpen:\nhttps://pixo.pics/a?b=1&c=2\n\nBye.\n"
            , test "escapes text in the HTML" <|
                \() ->
                    Layout.render
                        { subject = "Tom & Jerry"
                        , preview = "1 < 2"
                        , blocks = [ Layout.Paragraph "<script>alert('x')</script>" ]
                        }
                        |> .html
                        |> Expect.all
                            [ contains "<title>Tom &amp; Jerry</title>"
                            , contains ">1 &lt; 2</div>"
                            , contains "&lt;script&gt;alert(&#39;x&#39;)&lt;/script&gt;"
                            , excludes "<script>"
                            ]
            , test "escapes a link so it can't leave its attribute" <|
                \() ->
                    Layout.render
                        { subject = "Hello"
                        , preview = ""
                        , blocks = [ Layout.Button { label = "Open", url = "https://pixo.pics/a?b=1&c=\"><b>" } ]
                        }
                        |> .html
                        |> Expect.all
                            [ contains "href=\"https://pixo.pics/a?b=1&amp;c=&quot;&gt;&lt;b&gt;\""
                            , excludes "<b>"
                            ]
            , test "keeps the subject as it is, since it is not HTML" <|
                \() ->
                    Layout.render { subject = "Tom & Jerry", preview = "", blocks = [] }
                        |> .subject
                        |> Expect.equal "Tom & Jerry"
            , test "counts one minute in the singular" <|
                \() ->
                    ( Layout.minutes 1, Layout.minutes 15 )
                        |> Expect.equal ( "1 minute", "15 minutes" )
            ]
        , describe "SignIn"
            [ test "carries the link in the HTML and in the text" <|
                \() ->
                    Services.Mailer.SignIn.email { url = "https://pixo.pics/sign-in?token=abc", expiresInMinutes = 15 }
                        |> Expect.all
                            [ .subject >> Expect.equal "Sign in to pixo"
                            , .html >> contains "href=\"https://pixo.pics/sign-in?token=abc\""
                            , .text >> contains "Sign in:\nhttps://pixo.pics/sign-in?token=abc"
                            , .text >> contains "expires in 15 minutes."
                            ]
            ]
        , describe "PasskeyAdded"
            [ test "names the passkey and the account, and links to the account" <|
                \() ->
                    withUsername "ada"
                        (\username ->
                            Services.Mailer.PasskeyAdded.email
                                { url = "https://pixo.pics/settings/account", username = username, passkey = Just "Apple Passwords" }
                                |> Expect.all
                                    [ .html >> String.contains "A passkey from Apple Passwords was added to the account ada." >> Expect.equal True
                                    , .text >> String.contains "A passkey from Apple Passwords was added to the account ada." >> Expect.equal True
                                    , .html >> String.contains "https://pixo.pics/settings/account" >> Expect.equal True
                                    , .text >> String.contains "https://pixo.pics/settings/account" >> Expect.equal True
                                    ]
                        )
            , test "says just a passkey when nothing names it" <|
                \() ->
                    withUsername "ada"
                        (\username ->
                            Services.Mailer.PasskeyAdded.email { url = "https://pixo.pics/settings/account", username = username, passkey = Nothing }
                                |> .text
                                |> String.contains "A passkey was added to the account ada."
                                |> Expect.equal True
                        )
            ]
        , describe "PasskeyRemoved"
            [ test "names the passkey and the account, and links to the account" <|
                \() ->
                    withUsername "ada"
                        (\username ->
                            Services.Mailer.PasskeyRemoved.email
                                { url = "https://pixo.pics/settings/account", username = username, passkey = Just "Apple Passwords" }
                                |> Expect.all
                                    [ .html >> String.contains "A passkey from Apple Passwords was removed from the account ada." >> Expect.equal True
                                    , .text >> String.contains "A passkey from Apple Passwords was removed from the account ada." >> Expect.equal True
                                    , .html >> String.contains "https://pixo.pics/settings/account" >> Expect.equal True
                                    , .text >> String.contains "https://pixo.pics/settings/account" >> Expect.equal True
                                    ]
                        )
            ]
        , describe "SignUp"
            [ test "names the account and carries the link" <|
                \() ->
                    withUsername "ada"
                        (\username ->
                            Services.Mailer.SignUp.email
                                { url = "https://pixo.pics/sign-up?token=abc"
                                , username = username
                                , expiresInMinutes = 15
                                }
                                |> Expect.all
                                    [ .subject >> Expect.equal "Confirm your email for pixo"
                                    , .html >> contains "href=\"https://pixo.pics/sign-up?token=abc\""
                                    , .text >> contains "create the account ada."
                                    , .text >> contains "Create my account:\nhttps://pixo.pics/sign-up?token=abc"
                                    ]
                        )
            ]
        ]


withUsername : String -> (Username.Username -> Expectation) -> Expectation
withUsername value toExpectation =
    case Username.parse value of
        Ok username ->
            toExpectation username

        Err problem ->
            Expect.fail problem


contains : String -> String -> Expect.Expectation
contains part whole =
    if String.contains part whole then
        Expect.pass

    else
        Expect.fail ("Expected to find " ++ part ++ " in:\n" ++ whole)


excludes : String -> String -> Expect.Expectation
excludes part whole =
    if String.contains part whole then
        Expect.fail ("Expected not to find " ++ part ++ " in:\n" ++ whole)

    else
        Expect.pass
