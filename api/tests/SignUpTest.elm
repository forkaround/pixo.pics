module SignUpTest exposing (suite)

import Data.Email as Email
import Data.SignUp as SignUp
import Data.Username as Username
import Expect
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "SignUp"
        [ describe "parse"
            [ test "trims and lower-cases the email and the username" <|
                \() ->
                    parsed { email = " Ada@Example.COM ", username = " Ada_99 " }
                        |> Expect.equal (Ok { email = "ada@example.com", username = "ada_99" })
            , describe "refuses an email"
                (List.map
                    (\email ->
                        test ("like " ++ Debug.toString email) <|
                            \() ->
                                parsed { email = email, username = "ada" }
                                    |> Expect.equal (Err "Enter a valid email address.")
                    )
                    [ ""
                    , "ada"
                    , "@example.com"
                    , "ada@example"
                    , "ada@.com"
                    , "ada@example."
                    , "ada@@example.com"
                    , "ada lovelace@example.com"
                    , "ada@example.com\nbcc: eve@example.com"
                    , "Ada <ada@example.com>"
                    , String.repeat 250 "a" ++ "@example.com"
                    ]
                )
            , describe "refuses a username"
                (List.map
                    (\username ->
                        test ("like " ++ Debug.toString username) <|
                            \() ->
                                parsed { email = "ada@example.com", username = username }
                                    |> Expect.equal (Err "A username has 3 to 20 characters: letters, digits and underscores.")
                    )
                    [ ""
                    , "ad"
                    , String.repeat 21 "a"
                    , "ada lovelace"
                    , "ada-lovelace"
                    , "ada.l"
                    , "adá"
                    , "<b>ada</b>"
                    ]
                )
            ]
        ]


parsed : { email : String, username : String } -> Result String { email : String, username : String }
parsed =
    SignUp.parse
        >> Result.map
            (\signUp ->
                { email = Email.toString signUp.email
                , username = Username.toString signUp.username
                }
            )
