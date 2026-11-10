module ApiTest exposing (suite)

import Api
import Expect
import Json.Decode as Decode
import Json.Encode as Encode
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "Api"
        [ describe "parse"
            (List.map
                (\route ->
                    test ("finds " ++ Api.methodToString (Api.method route) ++ " " ++ Api.toString route) <|
                        \() ->
                            Api.parse (Api.methodToString (Api.method route)) (Api.path route)
                                |> Expect.equal (Ok route)
                )
                routes
                ++ [ test "names the methods a path does allow" <|
                        \() ->
                            Api.parse "POST" [ "api", "health" ]
                                |> Expect.equal (Err (Api.MethodNotAllowed [ Api.Get ]))
                   , test "names both methods of a path that has two" <|
                        \() ->
                            Api.parse "DELETE" [ "api", "auth", "verify" ]
                                |> Expect.equal (Err (Api.MethodNotAllowed [ Api.Get, Api.Post ]))
                   , test "finds nothing at an unknown path" <|
                        \() ->
                            Api.parse "GET" [ "api", "nowhere" ]
                                |> Expect.equal (Err Api.NotFound)
                   ]
            )
        , describe "signUp"
            [ test "sends the email and the username as JSON" <|
                \() ->
                    Api.encodeBody Api.signUp { email = "ada@example.com", username = "ada" }
                        |> Maybe.map (Encode.encode 0)
                        |> Expect.equal (Just """{"email":"ada@example.com","username":"ada"}""")
            , test "decodes the body it encodes" <|
                \() ->
                    Decode.decodeString (Api.bodyDecoder Api.signUp) """{"email":"ada@example.com","username":"ada"}"""
                        |> Expect.equal (Ok { email = "ada@example.com", username = "ada" })
            , test "answers with an empty object" <|
                \() ->
                    Api.encodeResponse Api.signUp ()
                        |> Encode.encode 0
                        |> Expect.equal "{}"
            ]
        , describe "signIn"
            [ test "sends the email as JSON" <|
                \() ->
                    Api.encodeBody Api.signIn { email = "ada@example.com" }
                        |> Maybe.map (Encode.encode 0)
                        |> Expect.equal (Just """{"email":"ada@example.com"}""")
            , test "decodes the body it encodes" <|
                \() ->
                    Decode.decodeString (Api.bodyDecoder Api.signIn) """{"email":"ada@example.com"}"""
                        |> Expect.equal (Ok { email = "ada@example.com" })
            ]
        , describe "session"
            [ test "decodes what it encodes" <|
                \() ->
                    Api.encodeResponse Api.session { email = "ada@example.com", username = "ada" }
                        |> Decode.decodeValue (Api.responseDecoder Api.session)
                        |> Expect.equal (Ok { email = "ada@example.com", username = "ada" })
            ]
        , describe "verification"
            [ test "decodes what it encodes" <|
                \() ->
                    Api.encodeResponse Api.verification { email = "ada@example.com", username = "ada", newAccount = True }
                        |> Decode.decodeValue (Api.responseDecoder Api.verification)
                        |> Expect.equal (Ok { email = "ada@example.com", username = "ada", newAccount = True })
            ]
        , describe "verify"
            [ test "decodes what it encodes" <|
                \() ->
                    Api.encodeResponse Api.verify { email = "ada@example.com", username = "ada" }
                        |> Decode.decodeValue (Api.responseDecoder Api.verify)
                        |> Expect.equal (Ok { email = "ada@example.com", username = "ada" })
            ]
        , describe "health"
            [ test "decodes what it encodes" <|
                \() ->
                    Api.encodeResponse Api.health report
                        |> Decode.decodeValue (Api.responseDecoder Api.health)
                        |> Expect.equal (Ok report)
            ]
        ]


routes : List Api.Route
routes =
    [ Api.GetHealth
    , Api.PostSignUp
    , Api.PostSignIn
    , Api.GetVerification
    , Api.PostVerification
    , Api.GetSession
    , Api.DeleteSession
    ]


report : Api.Health
report =
    Api.healthOf
        { releaseId = Just "064160704a6b52143928d03db8e50fcd5dfe0205"
        , releaseUrl = Just "https://github.com/forkaround/pixo.pics/commit/064160704a6b52143928d03db8e50fcd5dfe0205"
        , region = Just "arn1"
        }
        { status = Api.Warn
        , latencyMs = Just 1200
        , output = Just "Slow"
        }
