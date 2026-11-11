module BrevoTest exposing (suite)

import Expect
import Http
import Json.Encode as Encode
import Services.Brevo as Brevo
import Test exposing (Test, describe, test)
import Test.Http


suite : Test
suite =
    describe "Brevo"
        [ describe "email"
            [ test "posts to the transactional email endpoint with the API key" <|
                \() ->
                    let
                        request =
                            Brevo.email (Brevo.client "xkeysib-secret") message identity
                    in
                    ( request.method, request.url, request.headers )
                        |> Expect.equal
                            ( "POST"
                            , "https://api.brevo.com/v3/smtp/email"
                            , [ ( "api-key", "xkeysib-secret" ), ( "accept", "application/json" ) ]
                            )
            , test "sends the HTML and the text, and leaves out what is not set" <|
                \() ->
                    Brevo.email (Brevo.client "xkeysib-secret") message identity
                        |> .body
                        |> Encode.encode 0
                        |> Expect.equal """{"sender":{"email":"hello@pixo.pics"},"to":[{"email":"ada@example.com"}],"subject":"Sign in","htmlContent":"<p>Hi</p>","textContent":"Hi"}"""
            , test "sends names, the reply address and tags when set" <|
                \() ->
                    Brevo.email (Brevo.client "xkeysib-secret")
                        { message
                            | from = { email = "hello@pixo.pics", name = Just "pixo" }
                            , to = [ { email = "ada@example.com", name = Just "Ada" } ]
                            , replyTo = Just { email = "support@pixo.pics", name = Nothing }
                            , tags = [ "sign-in" ]
                        }
                        identity
                        |> .body
                        |> Encode.encode 0
                        |> Expect.equal """{"sender":{"email":"hello@pixo.pics","name":"pixo"},"to":[{"email":"ada@example.com","name":"Ada"}],"replyTo":{"email":"support@pixo.pics"},"subject":"Sign in","htmlContent":"<p>Hi</p>","textContent":"Hi","tags":["sign-in"]}"""
            , test "has no timeout unless given one" <|
                \() ->
                    ( (Brevo.email (Brevo.client "xkeysib-secret") message identity).timeout
                    , (Brevo.email (Brevo.client "xkeysib-secret" |> Brevo.withTimeout 5000) message identity).timeout
                    )
                        |> Expect.equal ( Nothing, Just 5000 )
            ]
        , describe "the response"
            [ test "gives the message id of a sent email" <|
                \() ->
                    answer (response 201 """{"messageId":"<202611101200.12345678@smtp-relay.mailin.fr>"}""")
                        |> Expect.equal (Ok { messageId = Just "<202611101200.12345678@smtp-relay.mailin.fr>" })
            , test "takes any 2xx as sent, even without a message id" <|
                \() ->
                    answer (response 202 "")
                        |> Expect.equal (Ok { messageId = Nothing })
            , test "reads the code and message of a refusal" <|
                \() ->
                    answer (response 400 """{"code":"invalid_parameter","message":"email is not valid in to"}""")
                        |> Expect.equal
                            (Err
                                (Brevo.Rejected
                                    { status = 400
                                    , code = "invalid_parameter"
                                    , message = "email is not valid in to"
                                    }
                                )
                            )
            , test "reads a refused API key the same way" <|
                \() ->
                    answer (response 401 """{"code":"unauthorized","message":"Key not found"}""")
                        |> Result.mapError Brevo.errorToString
                        |> Expect.equal (Err "Brevo refused the email with HTTP 401 (unauthorized): Key not found")
            , test "keeps the status and body of any other error" <|
                \() ->
                    answer (response 502 "Bad gateway")
                        |> Expect.equal (Err (Brevo.ServerError 502 "Bad gateway"))
            , test "tells a timeout from an unreachable network" <|
                \() ->
                    ( answer Http.Timeout_, answer Http.NetworkError_ )
                        |> Expect.equal ( Err Brevo.Timeout, Err Brevo.NetworkError )
            , test "never puts the API key in an error" <|
                \() ->
                    answer (response 401 """{"code":"unauthorized","message":"Key not found"}""")
                        |> Result.mapError (Brevo.errorToString >> String.contains "xkeysib-secret")
                        |> Expect.equal (Err False)
            ]
        ]


message : Brevo.Message
message =
    { from = { email = "hello@pixo.pics", name = Nothing }
    , to = [ { email = "ada@example.com", name = Nothing } ]
    , replyTo = Nothing
    , tags = []
    , content = { subject = "Sign in", html = "<p>Hi</p>", text = "Hi" }
    }


answer : Http.Response String -> Result Brevo.Error Brevo.Sent
answer httpResponse =
    (Brevo.email (Brevo.client "xkeysib-secret") message identity).onResponse httpResponse


response : Int -> String -> Http.Response String
response statusCode body =
    Test.Http.httpResponse { statusCode = statusCode, headers = [], body = body }
