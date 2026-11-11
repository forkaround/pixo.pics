module BrevoMailerTest exposing (suite)

import Data.Email as Email
import Effect
import Expect exposing (Expectation)
import Http
import Json.Encode as Encode
import Services.BrevoMailer as BrevoMailer
import Services.Mailer as Mailer
import Test exposing (Test, describe, test)
import Test.Http


suite : Test
suite =
    describe "BrevoMailer"
        [ describe "the request"
            [ test "posts to the transactional email endpoint with the API key and a timeout" <|
                \() ->
                    withRequest
                        (\made ->
                            ( ( made.method, made.url ), made.headers, made.timeout )
                                |> Expect.equal
                                    ( ( "POST", "https://api.brevo.com/v3/smtp/email" )
                                    , [ ( "api-key", "xkeysib-secret" ), ( "accept", "application/json" ) ]
                                    , Just 10000
                                    )
                        )
            , test "sends the sender as pixo, the recipient, the subject, the HTML, the text and the tag" <|
                \() ->
                    withRequest
                        (\made ->
                            Encode.encode 0 made.body
                                |> Expect.equal """{"sender":{"email":"hello@pixo.pics","name":"pixo"},"to":[{"email":"ada@example.com"}],"subject":"Sign in","htmlContent":"<p>Hi</p>","textContent":"Hi","tags":["sign-in"]}"""
                        )
            ]
        , describe "the outcome"
            [ test "says no more than that the email went" <|
                \() ->
                    answer (response 201 """{"messageId":"<202611101200.12345678@smtp-relay.mailin.fr>"}""")
                        |> Expect.equal (Ok ())
            , test "takes any 2xx as sent, even without a body" <|
                \() ->
                    answer (response 202 "")
                        |> Expect.equal (Ok ())
            , test "tells why Brevo refused, as text" <|
                \() ->
                    answer (response 400 """{"code":"invalid_parameter","message":"email is not valid in to"}""")
                        |> Expect.equal (Err "Brevo refused the email with HTTP 400 (invalid_parameter): email is not valid in to")
            , test "reads a refused API key the same way" <|
                \() ->
                    answer (response 401 """{"code":"unauthorized","message":"Key not found"}""")
                        |> Expect.equal (Err "Brevo refused the email with HTTP 401 (unauthorized): Key not found")
            , test "keeps the status and body of any other error" <|
                \() ->
                    answer (response 502 "Bad gateway")
                        |> Expect.equal (Err "Brevo answered with HTTP 502: Bad gateway")
            , test "tells a timeout from an unreachable network" <|
                \() ->
                    ( answer Http.Timeout_, answer Http.NetworkError_ )
                        |> Expect.equal ( Err "Brevo took too long to answer.", Err "Could not reach Brevo." )
            , test "never puts the API key in an error" <|
                \() ->
                    answer (response 401 """{"code":"unauthorized","message":"Key not found"}""")
                        |> Result.mapError (String.contains "xkeysib-secret")
                        |> Expect.equal (Err False)
            ]
        ]


request : Result String (Effect.HttpRequest Mailer.Outcome)
request =
    case ( Email.parse "hello@pixo.pics", Email.parse "ada@example.com" ) of
        ( Ok from, Ok to ) ->
            case
                (BrevoMailer.mailer { apiKey = "xkeysib-secret", from = from }).send
                    { to = to, tag = "sign-in" }
                    { subject = "Sign in", html = "<p>Hi</p>", text = "Hi" }
            of
                Effect.Http made ->
                    Ok made

                _ ->
                    Err "The Brevo mailer did not make an HTTP request."

        _ ->
            Err "An address did not parse."


withRequest : (Effect.HttpRequest Mailer.Outcome -> Expectation) -> Expectation
withRequest toExpectation =
    case request of
        Ok made ->
            toExpectation made

        Err problem ->
            Expect.fail problem


answer : Http.Response String -> Mailer.Outcome
answer httpResponse =
    request
        |> Result.andThen (\made -> made.onResponse httpResponse)


response : Int -> String -> Http.Response String
response statusCode body =
    Test.Http.httpResponse { statusCode = statusCode, headers = [], body = body }
