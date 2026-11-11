module BrevoMailerTest exposing (suite)

import Data.Email as Email
import Effect
import Expect exposing (Expectation)
import Json.Decode as Decode
import Services.BrevoMailer as BrevoMailer
import Services.Mailer as Mailer
import Services.Mailer.SignIn
import Test exposing (Test, describe, test)
import Test.Http


suite : Test
suite =
    describe "BrevoMailer"
        [ test "sends a template from the configured sender, tagged, with a timeout" <|
            \() ->
                withRequest
                    (\request ->
                        ( request.headers
                        , request.timeout
                        , Decode.decodeValue envelope request.body
                        )
                            |> Expect.equal
                                ( [ ( "api-key", "xkeysib-secret" ), ( "accept", "application/json" ) ]
                                , Just 10000
                                , Ok
                                    { from = "hello@pixo.pics"
                                    , name = "pixo"
                                    , to = [ "ada@example.com" ]
                                    , subject = "Sign in to pixo"
                                    , tags = [ "sign-in" ]
                                    }
                                )
                    )
        , test "says no more than that the email went" <|
            \() ->
                withRequest
                    (\request ->
                        request.onResponse
                            (Test.Http.httpResponse
                                { statusCode = 201, headers = [], body = """{"messageId":"<1@smtp-relay.mailin.fr>"}""" }
                            )
                            |> Expect.equal (Ok ())
                    )
        , test "tells why Brevo refused, as text" <|
            \() ->
                withRequest
                    (\request ->
                        request.onResponse
                            (Test.Http.httpResponse
                                { statusCode = 401, headers = [], body = """{"code":"unauthorized","message":"Key not found"}""" }
                            )
                            |> Expect.equal (Err "Brevo refused the email with HTTP 401 (unauthorized): Key not found")
                    )
        ]


withRequest : (Effect.HttpRequest Mailer.Outcome -> Expectation) -> Expectation
withRequest toExpectation =
    case ( Email.parse "hello@pixo.pics", Email.parse "ada@example.com" ) of
        ( Ok from, Ok to ) ->
            case
                (BrevoMailer.mailer { apiKey = "xkeysib-secret", from = from }).send
                    { to = to, tag = "sign-in" }
                    (Services.Mailer.SignIn.email { url = "https://pixo.pics/sign-in?token=abc", expiresInMinutes = 15 })
            of
                Effect.Http request ->
                    toExpectation request

                _ ->
                    Expect.fail "The Brevo mailer did not make an HTTP request."

        _ ->
            Expect.fail "An address did not parse."


type alias Envelope =
    { from : String
    , name : String
    , to : List String
    , subject : String
    , tags : List String
    }


envelope : Decode.Decoder Envelope
envelope =
    Decode.map5 Envelope
        (Decode.at [ "sender", "email" ] Decode.string)
        (Decode.at [ "sender", "name" ] Decode.string)
        (Decode.field "to" (Decode.list (Decode.field "email" Decode.string)))
        (Decode.field "subject" Decode.string)
        (Decode.field "tags" (Decode.list Decode.string))
