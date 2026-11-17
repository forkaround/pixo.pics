module MainTest exposing (suite)

import Effect
import Expect exposing (Expectation)
import Http
import InteropDefinitions as Interop
import Json.Decode as Decode
import Json.Encode as Encode
import Main
import ProgramTest exposing (ProgramTest)
import Services.BrevoMailer as BrevoMailer
import SimulatedEffect.Cmd
import SimulatedEffect.Http
import SimulatedEffect.Ports
import SimulatedEffect.Task
import SimulatedEffect.Time
import Test exposing (Test, describe, test)
import Test.Http
import Time
import TsJson.Decode as TsDecode
import TsJson.Encode as TsEncode


suite : Test
suite =
    describe "Main"
        [ describe "GET /api/health"
            [ test "passes when the database answers" <|
                \() ->
                    start (Just databaseUrl)
                        |> request "first" "GET" [ "api", "health" ]
                        |> ProgramTest.simulateHttpOk "POST" endpoint notExempt
                        |> expectResponses
                            [ health "first"
                                200
                                """{"status":"pass","releaseId":"0641607","region":"arn1","checks":{"database":{"status":"pass","latencyMs":0}}}"""
                            ]
            , test "asks Neon with the connection string from the flags" <|
                \() ->
                    start (Just databaseUrl)
                        |> request "first" "GET" [ "api", "health" ]
                        |> ProgramTest.expectHttpRequest "POST"
                            endpoint
                            (Test.Http.hasHeader "Neon-Connection-String" databaseUrl)
            , test "warns when the database is slow" <|
                \() ->
                    start (Just databaseUrl)
                        |> request "first" "GET" [ "api", "health" ]
                        |> ProgramTest.advanceTime 1500
                        |> ProgramTest.simulateHttpOk "POST" endpoint notExempt
                        |> expectSent (\values -> ( logs values, responses values ))
                            ( [ ( "info", "Sending queries to " ++ endpoint )
                              , ( "warn", "No PADDLE_* settings, so billing routes answer 503." )
                              , ( "warn", "No R2_* settings, so publishing answers 503." )
                              , ( "warn", "No MODERATION_EMAIL, so reports send no email." )
                              , ( "warn", "Health check slow: the database took 1500 ms." )
                              ]
                            , [ health "first"
                                    200
                                    """{"status":"warn","releaseId":"0641607","region":"arn1","checks":{"database":{"status":"warn","latencyMs":1500,"output":"Database slow, over 1000 ms"}}}"""
                              ]
                            )
            , test "fails when its role is exempt from row-level security" <|
                \() ->
                    start (Just databaseUrl)
                        |> request "first" "GET" [ "api", "health" ]
                        |> ProgramTest.simulateHttpOk "POST" endpoint """{"rows":[{"exempt":true}],"rowCount":1}"""
                        |> expectSent (\values -> ( levels values, responses values ))
                            ( [ "info", "warn", "warn", "warn", "error" ]
                            , [ health "first" 503 (failed "Database sent an unexpected result") ]
                            )
            , test "answers the 11th check in a minute from one address with 429, without the database or a log" <|
                \() ->
                    start (Just databaseUrl)
                        |> repeat 10 (healthFrom "203.0.113.7")
                        |> healthFrom "203.0.113.7" "11"
                        |> ProgramTest.ensureHttpRequests "POST" endpoint (List.length >> Expect.equal 10)
                        |> expectSent (\values -> ( levels values, responses values ))
                            ( [ "info", "warn", "warn", "warn" ]
                            , [ { resolve = "11"
                                , status = 429
                                , headers = ( "retry-after", "6" ) :: jsonHeaders
                                , body = """{"error":"Too many attempts."}"""
                                }
                              ]
                            )
            , test "fails when Neon can't be reached" <|
                \() ->
                    start (Just databaseUrl)
                        |> request "first" "GET" [ "api", "health" ]
                        |> ProgramTest.simulateHttpResponse "POST" endpoint Test.Http.networkError
                        |> expectResponses [ health "first" 503 (failed "Database unreachable") ]
            , test "fails when Neon takes too long" <|
                \() ->
                    start (Just databaseUrl)
                        |> request "first" "GET" [ "api", "health" ]
                        |> ProgramTest.simulateHttpResponse "POST" endpoint Test.Http.timeout
                        |> expectResponses [ health "first" 503 (failed "Database timed out after 5000 ms") ]
            , test "fails at once when the database isn't configured" <|
                \() ->
                    start Nothing
                        |> request "first" "GET" [ "api", "health" ]
                        |> expectSent (\values -> ( levels values, responses values ))
                            ( [ "warn" ]
                            , [ health "first"
                                    503
                                    """{"status":"fail","releaseId":"0641607","region":"arn1","checks":{"database":{"status":"fail","output":"Not configured: DATABASE_URL (a URL) and API_DATABASE_PASSWORD must both be set."}}}"""
                              ]
                            )
            , test "answers each request through its own resolver" <|
                \() ->
                    start (Just databaseUrl)
                        |> request "first" "GET" [ "api", "health" ]
                        |> request "second" "GET" [ "api", "nowhere" ]
                        |> ProgramTest.simulateHttpOk "POST" endpoint notExempt
                        |> expectSent (responses >> List.map (\{ resolve, status } -> ( resolve, status )))
                            [ ( "second", 404 ), ( "first", 200 ) ]
            ]
        , describe "POST /api/auth/signup"
            [ test "answers 202 once the sign-up email is sent" <|
                \() ->
                    start (Just databaseUrl)
                        |> signUp """{"email":"ada@example.com","username":"ada"}"""
                        |> ProgramTest.simulateHttpOk "POST" endpoint (linked "sign-up")
                        |> ProgramTest.simulateHttpOk "POST" BrevoMailer.endpoint """{"messageId":"<1@smtp-relay.mailin.fr>"}"""
                        |> expectResponses
                            [ { resolve = "first", status = 202, headers = jsonHeaders, body = "{}" } ]
            , test "asks for a link that lasts 15 minutes" <|
                \() ->
                    start (Just databaseUrl)
                        |> signUp """{"email":" Ada@Example.com ","username":"Ada"}"""
                        |> ProgramTest.expectHttpRequest "POST"
                            endpoint
                            (Test.Http.expectJsonBody
                                (Decode.field "params" (Decode.list Decode.string))
                                (Expect.equal [ "ada@example.com", "ada", "900" ])
                            )
            , test "emails the sign-up link to the new address" <|
                \() ->
                    start (Just databaseUrl)
                        |> signUp """{"email":"ada@example.com","username":"ada"}"""
                        |> ProgramTest.simulateHttpOk "POST" endpoint (linked "sign-up")
                        |> ProgramTest.expectHttpRequest "POST"
                            BrevoMailer.endpoint
                            (Expect.all
                                [ Test.Http.hasHeader "api-key" "xkeysib-secret"
                                , Test.Http.expectJsonBody email
                                    (Expect.equal
                                        { from = "hello@pixo.pics"
                                        , to = [ "ada@example.com" ]
                                        , subject = "Confirm your email for pixo"
                                        , tags = [ "sign-up" ]
                                        , linked = True
                                        }
                                    )
                                ]
                            )
            , test "emails a sign-in link when the address has an account" <|
                \() ->
                    start (Just databaseUrl)
                        |> signUp """{"email":"ada@example.com","username":"ada"}"""
                        |> ProgramTest.simulateHttpOk "POST" endpoint (linked "sign-in")
                        |> ProgramTest.ensureHttpRequest "POST"
                            BrevoMailer.endpoint
                            (Test.Http.expectJsonBody email
                                (Expect.equal
                                    { from = "hello@pixo.pics"
                                    , to = [ "ada@example.com" ]
                                    , subject = "Sign in to pixo"
                                    , tags = [ "sign-in" ]
                                    , linked = True
                                    }
                                )
                            )
                        |> ProgramTest.simulateHttpOk "POST" BrevoMailer.endpoint """{"messageId":"<1@smtp-relay.mailin.fr>"}"""
                        |> expectResponses
                            [ { resolve = "first", status = 202, headers = jsonHeaders, body = "{}" } ]
            , test "prints the sign-up link instead of emailing it when no key is set locally" <|
                \() ->
                    startWith { databaseUrl = Just databaseUrl, brevoApiKey = Nothing, emailFrom = Just "hello@pixo.pics", origin = "https://pixo.pics", local = True, paddle = Nothing, storage = Nothing, moderationEmail = Nothing }
                        |> signUp """{"email":"ada@example.com","username":"ada"}"""
                        |> ProgramTest.simulateHttpOk "POST" endpoint (linked "sign-up")
                        |> ProgramTest.ensureHttpRequests "POST" BrevoMailer.endpoint (List.length >> Expect.equal 0)
                        |> expectSent
                            (\values ->
                                ( List.any (Tuple.second >> String.contains "https://pixo.pics/verify#Zm9vYmFy") (logs values)
                                , List.map .status (responses values)
                                )
                            )
                            ( True, [ 202 ] )
            , test "answers 409 when the username is taken" <|
                \() ->
                    start (Just databaseUrl)
                        |> signUp """{"email":"ada@example.com","username":"ada"}"""
                        |> ProgramTest.simulateHttpOk "POST" endpoint """{"rows":[{"outcome":"username-taken","token":null}],"rowCount":1}"""
                        |> expectResponses
                            [ { resolve = "first"
                              , status = 409
                              , headers = jsonHeaders
                              , body = """{"error":"That username is taken."}"""
                              }
                            ]
            , test "answers 429 to a 4th link for one cleaned email within the hour" <|
                \() ->
                    start (Just databaseUrl)
                        |> signUpAs "1" """{"email":" Ada@Example.com ","username":"ada"}"""
                        |> signUpAs "2" """{"email":"ada@example.com","username":"ada"}"""
                        |> signUpAs "3" """{"email":" Ada@Example.com ","username":"ada"}"""
                        |> signUpAs "4" """{"email":"ada@example.com","username":"ada"}"""
                        |> ProgramTest.ensureHttpRequests "POST" endpoint (List.length >> Expect.equal 3)
                        |> expectResponses
                            [ { resolve = "4"
                              , status = 429
                              , headers = ( "retry-after", "1200" ) :: jsonHeaders
                              , body = """{"error":"Too many attempts."}"""
                              }
                            ]
            , test "answers 429 to a 6th link from one address within 15 minutes" <|
                \() ->
                    start (Just databaseUrl)
                        |> repeat 5 (\n -> signUpAs n ("{\"email\":\"ada" ++ n ++ "@example.com\",\"username\":\"ada" ++ n ++ "\"}"))
                        |> signUpAs "6" """{"email":"ada6@example.com","username":"ada6"}"""
                        |> ProgramTest.ensureHttpRequests "POST" endpoint (List.length >> Expect.equal 5)
                        |> expectResponses
                            [ { resolve = "6"
                              , status = 429
                              , headers = ( "retry-after", "180" ) :: jsonHeaders
                              , body = """{"error":"Too many attempts."}"""
                              }
                            ]
            , test "does not count a link refused for its email against the address" <|
                \() ->
                    start (Just databaseUrl)
                        |> signUpAs "1" """{"email":"ada@example.com","username":"ada"}"""
                        |> signUpAs "2" """{"email":"ada@example.com","username":"ada"}"""
                        |> signUpAs "3" """{"email":"ada@example.com","username":"ada"}"""
                        |> signUpAs "4" """{"email":"ada@example.com","username":"ada"}"""
                        |> signUpAs "5" """{"email":"bob@example.com","username":"bob"}"""
                        |> signUpAs "6" """{"email":"cyd@example.com","username":"cyd"}"""
                        |> signUpAs "7" """{"email":"dan@example.com","username":"dan"}"""
                        |> expectSent
                            (responses
                                >> List.filter (\{ status } -> status == 429)
                                >> List.map (\{ resolve, headers } -> ( resolve, headers ))
                            )
                            [ ( "4", ( "retry-after", "1200" ) :: jsonHeaders )
                            , ( "7", ( "retry-after", "180" ) :: jsonHeaders )
                            ]
            , test "answers 400 to a body that is not a sign-up" <|
                \() ->
                    start (Just databaseUrl)
                        |> signUp """{"email":"ada@example.com"}"""
                        |> expectResponses
                            [ { resolve = "first"
                              , status = 400
                              , headers = jsonHeaders
                              , body = """{"error":"Expected a JSON object with an email and a username."}"""
                              }
                            ]
            , test "answers 422 to an email that is not one" <|
                \() ->
                    start (Just databaseUrl)
                        |> signUp """{"email":"ada","username":"ada"}"""
                        |> expectResponses
                            [ { resolve = "first"
                              , status = 422
                              , headers = jsonHeaders
                              , body = """{"error":"Enter a valid email address."}"""
                              }
                            ]
            , test "answers 502 and logs why when Brevo refuses the email" <|
                \() ->
                    start (Just databaseUrl)
                        |> signUp """{"email":"ada@example.com","username":"ada"}"""
                        |> ProgramTest.simulateHttpOk "POST" endpoint (linked "sign-up")
                        |> ProgramTest.simulateHttpResponse "POST"
                            BrevoMailer.endpoint
                            (Test.Http.httpResponse
                                { statusCode = 401
                                , headers = []
                                , body = """{"code":"unauthorized","message":"Key not found"}"""
                                }
                            )
                        |> expectSent (\values -> ( logs values, responses values ))
                            ( [ ( "info", "Sending queries to " ++ endpoint )
                              , ( "warn", "No PADDLE_* settings, so billing routes answer 503." )
                              , ( "warn", "No R2_* settings, so publishing answers 503." )
                              , ( "warn", "No MODERATION_EMAIL, so reports send no email." )
                              , ( "error", "Magic link failed at its email: Brevo refused the email with HTTP 401 (unauthorized): Key not found" )
                              ]
                            , [ { resolve = "first"
                                , status = 502
                                , headers = jsonHeaders
                                , body = """{"error":"The email wasn't sent."}"""
                                }
                              ]
                            )
            , test "answers 503 and logs why when the database fails" <|
                \() ->
                    start (Just databaseUrl)
                        |> signUp """{"email":"ada@example.com","username":"ada"}"""
                        |> ProgramTest.simulateHttpResponse "POST" endpoint Test.Http.networkError
                        |> expectSent (\values -> ( levels values, List.map .status (responses values) ))
                            ( [ "info", "warn", "warn", "warn", "error" ], [ 503 ] )
            , test "answers 503 when email isn't configured on Vercel" <|
                \() ->
                    startWith { databaseUrl = Just databaseUrl, brevoApiKey = Nothing, emailFrom = Just "hello@pixo.pics", origin = "https://pixo.pics", local = False, paddle = Nothing, storage = Nothing, moderationEmail = Nothing }
                        |> signUp """{"email":"ada@example.com","username":"ada"}"""
                        |> expectResponses
                            [ { resolve = "first"
                              , status = 503
                              , headers = jsonHeaders
                              , body = """{"error":"Unavailable right now."}"""
                              }
                            ]
            , test "answers 503 and logs when the database gives an outcome a sign-up can't have" <|
                \() ->
                    start (Just databaseUrl)
                        |> signUp """{"email":"ada@example.com","username":"ada"}"""
                        |> ProgramTest.simulateHttpOk "POST" endpoint """{"rows":[{"outcome":"no-account","token":null}],"rowCount":1}"""
                        |> expectSent (\values -> ( levels values, List.map .status (responses values) ))
                            ( [ "info", "warn", "warn", "warn", "error" ], [ 503 ] )
            ]
        , describe "GET /api/auth/verify"
            [ test "says who asked for the link, without using it" <|
                \() ->
                    start (Just databaseUrl)
                        |> verify "GET" [ ( "authorization", "Bearer Zm9vYmFy" ) ]
                        |> ProgramTest.ensureHttpRequest "POST"
                            endpoint
                            (Test.Http.expectJsonBody
                                (Decode.map2 Tuple.pair
                                    (Decode.field "query" Decode.string)
                                    (Decode.field "params" (Decode.list Decode.string))
                                )
                                (Expect.equal ( "SELECT email, username, new_account FROM magic_link($1)", [ "Zm9vYmFy" ] ))
                            )
                        |> ProgramTest.simulateHttpOk "POST"
                            endpoint
                            """{"rows":[{"email":"ada@example.com","username":"ada","new_account":true}],"rowCount":1}"""
                        |> expectResponses
                            [ { resolve = "first"
                              , status = 200
                              , headers = jsonHeaders
                              , body = """{"email":"ada@example.com","username":"ada","newAccount":true}"""
                              }
                            ]
            , test "answers 404 to a link that is used, expired or unknown" <|
                \() ->
                    start (Just databaseUrl)
                        |> verify "GET" [ ( "authorization", "Bearer Zm9vYmFy" ) ]
                        |> ProgramTest.simulateHttpOk "POST" endpoint """{"rows":[],"rowCount":0}"""
                        |> expectResponses
                            [ { resolve = "first"
                              , status = 404
                              , headers = jsonHeaders
                              , body = """{"error":"This link is not valid any more. Sign up again to get a new one."}"""
                              }
                            ]
            , test "answers 401 without a token" <|
                \() ->
                    start (Just databaseUrl)
                        |> verify "GET" []
                        |> expectResponses
                            [ { resolve = "first"
                              , status = 401
                              , headers = ( "www-authenticate", "Bearer" ) :: jsonHeaders
                              , body = """{"error":"Send the link's token as a bearer token."}"""
                              }
                            ]
            ]
        , describe "POST /api/auth/verify"
            [ test "uses the link and starts a session in a cookie" <|
                \() ->
                    start (Just databaseUrl)
                        |> verify "POST" [ ( "authorization", "Bearer Zm9vYmFy" ) ]
                        |> ProgramTest.ensureHttpRequest "POST"
                            endpoint
                            (Test.Http.expectJsonBody
                                (Decode.field "params" (Decode.list Decode.string))
                                (Expect.equal [ "Zm9vYmFy", "2592000" ])
                            )
                        |> ProgramTest.simulateHttpOk "POST" endpoint (consumed "verified")
                        |> expectResponses
                            [ { resolve = "first"
                              , status = 200
                              , headers =
                                    ( "set-cookie", "session=c2Vzc2lvbg; Path=/; Max-Age=2592000; HttpOnly; SameSite=Lax; Secure" )
                                        :: jsonHeaders
                              , body = """{"email":"ada@example.com","username":"ada"}"""
                              }
                            ]
            , test "answers 404 to a link that is used, expired or unknown" <|
                \() ->
                    start (Just databaseUrl)
                        |> verify "POST" [ ( "authorization", "Bearer Zm9vYmFy" ) ]
                        |> ProgramTest.simulateHttpOk "POST" endpoint (consumed "invalid")
                        |> expectSent (responses >> List.map (\{ status, headers } -> ( status, headers )))
                            [ ( 404, jsonHeaders ) ]
            , test "answers 409 when the username was taken meanwhile" <|
                \() ->
                    start (Just databaseUrl)
                        |> verify "POST" [ ( "authorization", "Bearer Zm9vYmFy" ) ]
                        |> ProgramTest.simulateHttpOk "POST" endpoint (consumed "username-taken")
                        |> expectSent (responses >> List.map (\{ status, headers } -> ( status, headers )))
                            [ ( 409, jsonHeaders ) ]
            , test "answers 503 and logs why when the database fails" <|
                \() ->
                    start (Just databaseUrl)
                        |> verify "POST" [ ( "authorization", "Bearer Zm9vYmFy" ) ]
                        |> ProgramTest.simulateHttpResponse "POST" endpoint Test.Http.networkError
                        |> expectSent (\values -> ( levels values, List.map .status (responses values) ))
                            ( [ "info", "warn", "warn", "warn", "error" ], [ 503 ] )
            ]
        , describe "POST /api/auth/signin"
            [ test "emails a sign-in link to an address that has an account" <|
                \() ->
                    start (Just databaseUrl)
                        |> signIn """{"email":" Ada@Example.com "}"""
                        |> ProgramTest.ensureHttpRequest "POST"
                            endpoint
                            (Test.Http.expectJsonBody
                                (Decode.map2 Tuple.pair
                                    (Decode.field "query" Decode.string)
                                    (Decode.field "params" (Decode.list Decode.string))
                                )
                                (Expect.equal
                                    ( "SELECT outcome, token FROM create_sign_in_link($1, $2)"
                                    , [ "ada@example.com", "900" ]
                                    )
                                )
                            )
                        |> ProgramTest.simulateHttpOk "POST" endpoint (linked "sign-in")
                        |> ProgramTest.ensureHttpRequest "POST"
                            BrevoMailer.endpoint
                            (Test.Http.expectJsonBody email
                                (Expect.equal
                                    { from = "hello@pixo.pics"
                                    , to = [ "ada@example.com" ]
                                    , subject = "Sign in to pixo"
                                    , tags = [ "sign-in" ]
                                    , linked = True
                                    }
                                )
                            )
                        |> ProgramTest.simulateHttpOk "POST" BrevoMailer.endpoint """{"messageId":"<1@smtp-relay.mailin.fr>"}"""
                        |> expectResponses
                            [ { resolve = "first", status = 202, headers = jsonHeaders, body = "{}" } ]
            , test "answers 202 and emails nobody when the address has no account" <|
                \() ->
                    start (Just databaseUrl)
                        |> signIn """{"email":"ada@example.com"}"""
                        |> ProgramTest.simulateHttpOk "POST" endpoint """{"rows":[{"outcome":"no-account","token":null}],"rowCount":1}"""
                        |> ProgramTest.ensureHttpRequests "POST" BrevoMailer.endpoint (List.length >> Expect.equal 0)
                        |> expectResponses
                            [ { resolve = "first", status = 202, headers = jsonHeaders, body = "{}" } ]
            , test "answers 422 to an email that is not one" <|
                \() ->
                    start (Just databaseUrl)
                        |> signIn """{"email":"ada"}"""
                        |> expectResponses
                            [ { resolve = "first"
                              , status = 422
                              , headers = jsonHeaders
                              , body = """{"error":"Enter a valid email address."}"""
                              }
                            ]
            , test "answers 400 to a body without an email" <|
                \() ->
                    start (Just databaseUrl)
                        |> signIn "{}"
                        |> expectResponses
                            [ { resolve = "first"
                              , status = 400
                              , headers = jsonHeaders
                              , body = """{"error":"Expected a JSON object with an email."}"""
                              }
                            ]
            , test "answers 503 and logs when the database gives an outcome a sign-in can't have" <|
                \() ->
                    start (Just databaseUrl)
                        |> signIn """{"email":"ada@example.com"}"""
                        |> ProgramTest.simulateHttpOk "POST" endpoint (linked "sign-up")
                        |> expectSent (\values -> ( levels values, List.map .status (responses values) ))
                            ( [ "info", "warn", "warn", "warn", "error" ], [ 503 ] )
            ]
        , describe "GET /api/auth/session"
            [ test "answers with the account and the billing of the session in the cookie" <|
                \() ->
                    start (Just databaseUrl)
                        |> session "GET" [ ( "cookie", "theme=dark; session=c2Vzc2lvbg" ) ]
                        |> ProgramTest.ensureHttpRequest "POST"
                            endpoint
                            (Test.Http.expectJsonBody
                                (Decode.field "queries"
                                    (Decode.list
                                        (Decode.map2 Tuple.pair
                                            (Decode.field "query" Decode.string)
                                            (Decode.field "params" (Decode.list Decode.string))
                                        )
                                    )
                                )
                                (Expect.equal
                                    [ ( "SELECT authenticate($1) AS user_id", [ "c2Vzc2lvbg" ] )
                                    , ( "SELECT email, username FROM users", [] )
                                    , ( subscriptionsQuery, [] )
                                    ]
                                )
                            )
                        |> ProgramTest.simulateHttpOk "POST" endpoint (sessionAnswer "")
                        |> expectResponses
                            [ { resolve = "first"
                              , status = 200
                              , headers = jsonHeaders
                              , body = """{"email":"ada@example.com","username":"ada","billing":{"plan":"free","renewsAt":null,"endsAt":null,"pastDue":false,"canManage":false}}"""
                              }
                            ]
            , test "answers Pro with the date it renews" <|
                \() ->
                    start (Just databaseUrl)
                        |> session "GET" [ ( "cookie", "session=c2Vzc2lvbg" ) ]
                        |> ProgramTest.simulateHttpOk "POST" endpoint (sessionAnswer (subscriptionRow "active" "null" "null" "\"2026-12-17T10:00:00Z\""))
                        |> expectResponses
                            [ { resolve = "first"
                              , status = 200
                              , headers = jsonHeaders
                              , body = """{"email":"ada@example.com","username":"ada","billing":{"plan":"pro","renewsAt":"2026-12-17T10:00:00Z","endsAt":null,"pastDue":false,"canManage":true}}"""
                              }
                            ]
            , test "answers Pro with the date it ends when it is set to cancel" <|
                \() ->
                    start (Just databaseUrl)
                        |> session "GET" [ ( "cookie", "session=c2Vzc2lvbg" ) ]
                        |> ProgramTest.simulateHttpOk "POST" endpoint (sessionAnswer (subscriptionRow "active" "\"cancel\"" "\"2026-12-17T10:00:00Z\"" "\"2026-12-17T10:00:00Z\""))
                        |> expectResponses
                            [ { resolve = "first"
                              , status = 200
                              , headers = jsonHeaders
                              , body = """{"email":"ada@example.com","username":"ada","billing":{"plan":"pro","renewsAt":null,"endsAt":"2026-12-17T10:00:00Z","pastDue":false,"canManage":true}}"""
                              }
                            ]
            , test "answers Pro and past due when the last payment failed" <|
                \() ->
                    start (Just databaseUrl)
                        |> session "GET" [ ( "cookie", "session=c2Vzc2lvbg" ) ]
                        |> ProgramTest.simulateHttpOk "POST" endpoint (sessionAnswer (subscriptionRow "past_due" "null" "null" "\"2026-12-17T10:00:00Z\""))
                        |> expectResponses
                            [ { resolve = "first"
                              , status = 200
                              , headers = jsonHeaders
                              , body = """{"email":"ada@example.com","username":"ada","billing":{"plan":"pro","renewsAt":"2026-12-17T10:00:00Z","endsAt":null,"pastDue":true,"canManage":true}}"""
                              }
                            ]
            , test "answers Free with a portal after the subscription ended" <|
                \() ->
                    start (Just databaseUrl)
                        |> session "GET" [ ( "cookie", "session=c2Vzc2lvbg" ) ]
                        |> ProgramTest.simulateHttpOk "POST" endpoint (sessionAnswer (subscriptionRow "canceled" "null" "null" "null"))
                        |> expectResponses
                            [ { resolve = "first"
                              , status = 200
                              , headers = jsonHeaders
                              , body = """{"email":"ada@example.com","username":"ada","billing":{"plan":"free","renewsAt":null,"endsAt":null,"pastDue":false,"canManage":true}}"""
                              }
                            ]
            , test "answers 401 when the session is over" <|
                \() ->
                    start (Just databaseUrl)
                        |> session "GET" [ ( "cookie", "session=c2Vzc2lvbg" ) ]
                        |> ProgramTest.simulateHttpOk "POST" endpoint """{"results":[{"rows":[{"user_id":null}]},{"rows":[]},{"rows":[]}]}"""
                        |> expectResponses
                            [ { resolve = "first", status = 401, headers = jsonHeaders, body = """{"error":"Not signed in."}""" } ]
            , test "answers 401 at once without a cookie" <|
                \() ->
                    start (Just databaseUrl)
                        |> session "GET" []
                        |> expectResponses
                            [ { resolve = "first", status = 401, headers = jsonHeaders, body = """{"error":"Not signed in."}""" } ]
            , test "answers 503 and logs why when the database fails" <|
                \() ->
                    start (Just databaseUrl)
                        |> session "GET" [ ( "cookie", "session=c2Vzc2lvbg" ) ]
                        |> ProgramTest.simulateHttpResponse "POST" endpoint Test.Http.networkError
                        |> expectSent (\values -> ( logs values, List.map .status (responses values) ))
                            ( [ ( "info", "Sending queries to " ++ endpoint )
                              , ( "warn", "No PADDLE_* settings, so billing routes answer 503." )
                              , ( "warn", "No R2_* settings, so publishing answers 503." )
                              , ( "warn", "No MODERATION_EMAIL, so reports send no email." )
                              , ( "error", "Session lookup failed: Could not reach Neon." )
                              ]
                            , [ 503 ]
                            )
            ]
        , describe "DELETE /api/auth/account"
            [ test "deletes only the current account and clears the cookie" <|
                \() ->
                    start (Just databaseUrl)
                        |> deleteAccount [ ( "cookie", "session=c2Vzc2lvbg" ) ]
                        |> ProgramTest.ensureHttpRequest "POST"
                            endpoint
                            (Test.Http.expectJsonBody
                                (Decode.field "queries"
                                    (Decode.list
                                        (Decode.map2 Tuple.pair
                                            (Decode.field "query" Decode.string)
                                            (Decode.field "params" (Decode.list Decode.string))
                                        )
                                    )
                                )
                                (Expect.equal
                                    [ ( "SELECT authenticate($1) AS user_id", [ "c2Vzc2lvbg" ] )
                                    , ( beforeDeletionQuery, [] )
                                    ]
                                )
                            )
                        |> ProgramTest.simulateHttpOk "POST" endpoint (beforeDeletionAnswer False [])
                        |> ProgramTest.ensureHttpRequest "POST"
                            endpoint
                            (Test.Http.expectJsonBody
                                (Decode.field "queries"
                                    (Decode.list
                                        (Decode.map2 Tuple.pair
                                            (Decode.field "query" Decode.string)
                                            (Decode.field "params" (Decode.list Decode.string))
                                        )
                                    )
                                )
                                (Expect.equal
                                    [ ( "SELECT authenticate($1) AS user_id", [ "c2Vzc2lvbg" ] )
                                    , ( deletionQuery, [ "c2Vzc2lvbg" ] )
                                    ]
                                )
                            )
                        |> ProgramTest.simulateHttpOk "POST" endpoint (deletionAnswer "deleted")
                        |> expectResponses
                            [ { resolve = "first"
                              , status = 200
                              , headers = ( "set-cookie", "session=; Path=/; Max-Age=0; HttpOnly; SameSite=Lax; Secure" ) :: jsonHeaders
                              , body = "{}"
                              }
                            ]
            , test "rejects a missing session" <|
                \() ->
                    start (Just databaseUrl)
                        |> deleteAccount []
                        |> expectSent (responses >> List.map .status) [ 401 ]
            , test "rejects an expired or invalid session" <|
                \() ->
                    start (Just databaseUrl)
                        |> deleteAccount [ ( "cookie", "session=c2Vzc2lvbg" ) ]
                        |> ProgramTest.simulateHttpOk "POST" endpoint """{"results":[{"rows":[{"user_id":null}]},{"rows":[{"renewing":false,"pictures":[]}]}]}"""
                        |> expectSent (responses >> List.map .status) [ 401 ]
            , test "rejects a session that ends while the account is deleted" <|
                \() ->
                    start (Just databaseUrl)
                        |> deleteAccount [ ( "cookie", "session=c2Vzc2lvbg" ) ]
                        |> ProgramTest.simulateHttpOk "POST" endpoint (beforeDeletionAnswer False [])
                        |> ProgramTest.simulateHttpOk "POST" endpoint (deletionAnswer "signed-out")
                        |> expectSent (responses >> List.map .status) [ 401 ]
            , test "refuses while Pro renews, and keeps the cookie" <|
                \() ->
                    start (Just databaseUrl)
                        |> deleteAccount [ ( "cookie", "session=c2Vzc2lvbg" ) ]
                        |> ProgramTest.simulateHttpOk "POST" endpoint (beforeDeletionAnswer True [])
                        |> expectResponses
                            [ { resolve = "first"
                              , status = 409
                              , headers = jsonHeaders
                              , body = """{"error":"Cancel Pro before deleting your account."}"""
                              }
                            ]
            , test "refuses when Pro starts renewing while the account is deleted" <|
                \() ->
                    start (Just databaseUrl)
                        |> deleteAccount [ ( "cookie", "session=c2Vzc2lvbg" ) ]
                        |> ProgramTest.simulateHttpOk "POST" endpoint (beforeDeletionAnswer False [])
                        |> ProgramTest.simulateHttpOk "POST" endpoint (deletionAnswer "renewing")
                        |> expectSent (responses >> List.map .status) [ 409 ]
            , test "deletes the published files first" <|
                \() ->
                    startPublishing
                        |> deleteAccount [ ( "cookie", "session=c2Vzc2lvbg" ) ]
                        |> ProgramTest.simulateHttpOk "POST" endpoint (beforeDeletionAnswer False [ pictureId, otherPictureId ])
                        |> ProgramTest.ensureHttpRequest "DELETE" (publicFile pictureId) (\_ -> Expect.pass)
                        |> ProgramTest.simulateHttpResponse "DELETE" (publicFile pictureId) (Test.Http.httpResponse { statusCode = 204, headers = [], body = "" })
                        |> ProgramTest.ensureHttpRequest "POST" endpoint (expectQueries [ authenticateQuery, ( dropQuery, [ pictureId ] ) ])
                        |> ProgramTest.simulateHttpOk "POST" endpoint (droppedAnswer pictureId)
                        |> ProgramTest.ensureHttpRequests "DELETE" (publicFile otherPictureId) (List.length >> Expect.equal 1)
                        |> ProgramTest.simulateHttpResponse "DELETE" (publicFile otherPictureId) (Test.Http.httpResponse { statusCode = 204, headers = [], body = "" })
                        |> ProgramTest.ensureHttpRequest "POST" endpoint (expectQueries [ authenticateQuery, ( dropQuery, [ otherPictureId ] ) ])
                        |> ProgramTest.simulateHttpOk "POST" endpoint (droppedAnswer otherPictureId)
                        |> ProgramTest.ensureHttpRequest "POST" endpoint (expectQueries [ authenticateQuery, ( deletionQuery, [ "c2Vzc2lvbg" ] ) ])
                        |> ProgramTest.simulateHttpOk "POST" endpoint (deletionAnswer "deleted")
                        |> expectSent (responses >> List.map .status) [ 200 ]
            , test "keeps the account when a file can't be deleted" <|
                \() ->
                    startPublishing
                        |> deleteAccount [ ( "cookie", "session=c2Vzc2lvbg" ) ]
                        |> ProgramTest.simulateHttpOk "POST" endpoint (beforeDeletionAnswer False [ pictureId ])
                        |> ProgramTest.simulateHttpResponse "DELETE" (publicFile pictureId) (Test.Http.httpResponse { statusCode = 403, headers = [], body = "<Error/>" })
                        |> ProgramTest.ensureHttpRequests "POST" endpoint (List.length >> Expect.equal 0)
                        |> expectSent (responses >> List.map .status) [ 503 ]
            , test "answers 503 when pictures are published and R2 is not configured" <|
                \() ->
                    start (Just databaseUrl)
                        |> deleteAccount [ ( "cookie", "session=c2Vzc2lvbg" ) ]
                        |> ProgramTest.simulateHttpOk "POST" endpoint (beforeDeletionAnswer False [ pictureId ])
                        |> ProgramTest.ensureHttpRequests "POST" endpoint (List.length >> Expect.equal 0)
                        |> expectSent (\values -> ( List.map .status (responses values), List.drop 4 (logs values) ))
                            ( [ 503 ]
                            , [ ( "error", "Can't delete an account with published pictures without R2 settings." ) ]
                            )
            , test "reports a database failure without clearing the cookie" <|
                \() ->
                    start (Just databaseUrl)
                        |> deleteAccount [ ( "cookie", "session=c2Vzc2lvbg" ) ]
                        |> ProgramTest.simulateHttpResponse "POST" endpoint Test.Http.networkError
                        |> expectResponses
                            [ { resolve = "first", status = 503, headers = jsonHeaders, body = """{"error":"Unavailable right now."}""" } ]
            ]
        , describe "POST /api/pictures"
            [ test "answers 503 without R2 settings" <|
                \() ->
                    startPaid
                        |> publishRequest "first" publishBody
                        |> expectSent (responses >> List.map .status) [ 503 ]
            , test "answers 400 for a body that is not JSON" <|
                \() ->
                    startPublishing
                        |> publishRequest "first" "nope"
                        |> expectResponses
                            [ { resolve = "first"
                              , status = 400
                              , headers = jsonHeaders
                              , body = """{"error":"Expected a JSON object with a prompt, a width, a height, steps, a seed, a model and bytes."}"""
                              }
                            ]
            , test "answers 422 for a picture over 2 MB" <|
                \() ->
                    startPublishing
                        |> publishRequest "first" (String.replace "\"bytes\":12" "\"bytes\":2097153" publishBody)
                        |> expectResponses
                            [ { resolve = "first", status = 422, headers = jsonHeaders, body = """{"error":"The picture must be at most 2 MB."}""" } ]
            , test "answers 401 when the session is over" <|
                \() ->
                    startPublishing
                        |> publishRequest "first" publishBody
                        |> ProgramTest.simulateHttpOk "POST" endpoint """{"results":[{"rows":[{"user_id":null}]},{"rows":[{"pro":null,"id":null}]}]}"""
                        |> expectSent (responses >> List.map .status) [ 401 ]
            , test "answers 403 when the user is not Pro" <|
                \() ->
                    startPublishing
                        |> publishRequest "first" publishBody
                        |> ProgramTest.simulateHttpOk "POST" endpoint (askedAnswer "false" "null")
                        |> expectResponses
                            [ { resolve = "first", status = 403, headers = jsonHeaders, body = """{"error":"Publishing needs Pro."}""" } ]
            , test "answers 429 when three publishes are unfinished" <|
                \() ->
                    startPublishing
                        |> publishRequest "first" publishBody
                        |> ProgramTest.simulateHttpOk "POST" endpoint (askedAnswer "true" "null")
                        |> expectResponses
                            [ { resolve = "first", status = 429, headers = jsonHeaders, body = """{"error":"Finish or cancel your other publishes first."}""" } ]
            , test "asks the database with the metadata and the tags, in one statement" <|
                \() ->
                    startPublishing
                        |> publishRequest "first" publishBody
                        |> ProgramTest.expectHttpRequest "POST"
                            endpoint
                            (Test.Http.expectJsonBody
                                (Decode.field "queries" (Decode.list (Decode.field "params" (Decode.list Decode.string))))
                                (Expect.equal
                                    [ [ "c2Vzc2lvbg" ]
                                    , [ "a red fox", "1024", "1024", "4", "7", "prism-ml/bonsai-image-ternary-4B-mlx-2bit", "12", "[\"fox\"]" ]
                                    ]
                                )
                            )
            , test "answers a presigned link for the staged key, and calls R2 not at all" <|
                \() ->
                    startPublishing
                        |> publishRequest "first" publishBody
                        |> ProgramTest.simulateHttpOk "POST" endpoint (askedAnswer "true" ("\"" ++ pictureId ++ "\""))
                        |> ProgramTest.ensureHttpRequests "HEAD" (stagedFile pictureId) (List.length >> Expect.equal 0)
                        |> ProgramTest.ensureHttpRequests "PUT" (stagedFile pictureId) (List.length >> Expect.equal 0)
                        |> expectSent
                            (responses
                                >> List.map
                                    (\answer ->
                                        ( answer.status
                                        , String.startsWith ("{\"id\":\"" ++ pictureId ++ "\",\"url\":\"" ++ stagedFile pictureId ++ "?X-Amz-") answer.body
                                        , String.endsWith "\"headers\":[[\"content-type\",\"image/jpeg\"]]}" answer.body
                                        )
                                    )
                            )
                            [ ( 201, True, True ) ]
            , test "answers 429 to the 31st publish in an hour from one address" <|
                \() ->
                    startPublishing
                        |> repeat 31 (\resolve app -> app |> ProgramTest.advanceTime 2000 |> publishRequest resolve publishBody)
                        |> expectSent
                            (responses >> List.filter (\answer -> answer.status == 429) >> List.map (\answer -> ( answer.resolve, List.any (Tuple.first >> (==) "retry-after") answer.headers )))
                            [ ( "31", True ) ]
            ]
        , describe "POST /api/pictures/<id>/published"
            [ test "answers 404 when the row is missing" <|
                \() ->
                    startPublishing
                        |> confirmRequest "first"
                        |> ProgramTest.simulateHttpOk "POST" endpoint """{"results":[{"rows":[{"user_id":"7b0c9a52-3d0e-4a53-9a7e-0f6f1f3f5b11"}]},{"rows":[]}]}"""
                        |> expectResponses
                            [ { resolve = "first", status = 404, headers = jsonHeaders, body = """{"error":"No such picture."}""" } ]
            , test "answers 200 with the stored answer, and calls R2 not at all, when already published" <|
                \() ->
                    startPublishing
                        |> confirmRequest "first"
                        |> ProgramTest.simulateHttpOk "POST" endpoint (unconfirmedAnswer "12" "\"2026-11-17T10:00:00Z\"")
                        |> ProgramTest.ensureHttpRequests "HEAD" (stagedFile pictureId) (List.length >> Expect.equal 0)
                        |> expectResponses [ publishedResponse ]
            , test "copies the checked file to the public bucket, deletes the staged one and marks the row" <|
                \() ->
                    startPublishing
                        |> confirmRequest "first"
                        |> ProgramTest.simulateHttpOk "POST" endpoint (unconfirmedAnswer "12" "null")
                        |> ProgramTest.simulateHttpResponse "HEAD" (stagedFile pictureId) (stagedAnswer "12" "image/jpeg")
                        |> ProgramTest.ensureHttpRequest "PUT"
                            (publicFile pictureId)
                            (Expect.all
                                [ Test.Http.hasHeader "x-amz-copy-source" ("/uploads/incoming/" ++ pictureId ++ ".jpg")
                                , Test.Http.hasHeader "x-amz-copy-source-if-match" "\"abc\""
                                ]
                            )
                        |> ProgramTest.simulateHttpResponse "PUT" (publicFile pictureId) (copiedAnswer "\"abc\"")
                        |> ProgramTest.ensureHttpRequests "DELETE" (stagedFile pictureId) (List.length >> Expect.equal 1)
                        |> ProgramTest.simulateHttpResponse "DELETE" (stagedFile pictureId) (Test.Http.httpResponse { statusCode = 204, headers = [], body = "" })
                        |> ProgramTest.ensureHttpRequest "POST" endpoint (expectQueries [ authenticateQuery, ( markQuery, [ pictureId ] ) ])
                        |> ProgramTest.simulateHttpOk "POST" endpoint (markedAnswer "\"2026-11-17T10:00:00Z\"")
                        |> expectResponses [ publishedResponse ]
            , test "refuses a staged file larger than declared, deleting it and the row" <|
                \() ->
                    startPublishing
                        |> confirmRequest "first"
                        |> ProgramTest.simulateHttpOk "POST" endpoint (unconfirmedAnswer "12" "null")
                        |> ProgramTest.simulateHttpResponse "HEAD" (stagedFile pictureId) (stagedAnswer "13" "image/jpeg")
                        |> ProgramTest.ensureHttpRequests "PUT" (publicFile pictureId) (List.length >> Expect.equal 0)
                        |> ProgramTest.simulateHttpResponse "DELETE" (stagedFile pictureId) (Test.Http.httpResponse { statusCode = 204, headers = [], body = "" })
                        |> ProgramTest.ensureHttpRequest "POST" endpoint (expectQueries [ authenticateQuery, ( dropQuery, [ pictureId ] ) ])
                        |> ProgramTest.simulateHttpOk "POST" endpoint (droppedAnswer pictureId)
                        |> expectResponses
                            [ { resolve = "first", status = 422, headers = jsonHeaders, body = """{"error":"The picture must be a JPEG of at most 2 MB."}""" } ]
            , test "refuses a staged file that is not a JPEG" <|
                \() ->
                    startPublishing
                        |> confirmRequest "first"
                        |> ProgramTest.simulateHttpOk "POST" endpoint (unconfirmedAnswer "12" "null")
                        |> ProgramTest.simulateHttpResponse "HEAD" (stagedFile pictureId) (stagedAnswer "12" "text/html")
                        |> ProgramTest.simulateHttpResponse "DELETE" (stagedFile pictureId) (Test.Http.httpResponse { statusCode = 204, headers = [], body = "" })
                        |> ProgramTest.ensureHttpRequest "POST" endpoint (expectQueries [ authenticateQuery, ( dropQuery, [ pictureId ] ) ])
                        |> ProgramTest.simulateHttpOk "POST" endpoint (droppedAnswer pictureId)
                        |> expectResponses
                            [ { resolve = "first", status = 422, headers = jsonHeaders, body = """{"error":"The picture must be a JPEG of at most 2 MB."}""" } ]
            , test "answers 409 and copies nothing when nothing was staged" <|
                \() ->
                    startPublishing
                        |> confirmRequest "first"
                        |> ProgramTest.simulateHttpOk "POST" endpoint (unconfirmedAnswer "12" "null")
                        |> ProgramTest.simulateHttpResponse "HEAD" (stagedFile pictureId) (Test.Http.httpResponse { statusCode = 404, headers = [], body = "" })
                        |> ProgramTest.ensureHttpRequests "PUT" (publicFile pictureId) (List.length >> Expect.equal 0)
                        |> expectResponses
                            [ { resolve = "first", status = 409, headers = jsonHeaders, body = """{"error":"The picture hasn't arrived. Upload it again."}""" } ]
            , test "answers 503 and marks nothing when the copy fails" <|
                \() ->
                    startPublishing
                        |> confirmRequest "first"
                        |> ProgramTest.simulateHttpOk "POST" endpoint (unconfirmedAnswer "12" "null")
                        |> ProgramTest.simulateHttpResponse "HEAD" (stagedFile pictureId) (stagedAnswer "12" "image/jpeg")
                        |> ProgramTest.simulateHttpResponse "PUT" (publicFile pictureId) (Test.Http.httpResponse { statusCode = 500, headers = [], body = "<Error/>" })
                        |> ProgramTest.ensureHttpRequests "POST" endpoint (List.length >> Expect.equal 0)
                        |> expectResponses
                            [ { resolve = "first", status = 503, headers = jsonHeaders, body = """{"error":"Couldn't store the picture."}""" } ]
            , test "deletes both files and the row when the staged file changed after the check" <|
                \() ->
                    startPublishing
                        |> confirmRequest "first"
                        |> ProgramTest.simulateHttpOk "POST" endpoint (unconfirmedAnswer "12" "null")
                        |> ProgramTest.simulateHttpResponse "HEAD" (stagedFile pictureId) (stagedAnswer "12" "image/jpeg")
                        |> ProgramTest.simulateHttpResponse "PUT" (publicFile pictureId) (Test.Http.httpResponse { statusCode = 412, headers = [], body = "<Error/>" })
                        |> ProgramTest.simulateHttpResponse "DELETE" (publicFile pictureId) (Test.Http.httpResponse { statusCode = 204, headers = [], body = "" })
                        |> ProgramTest.simulateHttpResponse "DELETE" (stagedFile pictureId) (Test.Http.httpResponse { statusCode = 204, headers = [], body = "" })
                        |> ProgramTest.ensureHttpRequest "POST" endpoint (expectQueries [ authenticateQuery, ( dropQuery, [ pictureId ] ) ])
                        |> ProgramTest.simulateHttpOk "POST" endpoint (droppedAnswer pictureId)
                        |> expectResponses
                            [ { resolve = "first", status = 409, headers = jsonHeaders, body = """{"error":"The picture changed while publishing. Publish it again."}""" } ]
            , test "keeps the row when the replaced copy can't be deleted" <|
                \() ->
                    startPublishing
                        |> confirmRequest "first"
                        |> ProgramTest.simulateHttpOk "POST" endpoint (unconfirmedAnswer "12" "null")
                        |> ProgramTest.simulateHttpResponse "HEAD" (stagedFile pictureId) (stagedAnswer "12" "image/jpeg")
                        |> ProgramTest.simulateHttpResponse "PUT" (publicFile pictureId) (Test.Http.httpResponse { statusCode = 412, headers = [], body = "<Error/>" })
                        |> ProgramTest.simulateHttpResponse "DELETE" (publicFile pictureId) (Test.Http.httpResponse { statusCode = 403, headers = [], body = "<Error/>" })
                        |> ProgramTest.ensureHttpRequests "DELETE" (stagedFile pictureId) (List.length >> Expect.equal 0)
                        |> ProgramTest.ensureHttpRequests "POST" endpoint (List.length >> Expect.equal 0)
                        |> expectSent
                            (\values ->
                                ( List.map .status (responses values)
                                , logs values |> List.filter (\( level, message ) -> level == "error" && String.startsWith ("R2 delete failed for " ++ pictureId) message) |> List.length
                                )
                            )
                            ( [ 503 ], 1 )
            , test "does the same when the copy carries another ETag" <|
                \() ->
                    startPublishing
                        |> confirmRequest "first"
                        |> ProgramTest.simulateHttpOk "POST" endpoint (unconfirmedAnswer "12" "null")
                        |> ProgramTest.simulateHttpResponse "HEAD" (stagedFile pictureId) (stagedAnswer "12" "image/jpeg")
                        |> ProgramTest.simulateHttpResponse "PUT" (publicFile pictureId) (copiedAnswer "\"zzz\"")
                        |> ProgramTest.ensureHttpRequests "DELETE" (publicFile pictureId) (List.length >> Expect.equal 1)
                        |> ProgramTest.simulateHttpResponse "DELETE" (publicFile pictureId) (Test.Http.httpResponse { statusCode = 204, headers = [], body = "" })
                        |> ProgramTest.simulateHttpResponse "DELETE" (stagedFile pictureId) (Test.Http.httpResponse { statusCode = 204, headers = [], body = "" })
                        |> ProgramTest.ensureHttpRequest "POST" endpoint (expectQueries [ authenticateQuery, ( dropQuery, [ pictureId ] ) ])
                        |> ProgramTest.simulateHttpOk "POST" endpoint (droppedAnswer pictureId)
                        |> expectResponses
                            [ { resolve = "first", status = 409, headers = jsonHeaders, body = """{"error":"The picture changed while publishing. Publish it again."}""" } ]
            , test "answers with the stored row when a concurrent confirm marked it first" <|
                \() ->
                    startPublishing
                        |> confirmRequest "first"
                        |> ProgramTest.simulateHttpOk "POST" endpoint (unconfirmedAnswer "12" "null")
                        |> ProgramTest.simulateHttpResponse "HEAD" (stagedFile pictureId) (stagedAnswer "12" "image/jpeg")
                        |> ProgramTest.simulateHttpResponse "PUT" (publicFile pictureId) (copiedAnswer "\"abc\"")
                        |> ProgramTest.simulateHttpResponse "DELETE" (stagedFile pictureId) (Test.Http.httpResponse { statusCode = 204, headers = [], body = "" })
                        |> ProgramTest.simulateHttpOk "POST" endpoint """{"results":[{"rows":[{"user_id":"7b0c9a52-3d0e-4a53-9a7e-0f6f1f3f5b11"}]},{"rows":[]}]}"""
                        |> ProgramTest.simulateHttpOk "POST" endpoint (unconfirmedAnswer "12" "\"2026-11-17T10:00:00Z\"")
                        |> expectResponses [ publishedResponse ]
            ]
        , describe "DELETE /api/pictures/<id>"
            [ test "answers 404 for a picture that is not the user's" <|
                \() ->
                    startPublishing
                        |> unpublishRequest
                        |> ProgramTest.simulateHttpOk "POST" endpoint (foundAnswer "false")
                        |> expectResponses
                            [ { resolve = "first", status = 404, headers = jsonHeaders, body = """{"error":"No such picture."}""" } ]
            , test "deletes the public file, the staged one and the row" <|
                \() ->
                    startPublishing
                        |> unpublishRequest
                        |> ProgramTest.simulateHttpOk "POST" endpoint (foundAnswer "true")
                        |> ProgramTest.simulateHttpResponse "DELETE" (publicFile pictureId) (Test.Http.httpResponse { statusCode = 204, headers = [], body = "" })
                        |> ProgramTest.simulateHttpResponse "DELETE" (stagedFile pictureId) (Test.Http.httpResponse { statusCode = 204, headers = [], body = "" })
                        |> ProgramTest.ensureHttpRequest "POST" endpoint (expectQueries [ authenticateQuery, ( dropQuery, [ pictureId ] ) ])
                        |> ProgramTest.simulateHttpOk "POST" endpoint (droppedAnswer pictureId)
                        |> expectResponses
                            [ { resolve = "first", status = 200, headers = jsonHeaders, body = "{}" } ]
            , test "answers 503 and keeps the row when the public delete fails" <|
                \() ->
                    startPublishing
                        |> unpublishRequest
                        |> ProgramTest.simulateHttpOk "POST" endpoint (foundAnswer "true")
                        |> ProgramTest.simulateHttpResponse "DELETE" (publicFile pictureId) (Test.Http.httpResponse { statusCode = 403, headers = [], body = "<Error/>" })
                        |> ProgramTest.ensureHttpRequests "POST" endpoint (List.length >> Expect.equal 0)
                        |> expectSent (\values -> ( List.map .status (responses values), List.drop 1 (logs values) ))
                            ( [ 503 ]
                            , [ ( "error", "R2 delete failed: R2 answered 403: <Error/>" ) ]
                            )
            ]
        , describe "POST /api/pictures/<id>/reports"
            [ test "answers 400 for a body without a reason" <|
                \() ->
                    startPublishing
                        |> reportRequest "first" "{}"
                        |> expectResponses
                            [ { resolve = "first", status = 400, headers = jsonHeaders, body = """{"error":"Expected a JSON object with a reason."}""" } ]
            , test "answers 422 for an unknown reason, without the database" <|
                \() ->
                    startPublishing
                        |> reportRequest "first" """{"reason":"rude"}"""
                        |> ProgramTest.ensureHttpRequests "POST" endpoint (List.length >> Expect.equal 0)
                        |> expectResponses
                            [ { resolve = "first", status = 422, headers = jsonHeaders, body = """{"error":"Unknown reason."}""" } ]
            , test "answers 404 for a picture that is not published" <|
                \() ->
                    startPublishing
                        |> reportRequest "first" """{"reason":"spam"}"""
                        |> ProgramTest.ensureHttpRequest "POST" endpoint (expectPlainQuery ( "SELECT outcome, first, username FROM report_picture($1::uuid, $2)", [ Just pictureId, Just "spam" ] ))
                        |> ProgramTest.simulateHttpOk "POST" endpoint (reportedRows "not-found" "true" "null")
                        |> expectResponses
                            [ { resolve = "first", status = 404, headers = jsonHeaders, body = """{"error":"No such picture."}""" } ]
            , test "answers 202 and sends no email for a second report" <|
                \() ->
                    startPublishing
                        |> reportRequest "first" """{"reason":"spam"}"""
                        |> ProgramTest.simulateHttpOk "POST" endpoint (reportedRows "reported" "false" "\"ada\"")
                        |> ProgramTest.ensureHttpRequests "POST" BrevoMailer.endpoint (List.length >> Expect.equal 0)
                        |> expectResponses
                            [ { resolve = "first", status = 202, headers = jsonHeaders, body = "{}" } ]
            , test "emails the moderators about the first report" <|
                \() ->
                    startPublishing
                        |> reportRequest "first" """{"reason":"spam"}"""
                        |> ProgramTest.simulateHttpOk "POST" endpoint (reportedRows "reported" "true" "\"ada\"")
                        |> ProgramTest.ensureHttpRequest "POST"
                            BrevoMailer.endpoint
                            (Test.Http.expectJsonBody
                                (Decode.map5 (\to subject tags html text -> { to = to, subject = subject, tags = tags, html = html, text = text })
                                    (Decode.field "to" (Decode.list (Decode.field "email" Decode.string)))
                                    (Decode.field "subject" Decode.string)
                                    (Decode.field "tags" (Decode.list Decode.string))
                                    (Decode.field "htmlContent" Decode.string)
                                    (Decode.field "textContent" Decode.string)
                                )
                                (\{ to, subject, tags, html, text } ->
                                    Expect.all
                                        [ \_ -> Expect.equal [ "mod@pixo.pics" ] to
                                        , \_ -> Expect.equal "A picture was reported on pixo" subject
                                        , \_ -> Expect.equal [ "report" ] tags
                                        , \_ -> Expect.equal True (String.contains ("https://img.pixo.pics/" ++ pictureId ++ ".jpg") text)
                                        , \_ -> Expect.equal True (String.contains "https://pixo.pics/ada" text)
                                        , \_ -> Expect.equal True (String.contains "Reason: spam." html)
                                        , \_ -> Expect.equal True (String.contains ("take_down_picture('" ++ pictureId ++ "')") text)
                                        ]
                                        ()
                                )
                            )
                        |> ProgramTest.simulateHttpOk "POST" BrevoMailer.endpoint """{"messageId":"<1@smtp-relay.mailin.fr>"}"""
                        |> expectResponses
                            [ { resolve = "first", status = 202, headers = jsonHeaders, body = "{}" } ]
            , test "answers 202 all the same, and logs why, when the email fails" <|
                \() ->
                    startPublishing
                        |> reportRequest "first" """{"reason":"spam"}"""
                        |> ProgramTest.simulateHttpOk "POST" endpoint (reportedRows "reported" "true" "\"ada\"")
                        |> ProgramTest.simulateHttpResponse "POST" BrevoMailer.endpoint Test.Http.networkError
                        |> expectSent (\values -> ( List.map .status (responses values), List.map Tuple.first (List.drop 1 (logs values)) ))
                            ( [ 202 ], [ "error" ] )
            , test "answers 429 to the 11th report in an hour from one address" <|
                \() ->
                    startPublishing
                        |> repeat 11 (\resolve app -> app |> ProgramTest.advanceTime 2000 |> reportRequest resolve """{"reason":"spam"}""")
                        |> expectSent
                            (responses >> List.filter (\answer -> answer.status == 429) >> List.map (\answer -> ( answer.resolve, List.any (Tuple.first >> (==) "retry-after") answer.headers )))
                            [ ( "11", True ) ]
            ]
        , describe "GET /api/feed"
            [ test "answers 503 without R2 settings" <|
                \() ->
                    start (Just databaseUrl)
                        |> request "first" "GET" [ "api", "feed" ]
                        |> expectSent (responses >> List.map .status) [ 503 ]
            , test "asks for the newest page and answers the pictures, cached for everyone" <|
                \() ->
                    startPublishing
                        |> request "first" "GET" [ "api", "feed" ]
                        |> ProgramTest.ensureHttpRequest "POST" endpoint (expectPlainQuery ( "SELECT * FROM feed_pictures($1, $2::uuid, $3)", [ Nothing, Nothing, Just "30" ] ))
                        |> ProgramTest.simulateHttpOk "POST" endpoint (feedRows [ feedRow pictureId ])
                        |> expectResponses
                            [ { resolve = "first"
                              , status = 200
                              , headers = sharedHeaders 60
                              , body = """{"pictures":[{"id":"6f1c2a52-3d0e-4a53-9a7e-0f6f1f3f5b11","url":"https://img.pixo.pics/6f1c2a52-3d0e-4a53-9a7e-0f6f1f3f5b11.jpg","username":"ada","prompt":"a red fox","width":1024,"height":1024,"steps":4,"seed":7,"model":"prism-ml/bonsai-image-ternary-4B-mlx-2bit","tags":["fox"],"publishedAt":"2026-11-17T10:00:00Z"}],"next":null}"""
                              }
                            ]
            , test "points to the last picture when the page is full" <|
                \() ->
                    startPublishing
                        |> request "first" "GET" [ "api", "feed" ]
                        |> ProgramTest.simulateHttpOk "POST" endpoint (feedRows (List.repeat 29 (feedRow pictureId) ++ [ feedRow otherPictureId ]))
                        |> expectSent (responses >> List.map (.body >> Decode.decodeString (Decode.field "next" Decode.string) >> Result.toMaybe))
                            [ Just otherPictureId ]
            , test "passes the picture to start after" <|
                \() ->
                    startPublishing
                        |> request "first" "GET" [ "api", "feed", pictureId ]
                        |> ProgramTest.expectHttpRequest "POST" endpoint (expectPlainQuery ( "SELECT * FROM feed_pictures($1, $2::uuid, $3)", [ Nothing, Just pictureId, Just "30" ] ))
            , test "passes the tag" <|
                \() ->
                    startPublishing
                        |> request "first" "GET" [ "api", "feed", "tag", "fox" ]
                        |> ProgramTest.expectHttpRequest "POST" endpoint (expectPlainQuery ( "SELECT * FROM feed_pictures($1, $2::uuid, $3)", [ Just "fox", Nothing, Just "30" ] ))
            , test "passes a tag with a space" <|
                \() ->
                    startPublishing
                        |> request "first" "GET" [ "api", "feed", "tag", "sea life" ]
                        |> ProgramTest.expectHttpRequest "POST" endpoint (expectPlainQuery ( "SELECT * FROM feed_pictures($1, $2::uuid, $3)", [ Just "sea life", Nothing, Just "30" ] ))
            , test "answers 404 at a tag that is not written canonically" <|
                \() ->
                    startPublishing
                        |> request "first" "GET" [ "api", "feed", "tag", "Fox" ]
                        |> expectSent (responses >> List.map .status) [ 404 ]
            , test "reads the same with or without a session cookie and sets none" <|
                \() ->
                    startPublishing
                        |> requestWith { resolve = "first", method = "GET", path = [ "api", "feed" ], headers = pictureHeaders, body = "" }
                        |> ProgramTest.ensureHttpRequest "POST" endpoint (expectPlainQuery ( "SELECT * FROM feed_pictures($1, $2::uuid, $3)", [ Nothing, Nothing, Just "30" ] ))
                        |> ProgramTest.simulateHttpOk "POST" endpoint (feedRows [])
                        |> expectSent (responses >> List.map (.headers >> List.any (\( name, _ ) -> name == "set-cookie")))
                            [ False ]
            ]
        , describe "GET /api/feed/tags"
            [ test "asks for the top 20 and answers them, cached for five minutes" <|
                \() ->
                    start (Just databaseUrl)
                        |> request "first" "GET" [ "api", "feed", "tags" ]
                        |> ProgramTest.ensureHttpRequest "POST" endpoint (expectPlainQuery ( "SELECT tag, pictures FROM feed_tags($1)", [ Just "20" ] ))
                        |> ProgramTest.simulateHttpOk "POST" endpoint """{"rows":[{"tag":"fox","pictures":"12"},{"tag":"x","pictures":"3"}],"rowCount":2}"""
                        |> expectResponses
                            [ { resolve = "first"
                              , status = 200
                              , headers = sharedHeaders 300
                              , body = """[{"tag":"fox","pictures":12}]"""
                              }
                            ]
            ]
        , describe "GET /api/profiles/<name>"
            [ test "answers 503 without R2 settings" <|
                \() ->
                    start (Just databaseUrl)
                        |> request "first" "GET" [ "api", "profiles", "ada" ]
                        |> expectSent (responses >> List.map .status) [ 503 ]
            , test "runs one plain query and answers the page, cached for everyone" <|
                \() ->
                    startPublishing
                        |> request "first" "GET" [ "api", "profiles", "ada" ]
                        |> ProgramTest.ensureHttpRequest "POST" endpoint (expectPlainQuery ( "SELECT username, pictures FROM profile($1, $2::uuid, $3)", [ Just "ada", Nothing, Just "30" ] ))
                        |> ProgramTest.simulateHttpOk "POST" endpoint (profileRows [ profilePicture pictureId ])
                        |> expectResponses
                            [ { resolve = "first"
                              , status = 200
                              , headers = sharedHeaders 60
                              , body = """{"username":"ada","pictures":[{"id":"6f1c2a52-3d0e-4a53-9a7e-0f6f1f3f5b11","url":"https://img.pixo.pics/6f1c2a52-3d0e-4a53-9a7e-0f6f1f3f5b11.jpg","username":"ada","prompt":"a red fox","width":1024,"height":1024,"steps":4,"seed":7,"model":"prism-ml/bonsai-image-ternary-4B-mlx-2bit","tags":["fox"],"publishedAt":"2026-11-17T10:00:00Z"}],"next":null}"""
                              }
                            ]
            , test "passes the picture to start after" <|
                \() ->
                    startPublishing
                        |> request "first" "GET" [ "api", "profiles", "ada", pictureId ]
                        |> ProgramTest.expectHttpRequest "POST" endpoint (expectPlainQuery ( "SELECT username, pictures FROM profile($1, $2::uuid, $3)", [ Just "ada", Just pictureId, Just "30" ] ))
            , test "sends the same query whether or not a cookie comes with it" <|
                \() ->
                    startPublishing
                        |> requestWith { resolve = "first", method = "GET", path = [ "api", "profiles", "ada" ], headers = pictureHeaders, body = "" }
                        |> ProgramTest.expectHttpRequest "POST" endpoint (expectPlainQuery ( "SELECT username, pictures FROM profile($1, $2::uuid, $3)", [ Just "ada", Nothing, Just "30" ] ))
            , test "answers 404 for no such profile, not cached" <|
                \() ->
                    startPublishing
                        |> request "first" "GET" [ "api", "profiles", "ada" ]
                        |> ProgramTest.simulateHttpOk "POST" endpoint """{"rows":[],"rowCount":0}"""
                        |> expectResponses
                            [ { resolve = "first", status = 404, headers = jsonHeaders, body = """{"error":"No such profile."}""" } ]
            , test "answers 404 at a reserved name" <|
                \() ->
                    startPublishing
                        |> request "first" "GET" [ "api", "profiles", "studio" ]
                        |> expectSent (responses >> List.map .status) [ 404 ]
            ]
        , describe "DELETE /api/auth/session"
            [ test "ends the session and clears the cookie" <|
                \() ->
                    start (Just databaseUrl)
                        |> session "DELETE" [ ( "cookie", "session=c2Vzc2lvbg" ) ]
                        |> ProgramTest.ensureHttpRequest "POST"
                            endpoint
                            (Test.Http.expectJsonBody
                                (Decode.map2 Tuple.pair
                                    (Decode.field "query" Decode.string)
                                    (Decode.field "params" (Decode.list Decode.string))
                                )
                                (Expect.equal ( "SELECT sign_out($1)", [ "c2Vzc2lvbg" ] ))
                            )
                        |> ProgramTest.simulateHttpOk "POST" endpoint """{"rows":[{"sign_out":""}],"rowCount":1}"""
                        |> expectResponses
                            [ { resolve = "first"
                              , status = 200
                              , headers = ( "set-cookie", "session=; Path=/; Max-Age=0; HttpOnly; SameSite=Lax; Secure" ) :: jsonHeaders
                              , body = "{}"
                              }
                            ]
            , test "clears the cookie at once when there is no session" <|
                \() ->
                    start (Just databaseUrl)
                        |> session "DELETE" []
                        |> expectSent (responses >> List.map .status) [ 200 ]
            , test "answers 503 and logs why when the database fails" <|
                \() ->
                    start (Just databaseUrl)
                        |> session "DELETE" [ ( "cookie", "session=c2Vzc2lvbg" ) ]
                        |> ProgramTest.simulateHttpResponse "POST" endpoint Test.Http.networkError
                        |> expectSent (\values -> ( logs values, List.map .status (responses values) ))
                            ( [ ( "info", "Sending queries to " ++ endpoint )
                              , ( "warn", "No PADDLE_* settings, so billing routes answer 503." )
                              , ( "warn", "No R2_* settings, so publishing answers 503." )
                              , ( "warn", "No MODERATION_EMAIL, so reports send no email." )
                              , ( "error", "Sign-out failed: Could not reach Neon." )
                              ]
                            , [ 503 ]
                            )
            ]
        , describe "GET /api/auth/passkeys"
            [ test "lists the passkeys of the session's account" <|
                \() ->
                    start (Just databaseUrl)
                        |> passkeys "GET" [ "api", "auth", "passkeys" ] ""
                        |> ProgramTest.ensureHttpRequest "POST"
                            endpoint
                            (Test.Http.expectJsonBody queries
                                (Expect.equal
                                    [ ( "SELECT authenticate($1) AS user_id", [ "c2Vzc2lvbg" ] )
                                    , ( """SELECT credential_id, name, to_char(created_at AT TIME ZONE 'UTC', 'YYYY-MM-DD"T"HH24:MI:SS"Z"') AS created_at FROM passkeys ORDER BY passkeys.created_at"""
                                      , []
                                      )
                                    ]
                                )
                            )
                        |> ProgramTest.simulateHttpOk "POST"
                            endpoint
                            """{"results":[{"rows":[{"user_id":"7b0c9a52-3d0e-4a53-9a7e-0f6f1f3f5b11"}]},{"rows":[{"credential_id":"oKGio6SlpqeoqaqrrK2urw","name":"Apple Passwords","created_at":"2026-11-10T09:30:00Z"}]}]}"""
                        |> expectResponses
                            [ { resolve = "first"
                              , status = 200
                              , headers = jsonHeaders
                              , body = """[{"id":"oKGio6SlpqeoqaqrrK2urw","name":"Apple Passwords","createdAt":"2026-11-10T09:30:00Z"}]"""
                              }
                            ]
            , test "answers 401 when the session is over" <|
                \() ->
                    start (Just databaseUrl)
                        |> passkeys "GET" [ "api", "auth", "passkeys" ] ""
                        |> ProgramTest.simulateHttpOk "POST" endpoint """{"results":[{"rows":[{"user_id":null}]},{"rows":[]}]}"""
                        |> expectResponses
                            [ { resolve = "first", status = 401, headers = jsonHeaders, body = """{"error":"Not signed in."}""" } ]
            , test "answers 401 at once without a cookie" <|
                \() ->
                    start (Just databaseUrl)
                        |> request "first" "GET" [ "api", "auth", "passkeys" ]
                        |> expectSent (responses >> List.map .status) [ 401 ]
            ]
        , describe "POST /api/auth/passkeys/options"
            [ test "answers with a challenge that lasts 5 minutes, for this site and this account" <|
                \() ->
                    start (Just databaseUrl)
                        |> passkeys "POST" [ "api", "auth", "passkeys", "options" ] ""
                        |> ProgramTest.ensureHttpRequest "POST"
                            endpoint
                            (Test.Http.expectJsonBody (Decode.map (List.map Tuple.second) queries)
                                (Expect.equal [ [ "c2Vzc2lvbg" ], [], [ "300" ], [], [] ])
                            )
                        |> ProgramTest.simulateHttpOk "POST" endpoint started
                        |> expectResponses
                            [ { resolve = "first"
                              , status = 200
                              , headers = jsonHeaders
                              , body = """{"challenge":"Y2hhbGxlbmdl","rp":{"id":"pixo.pics","name":"pixo"},"user":{"id":"7b0c9a52-3d0e-4a53-9a7e-0f6f1f3f5b11","name":"ada@example.com","displayName":"ada"},"algorithms":[-8,-7,-257],"excludeCredentials":["b2xkZXI"]}"""
                              }
                            ]
            , test "answers 401 when the session is over" <|
                \() ->
                    start (Just databaseUrl)
                        |> passkeys "POST" [ "api", "auth", "passkeys", "options" ] ""
                        |> ProgramTest.simulateHttpOk "POST"
                            endpoint
                            """{"results":[{"rows":[{"user_id":null}]},{"rows":[],"rowCount":0},{"rows":[]},{"rows":[]},{"rows":[]}]}"""
                        |> expectSent (responses >> List.map .status) [ 401 ]
            , test "answers 503 and logs why when the database fails" <|
                \() ->
                    start (Just databaseUrl)
                        |> passkeys "POST" [ "api", "auth", "passkeys", "options" ] ""
                        |> ProgramTest.simulateHttpResponse "POST" endpoint Test.Http.networkError
                        |> expectSent (\values -> ( levels values, List.map .status (responses values) ))
                            ( [ "info", "warn", "warn", "warn", "error" ], [ 503 ] )
            ]
        , describe "POST /api/auth/passkeys"
            [ test "saves the credential, named after what made it, once its challenge is used" <|
                \() ->
                    start (Just databaseUrl)
                        |> passkeys "POST" [ "api", "auth", "passkeys" ] (attestation clientData)
                        |> ProgramTest.ensureHttpRequest "POST"
                            endpoint
                            (Test.Http.expectJsonBody (Decode.map (List.map Tuple.second) queries)
                                (Expect.equal
                                    [ [ "c2Vzc2lvbg" ]
                                    , [ "Y2hhbGxlbmdl"
                                      , "oKGio6SlpqeoqaqrrK2urw"
                                      , "pQECAyYgASFYIAECAwQFBgcICQoLDA0ODxAREhMUFRYXGBkaGxwdHh8gIlgg__79_Pv6-fj39vX08_Lx8O_u7ezr6uno5-bl5OPi4eA"
                                      , "0"
                                      , """["internal","hybrid"]"""
                                      , "true"
                                      , "true"
                                      , "Apple Passwords"
                                      ]
                                    , []
                                    ]
                                )
                            )
                        |> ProgramTest.simulateHttpOk "POST"
                            endpoint
                            """{"results":[{"rows":[{"user_id":"7b0c9a52-3d0e-4a53-9a7e-0f6f1f3f5b11"}]},{"rows":[{"credential_id":"oKGio6SlpqeoqaqrrK2urw","name":"Apple Passwords","created_at":"2026-11-10T09:30:00Z"}]},{"rows":[{"email":"ada@example.com","username":"ada"}]}]}"""
                        |> ProgramTest.ensureHttpRequest "POST"
                            BrevoMailer.endpoint
                            (Test.Http.expectJsonBody (notice "Apple Passwords")
                                (Expect.equal
                                    { to = [ "ada@example.com" ]
                                    , subject = "A passkey was added to your pixo account"
                                    , tags = [ "passkey-added" ]
                                    , told = True
                                    }
                                )
                            )
                        |> ProgramTest.simulateHttpOk "POST" BrevoMailer.endpoint """{"messageId":"<1@smtp-relay.mailin.fr>"}"""
                        |> expectResponses [ added ]
            , test "answers 201 all the same, and logs why, when the email fails" <|
                \() ->
                    start (Just databaseUrl)
                        |> passkeys "POST" [ "api", "auth", "passkeys" ] (attestation clientData)
                        |> ProgramTest.simulateHttpOk "POST" endpoint saved
                        |> ProgramTest.simulateHttpResponse "POST" BrevoMailer.endpoint Test.Http.networkError
                        |> expectSent (\values -> ( levels values, responses values ))
                            ( [ "info", "warn", "warn", "warn", "error" ], [ added ] )
            , test "prints the notice instead of emailing it when no key is set locally" <|
                \() ->
                    startWith { databaseUrl = Just databaseUrl, brevoApiKey = Nothing, emailFrom = Just "hello@pixo.pics", origin = "https://pixo.pics", local = True, paddle = Nothing, storage = Nothing, moderationEmail = Nothing }
                        |> passkeys "POST" [ "api", "auth", "passkeys" ] (attestation clientData)
                        |> ProgramTest.simulateHttpOk "POST" endpoint saved
                        |> ProgramTest.ensureHttpRequests "POST" BrevoMailer.endpoint (List.length >> Expect.equal 0)
                        |> expectSent
                            (\values ->
                                ( levels values
                                , List.any (Tuple.second >> String.contains "To: ada@example.com") (logs values)
                                , responses values
                                )
                            )
                            ( [ "info", "warn", "warn", "warn", "warn", "info" ], True, [ added ] )
            , test "answers 409 when the challenge is expired, used or unknown" <|
                \() ->
                    start (Just databaseUrl)
                        |> passkeys "POST" [ "api", "auth", "passkeys" ] (attestation clientData)
                        |> ProgramTest.simulateHttpOk "POST"
                            endpoint
                            """{"results":[{"rows":[{"user_id":"7b0c9a52-3d0e-4a53-9a7e-0f6f1f3f5b11"}]},{"rows":[]},{"rows":[{"email":"ada@example.com","username":"ada"}]}]}"""
                        |> expectResponses
                            [ { resolve = "first"
                              , status = 409
                              , headers = jsonHeaders
                              , body = """{"error":"This passkey request expired."}"""
                              }
                            ]
            , test "answers 422, asks the database nothing and logs why when the passkey is from another site" <|
                \() ->
                    start (Just databaseUrl)
                        |> passkeys "POST" [ "api", "auth", "passkeys" ] (attestation clientDataElsewhere)
                        |> ProgramTest.ensureHttpRequests "POST" endpoint (List.length >> Expect.equal 0)
                        |> expectSent (\values -> ( logs values, responses values ))
                            ( [ ( "info", "Sending queries to " ++ endpoint )
                              , ( "warn", "No PADDLE_* settings, so billing routes answer 503." )
                              , ( "warn", "No R2_* settings, so publishing answers 503." )
                              , ( "warn", "No MODERATION_EMAIL, so reports send no email." )
                              , ( "warn", "Passkey refused: The client data comes from https://evil.example, not from https://pixo.pics." )
                              ]
                            , [ { resolve = "first"
                                , status = 422
                                , headers = jsonHeaders
                                , body = """{"error":"This passkey couldn't be verified."}"""
                                }
                              ]
                            )
            , test "answers 400 to a body that is not a passkey" <|
                \() ->
                    start (Just databaseUrl)
                        |> passkeys "POST" [ "api", "auth", "passkeys" ] "{}"
                        |> expectSent (responses >> List.map .status) [ 400 ]
            , test "answers 401 when the session is over" <|
                \() ->
                    start (Just databaseUrl)
                        |> passkeys "POST" [ "api", "auth", "passkeys" ] (attestation clientData)
                        |> ProgramTest.simulateHttpOk "POST" endpoint """{"results":[{"rows":[{"user_id":null}]},{"rows":[]},{"rows":[]}]}"""
                        |> expectSent (responses >> List.map .status) [ 401 ]
            ]
        , describe "DELETE /api/auth/passkeys/<id>"
            [ test "removes the account's passkey and emails the account" <|
                \() ->
                    start (Just databaseUrl)
                        |> passkeys "DELETE" [ "api", "auth", "passkeys", "oKGio6SlpqeoqaqrrK2urw" ] ""
                        |> ProgramTest.ensureHttpRequest "POST"
                            endpoint
                            (Test.Http.expectJsonBody queries
                                (Expect.equal
                                    [ ( "SELECT authenticate($1) AS user_id", [ "c2Vzc2lvbg" ] )
                                    , ( "DELETE FROM passkeys WHERE credential_id = $1 RETURNING name", [ "oKGio6SlpqeoqaqrrK2urw" ] )
                                    , ( "SELECT email, username FROM users", [] )
                                    ]
                                )
                            )
                        |> ProgramTest.simulateHttpOk "POST"
                            endpoint
                            """{"results":[{"rows":[{"user_id":"7b0c9a52-3d0e-4a53-9a7e-0f6f1f3f5b11"}]},{"rows":[{"name":"Apple Passwords"}]},{"rows":[{"email":"ada@example.com","username":"ada"}]}]}"""
                        |> ProgramTest.ensureHttpRequest "POST"
                            BrevoMailer.endpoint
                            (Test.Http.expectJsonBody (notice "Apple Passwords")
                                (Expect.equal
                                    { to = [ "ada@example.com" ]
                                    , subject = "A passkey was removed from your pixo account"
                                    , tags = [ "passkey-removed" ]
                                    , told = True
                                    }
                                )
                            )
                        |> ProgramTest.simulateHttpOk "POST" BrevoMailer.endpoint """{"messageId":"<1@smtp-relay.mailin.fr>"}"""
                        |> expectResponses
                            [ { resolve = "first", status = 200, headers = jsonHeaders, body = "{}" } ]
            , test "answers 404 and emails nobody when the account has no such passkey" <|
                \() ->
                    start (Just databaseUrl)
                        |> passkeys "DELETE" [ "api", "auth", "passkeys", "oKGio6SlpqeoqaqrrK2urw" ] ""
                        |> ProgramTest.simulateHttpOk "POST"
                            endpoint
                            """{"results":[{"rows":[{"user_id":"7b0c9a52-3d0e-4a53-9a7e-0f6f1f3f5b11"}]},{"rows":[]},{"rows":[{"email":"ada@example.com","username":"ada"}]}]}"""
                        |> ProgramTest.ensureHttpRequests "POST" BrevoMailer.endpoint (List.length >> Expect.equal 0)
                        |> expectResponses
                            [ { resolve = "first"
                              , status = 404
                              , headers = jsonHeaders
                              , body = """{"error":"This account has no such passkey."}"""
                              }
                            ]
            , test "answers 401 when the session is over" <|
                \() ->
                    start (Just databaseUrl)
                        |> passkeys "DELETE" [ "api", "auth", "passkeys", "oKGio6SlpqeoqaqrrK2urw" ] ""
                        |> ProgramTest.simulateHttpOk "POST" endpoint """{"results":[{"rows":[{"user_id":null}]},{"rows":[]},{"rows":[]}]}"""
                        |> expectSent (responses >> List.map .status) [ 401 ]
            ]
        , describe "POST /api/auth/signin/passkey/options"
            [ test "answers with a challenge that lasts 5 minutes" <|
                \() ->
                    start (Just databaseUrl)
                        |> signInOptions
                        |> ProgramTest.ensureHttpRequest "POST"
                            endpoint
                            (Test.Http.expectJsonBody
                                (Decode.map2 Tuple.pair
                                    (Decode.field "query" Decode.string)
                                    (Decode.field "params" (Decode.list Decode.string))
                                )
                                (Expect.equal ( "SELECT create_authentication_challenge($1) AS challenge", [ "300" ] ))
                            )
                        |> ProgramTest.simulateHttpOk "POST" endpoint """{"rows":[{"challenge":"Y2hhbGxlbmdl"}],"rowCount":1}"""
                        |> expectResponses
                            [ { resolve = "first"
                              , status = 200
                              , headers = jsonHeaders
                              , body = """{"challenge":"Y2hhbGxlbmdl","rpId":"pixo.pics"}"""
                              }
                            ]
            , test "answers 429 to the 11th challenge in 15 minutes from one address, without the database" <|
                \() ->
                    start (Just databaseUrl)
                        |> repeat 10 signInOptionsAs
                        |> signInOptionsAs "11"
                        |> ProgramTest.ensureHttpRequests "POST" endpoint (List.length >> Expect.equal 10)
                        |> expectResponses
                            [ { resolve = "11"
                              , status = 429
                              , headers = ( "retry-after", "90" ) :: jsonHeaders
                              , body = """{"error":"Too many attempts."}"""
                              }
                            ]
            ]
        , describe "POST /api/auth/signin/passkey"
            [ test "finds the key, has the signature checked, then starts a session in a cookie" <|
                \() ->
                    start (Just databaseUrl)
                        |> passkeySignIn assertion
                        |> ProgramTest.ensureHttpRequest "POST"
                            endpoint
                            (Test.Http.expectJsonBody
                                (Decode.map2 Tuple.pair
                                    (Decode.field "query" Decode.string)
                                    (Decode.field "params" (Decode.list Decode.string))
                                )
                                (Expect.equal ( "SELECT passkey_public_key($1) AS public_key", [ "oKGio6SlpqeoqaqrrK2urw" ] ))
                            )
                        |> ProgramTest.simulateHttpOk "POST" endpoint keyFound
                        |> ProgramTest.ensureOutgoingPortValues "interopFromElm"
                            sent
                            (checks
                                >> Expect.equal
                                    [ { resolve = "first"
                                      , key = ( "ES256", "AQIDBAUGBwgJCgsMDQ4PEBESExQVFhcYGRobHB0eHyA", "__79_Pv6-fj39vX08_Lx8O_u7ezr6uno5-bl5OPi4eA" )
                                      , signed = "hc4asGtQ0lfqei0FVSxvDEzvgNF-FdmbKJXm3Ic-1uwdAAAAAGGrdV6kson9YunnR9B0osE5k1x-xRe64NBdqS2heNa4"
                                      , signature = "gIGCg4SFhoeIiYqLjI2Oj5CRkpOUlZaXmJmam5ydnp8AEBESExQVFhcYGRobHB0eHyAhIiMkJSYnKCkqKywtLg"
                                      , signIn = ( "Y2hhbGxlbmdl", "oKGio6SlpqeoqaqrrK2urw", ( 0, True ) )
                                      }
                                    ]
                            )
                        |> signatureChecked True
                        |> ProgramTest.ensureHttpRequest "POST"
                            endpoint
                            (Test.Http.expectJsonBody
                                (Decode.map2 Tuple.pair
                                    (Decode.field "query" Decode.string)
                                    (Decode.field "params" (Decode.list Decode.string))
                                )
                                (Expect.equal
                                    ( "SELECT outcome, session_token, email, username FROM sign_in_with_passkey($1, $2, $3, $4, $5)"
                                    , [ "Y2hhbGxlbmdl", "oKGio6SlpqeoqaqrrK2urw", "0", "true", "2592000" ]
                                    )
                                )
                            )
                        |> ProgramTest.simulateHttpOk "POST"
                            endpoint
                            """{"rows":[{"outcome":"signed-in","session_token":"c2Vzc2lvbg","email":"ada@example.com","username":"ada"}],"rowCount":1}"""
                        |> expectResponses
                            [ { resolve = "first"
                              , status = 200
                              , headers =
                                    ( "set-cookie", "session=c2Vzc2lvbg; Path=/; Max-Age=2592000; HttpOnly; SameSite=Lax; Secure" )
                                        :: jsonHeaders
                              , body = """{"email":"ada@example.com","username":"ada"}"""
                              }
                            ]
            , test "answers 401, starts no session and logs why when the signature is wrong" <|
                \() ->
                    start (Just databaseUrl)
                        |> passkeySignIn assertion
                        |> ProgramTest.simulateHttpOk "POST" endpoint keyFound
                        |> signatureChecked False
                        |> ProgramTest.ensureHttpRequests "POST" endpoint (List.length >> Expect.equal 0)
                        |> expectSent (\values -> ( logs values, responses values ))
                            ( [ ( "info", "Sending queries to " ++ endpoint )
                              , ( "warn", "No PADDLE_* settings, so billing routes answer 503." )
                              , ( "warn", "No R2_* settings, so publishing answers 503." )
                              , ( "warn", "No MODERATION_EMAIL, so reports send no email." )
                              , ( "warn", "Passkey sign-in refused: The signature does not match the passkey." )
                              ]
                            , [ notVerified ]
                            )
            , test "answers 401 to a credential nobody has" <|
                \() ->
                    start (Just databaseUrl)
                        |> passkeySignIn assertion
                        |> ProgramTest.simulateHttpOk "POST" endpoint """{"rows":[{"public_key":null}],"rowCount":1}"""
                        |> expectResponses [ notVerified ]
            , test "answers 401 when the challenge is expired, used or unknown" <|
                \() ->
                    start (Just databaseUrl)
                        |> passkeySignIn assertion
                        |> ProgramTest.simulateHttpOk "POST" endpoint keyFound
                        |> signatureChecked True
                        |> ProgramTest.simulateHttpOk "POST"
                            endpoint
                            """{"rows":[{"outcome":"no-challenge","session_token":null,"email":null,"username":null}],"rowCount":1}"""
                        |> expectResponses [ notVerified ]
            , test "answers 401 when the counter did not move forward" <|
                \() ->
                    start (Just databaseUrl)
                        |> passkeySignIn assertion
                        |> ProgramTest.simulateHttpOk "POST" endpoint keyFound
                        |> signatureChecked True
                        |> ProgramTest.simulateHttpOk "POST"
                            endpoint
                            """{"rows":[{"outcome":"no-passkey","session_token":null,"email":null,"username":null}],"rowCount":1}"""
                        |> expectSent (\values -> ( logs values |> List.drop 4, responses values ))
                            ( [ ( "warn", "Passkey sign-in refused: The passkey is gone, or its counter did not move forward." ) ]
                            , [ notVerified ]
                            )
            , test "answers 401 and asks the database nothing when the passkey was used on another site" <|
                \() ->
                    start (Just databaseUrl)
                        |> passkeySignIn (String.replace "hc4asGtQ0lfqei0FVSxvDEzvgNF-FdmbKJXm3Ic-1uwdAAAAAA" "o3mm9u6vuaVeN4wRgDTidR5oL6ufLTCrE9ISVYbOGUcdAAAAAA" assertion)
                        |> ProgramTest.ensureHttpRequests "POST" endpoint (List.length >> Expect.equal 0)
                        |> expectResponses [ notVerified ]
            , test "answers 400 to a body that is not an assertion" <|
                \() ->
                    start (Just databaseUrl)
                        |> passkeySignIn "{}"
                        |> expectSent (responses >> List.map .status) [ 400 ]
            ]
        , describe "POST /api/billing/checkout"
            [ test "creates a transaction for the Pro price and the session's user" <|
                \() ->
                    startPaid
                        |> checkoutRequest "first"
                        |> ProgramTest.simulateHttpOk "POST" endpoint (checkoutStateAnswer "false" "null")
                        |> ProgramTest.ensureHttpRequest "POST"
                            "https://sandbox-api.paddle.com/transactions"
                            (\made ->
                                Expect.all
                                    [ Test.Http.hasHeader "authorization" "Bearer pdl_sdbx_apikey_test"
                                    , Test.Http.hasHeader "paddle-version" "1"
                                    , Test.Http.expectJsonBody Decode.value
                                        (Encode.encode 0
                                            >> Expect.equal """{"items":[{"price_id":"pri_pro","quantity":1}],"custom_data":{"user_id":"6f1c1f0e-7f3a-4c2b-9a51-0d4b1d7c8e21"}}"""
                                        )
                                    ]
                                    made
                            )
                        |> ProgramTest.simulateHttpResponse "POST"
                            "https://sandbox-api.paddle.com/transactions"
                            (Test.Http.httpResponse { statusCode = 201, headers = [], body = """{"data":{"id":"txn_01"}}""" })
                        |> expectResponses
                            [ { resolve = "first"
                              , status = 200
                              , headers = jsonHeaders
                              , body = """{"transactionId":"txn_01","clientToken":"test_token","environment":"sandbox"}"""
                              }
                            ]
            , test "adds the customer when the user has had a subscription" <|
                \() ->
                    startPaid
                        |> checkoutRequest "first"
                        |> ProgramTest.simulateHttpOk "POST" endpoint (checkoutStateAnswer "false" "\"ctm_01hv8x2a0e9xjn3k0q0p1y3z4c\"")
                        |> ProgramTest.expectHttpRequest "POST"
                            "https://sandbox-api.paddle.com/transactions"
                            (Test.Http.expectJsonBody Decode.value
                                (Encode.encode 0
                                    >> Expect.equal """{"items":[{"price_id":"pri_pro","quantity":1}],"custom_data":{"user_id":"6f1c1f0e-7f3a-4c2b-9a51-0d4b1d7c8e21"},"customer_id":"ctm_01hv8x2a0e9xjn3k0q0p1y3z4c"}"""
                                )
                            )
            , test "answers 409 without calling Paddle when the user already has Pro" <|
                \() ->
                    startPaid
                        |> checkoutRequest "first"
                        |> ProgramTest.simulateHttpOk "POST" endpoint (checkoutStateAnswer "true" "\"ctm_01hv8x2a0e9xjn3k0q0p1y3z4c\"")
                        |> ProgramTest.ensureHttpRequests "POST" "https://sandbox-api.paddle.com/transactions" (List.length >> Expect.equal 0)
                        |> expectResponses
                            [ { resolve = "first", status = 409, headers = jsonHeaders, body = """{"error":"You already have Pro."}""" } ]
            , test "answers 401 when the session is over" <|
                \() ->
                    startPaid
                        |> checkoutRequest "first"
                        |> ProgramTest.simulateHttpOk "POST" endpoint """{"results":[{"rows":[{"user_id":null}]},{"rows":[{"pro":false,"customer_id":null}]}]}"""
                        |> expectSent (responses >> List.map .status) [ 401 ]
            , test "answers 503 and logs why when Paddle refuses" <|
                \() ->
                    startPaid
                        |> checkoutRequest "first"
                        |> ProgramTest.simulateHttpOk "POST" endpoint (checkoutStateAnswer "false" "null")
                        |> ProgramTest.simulateHttpResponse "POST"
                            "https://sandbox-api.paddle.com/transactions"
                            (Test.Http.httpResponse { statusCode = 400, headers = [], body = """{"error":{"type":"request_error","code":"bad_request","detail":"Invalid price."}}""" })
                        |> expectSent (\values -> ( List.map .status (responses values), List.drop 2 (logs values) ))
                            ( [ 503 ]
                            , [ ( "error", "Paddle checkout failed: Paddle answered 400 (bad_request): Invalid price." ) ]
                            )
            , test "answers 503 without Paddle configured" <|
                \() ->
                    start (Just databaseUrl)
                        |> checkoutRequest "first"
                        |> expectResponses
                            [ { resolve = "first", status = 503, headers = jsonHeaders, body = """{"error":"Unavailable right now."}""" } ]
            , test "answers 429 to the 11th checkout in 15 minutes from one address" <|
                \() ->
                    startPaid
                        |> repeat 10 checkoutRequest
                        |> checkoutRequest "11"
                        |> expectSent (\values -> List.filter (\{ status } -> status == 429) (responses values))
                            [ { resolve = "11"
                              , status = 429
                              , headers = ( "retry-after", "90" ) :: jsonHeaders
                              , body = """{"error":"Too many attempts."}"""
                              }
                            ]
            ]
        , describe "POST /api/billing/portal"
            [ test "answers 404 when the user has no subscription rows" <|
                \() ->
                    startPaid
                        |> portalRequest
                        |> ProgramTest.simulateHttpOk "POST" endpoint """{"results":[{"rows":[{"user_id":"6f1c1f0e-7f3a-4c2b-9a51-0d4b1d7c8e21"}]},{"rows":[]}]}"""
                        |> expectResponses
                            [ { resolve = "first", status = 404, headers = jsonHeaders, body = """{"error":"No billing account yet."}""" } ]
            , test "opens a portal session for the customer with the subscriptions that grant Pro" <|
                \() ->
                    startPaid
                        |> portalRequest
                        |> ProgramTest.simulateHttpOk "POST"
                            endpoint
                            ("""{"results":[{"rows":[{"user_id":"6f1c1f0e-7f3a-4c2b-9a51-0d4b1d7c8e21"}]},{"rows":["""
                                ++ subscriptionRow "active" "null" "null" "\"2026-12-17T10:00:00Z\""
                                ++ ","
                                ++ String.replace "sub_01hv8x2a0e9xjn3k0q0p1y3z4b" "sub_01hv8x2a0e9xjn3k0q0p1y3z4e" (subscriptionRow "canceled" "null" "null" "null")
                                ++ """]}]}"""
                            )
                        |> ProgramTest.ensureHttpRequest "POST"
                            "https://sandbox-api.paddle.com/customers/ctm_01hv8x2a0e9xjn3k0q0p1y3z4c/portal-sessions"
                            (\made ->
                                Expect.all
                                    [ Test.Http.hasHeader "authorization" "Bearer pdl_sdbx_apikey_test"
                                    , Test.Http.expectJsonBody Decode.value
                                        (Encode.encode 0 >> Expect.equal """{"subscription_ids":["sub_01hv8x2a0e9xjn3k0q0p1y3z4b"]}""")
                                    ]
                                    made
                            )
                        |> ProgramTest.simulateHttpResponse "POST"
                            "https://sandbox-api.paddle.com/customers/ctm_01hv8x2a0e9xjn3k0q0p1y3z4c/portal-sessions"
                            (Test.Http.httpResponse
                                { statusCode = 201
                                , headers = []
                                , body = """{"data":{"urls":{"general":{"overview":"https://sandbox-customer-portal.paddle.com/x"}}}}"""
                                }
                            )
                        |> expectResponses
                            [ { resolve = "first"
                              , status = 200
                              , headers = jsonHeaders
                              , body = """{"url":"https://sandbox-customer-portal.paddle.com/x"}"""
                              }
                            ]
            , test "answers 503 and logs why when Paddle fails" <|
                \() ->
                    startPaid
                        |> portalRequest
                        |> ProgramTest.simulateHttpOk "POST"
                            endpoint
                            ("""{"results":[{"rows":[{"user_id":"6f1c1f0e-7f3a-4c2b-9a51-0d4b1d7c8e21"}]},{"rows":["""
                                ++ subscriptionRow "active" "null" "null" "\"2026-12-17T10:00:00Z\""
                                ++ """]}]}"""
                            )
                        |> ProgramTest.simulateHttpResponse "POST"
                            "https://sandbox-api.paddle.com/customers/ctm_01hv8x2a0e9xjn3k0q0p1y3z4c/portal-sessions"
                            Test.Http.networkError
                        |> expectSent (\values -> ( List.map .status (responses values), List.drop 2 (logs values) ))
                            ( [ 503 ]
                            , [ ( "error", "Paddle portal failed: Could not reach Paddle." ) ]
                            )
            ]
        , describe "POST /api/billing/webhook"
            [ test "answers 503 when Paddle is not configured" <|
                \() ->
                    start (Just databaseUrl)
                        |> webhook "first" freshSignature
                        |> expectResponses [ { resolve = "first", status = 503, headers = jsonHeaders, body = """{"error":"Unavailable right now."}""" } ]
            , test "answers 400 without a paddle-signature header" <|
                \() ->
                    startPaid
                        |> requestWith { resolve = "first", method = "POST", path = [ "api", "billing", "webhook" ], headers = [], body = subscriptionUpdated }
                        |> expectSent (responses >> List.map .status) [ 400 ]
            , test "answers 401 and sends nothing to check when the timestamp is 6 seconds old" <|
                \() ->
                    startPaid
                        |> ProgramTest.advanceTime 6000
                        |> webhook "first" freshSignature
                        |> expectSent (\values -> ( List.map .status (responses values), List.length (webhookChecks values), levels values ))
                            ( [ 401 ], 0, [ "info", "warn", "warn" ] )
            , test "asks app.ts to check the timestamp and the body with the configured secret" <|
                \() ->
                    startPaid
                        |> webhook "first" freshSignature
                        |> expectSent webhookChecks
                            [ { resolve = "first"
                              , secret = "pdl_ntfset_test"
                              , signed = "0:" ++ subscriptionUpdated
                              , signatures = [ String.repeat 64 "a" ]
                              }
                            ]
            , test "answers 400 and logs only where a body did not decode" <|
                \() ->
                    startPaid
                        |> webhook "first" freshSignature
                        |> webhookChecked """{"event_id":"evt_1","event_type":"subscription.updated","data":{"id":"sub_1","status":"active"}}""" True
                        |> expectSent (\values -> ( List.map .status (responses values), List.drop 2 (logs values) ))
                            ( [ 400 ], [ ( "error", "Paddle webhook did not decode at data." ) ] )
            , test "answers 401 and warns when the check fails" <|
                \() ->
                    startPaid
                        |> webhook "first" freshSignature
                        |> webhookChecked subscriptionUpdated False
                        |> expectSent (\values -> ( List.map .status (responses values), List.drop 2 (logs values) ))
                            ( [ 401 ], [ ( "warn", "Paddle webhook refused: the signature does not match." ) ] )
            , test "applies a subscription for the Pro price" <|
                \() ->
                    startPaid
                        |> webhook "first" freshSignature
                        |> webhookChecked subscriptionUpdated True
                        |> ProgramTest.expectHttpRequest "POST"
                            endpoint
                            (Test.Http.expectJsonBody
                                (Decode.field "params" (Decode.list (Decode.nullable Decode.string)))
                                (Expect.equal
                                    [ Just "sub_01hv8x2a0e9xjn3k0q0p1y3z4b"
                                    , Just "6f1c1f0e-7f3a-4c2b-9a51-0d4b1d7c8e21"
                                    , Just "ctm_01hv8x2a0e9xjn3k0q0p1y3z4c"
                                    , Just "active"
                                    , Just "pri_pro"
                                    , Just "2026-12-17T10:00:00Z"
                                    , Nothing
                                    , Nothing
                                    , Just "2026-11-17T10:00:00.500000Z"
                                    ]
                                )
                            )
            , test "answers 200 once the subscription is applied" <|
                \() ->
                    startPaid
                        |> webhook "first" freshSignature
                        |> webhookChecked subscriptionUpdated True
                        |> ProgramTest.simulateHttpOk "POST" endpoint """{"rows":[{"outcome":"applied"}],"rowCount":1}"""
                        |> expectSent (\values -> ( responses values, List.drop 2 (logs values) ))
                            ( [ { resolve = "first", status = 200, headers = jsonHeaders, body = "{}" } ]
                            , [ ( "info", "Paddle subscription.updated evt_01hv8x2a0e9xjn3k0q0p1y3z4a applied." ) ]
                            )
            , test "answers 200 without a query for another price" <|
                \() ->
                    startPaid
                        |> webhook "first" freshSignature
                        |> webhookChecked (eventBody "subscription.updated" "pri_other") True
                        |> ProgramTest.ensureHttpRequests "POST" endpoint (List.length >> Expect.equal 0)
                        |> expectSent (responses >> List.map .status) [ 200 ]
            , test "answers 200 without a query for another kind of event" <|
                \() ->
                    startPaid
                        |> webhook "first" freshSignature
                        |> webhookChecked """{"event_id":"evt_01hv8x2a0e9xjn3k0q0p1y3z4a","event_type":"transaction.completed","occurred_at":"2026-11-17T10:00:01.000000Z","data":{"id":"txn_01hv8x2a0e9xjn3k0q0p1y3z4a"}}""" True
                        |> ProgramTest.ensureHttpRequests "POST" endpoint (List.length >> Expect.equal 0)
                        |> expectSent (responses >> List.map .status) [ 200 ]
            , test "answers 200 and warns when no account is named" <|
                \() ->
                    startPaid
                        |> webhook "first" freshSignature
                        |> webhookChecked subscriptionUpdated True
                        |> ProgramTest.simulateHttpOk "POST" endpoint """{"rows":[{"outcome":"unmatched"}],"rowCount":1}"""
                        |> expectSent (\values -> ( List.map .status (responses values), List.drop 2 (logs values) ))
                            ( [ 200 ]
                            , [ ( "warn", "Paddle subscription.updated evt_01hv8x2a0e9xjn3k0q0p1y3z4a names no account; skipped." ) ]
                            )
            , test "answers 503 when Neon fails" <|
                \() ->
                    startPaid
                        |> webhook "first" freshSignature
                        |> webhookChecked subscriptionUpdated True
                        |> ProgramTest.simulateHttpResponse "POST" endpoint Test.Http.networkError
                        |> expectSent (responses >> List.map .status) [ 503 ]
            , test "accepts 40 webhooks from one address within a minute" <|
                \() ->
                    startPaid
                        |> repeat 40 (\resolve -> webhook resolve freshSignature)
                        |> expectSent (\values -> ( List.length (webhookChecks values), List.filter (\{ status } -> status == 429) (responses values) ))
                            ( 40, [] )
            ]
        , describe "init"
            [ test "warns that emails are printed when no key is set locally" <|
                \() ->
                    startWith { databaseUrl = Just databaseUrl, brevoApiKey = Nothing, emailFrom = Just "hello@pixo.pics", origin = "https://pixo.pics", local = True, paddle = Nothing, storage = Nothing, moderationEmail = Nothing }
                        |> expectSent logs
                            [ ( "info", "Sending queries to " ++ endpoint )
                            , ( "warn", "No BREVO_API_KEY, so emails are printed here instead of sent." )
                            , ( "warn", "No PADDLE_* settings, so billing routes answer 503." )
                            , ( "warn", "No R2_* settings, so publishing answers 503." )
                            , ( "warn", "No MODERATION_EMAIL, so reports send no email." )
                            ]
            , test "is broken, and logs why, when email isn't configured on Vercel" <|
                \() ->
                    startWith { databaseUrl = Just databaseUrl, brevoApiKey = Nothing, emailFrom = Just "hello@pixo.pics", origin = "https://pixo.pics", local = False, paddle = Nothing, storage = Nothing, moderationEmail = Nothing }
                        |> session "GET" [ ( "cookie", "session=c2Vzc2lvbg" ) ]
                        |> expectSent (\values -> ( logs values, List.map .status (responses values) ))
                            ( [ ( "error", "No email, so every route but health answers 503: BREVO_API_KEY and EMAIL_FROM must both be set." ) ]
                            , [ 503 ]
                            )
            , test "fails the health check when email isn't configured on Vercel" <|
                \() ->
                    startWith { databaseUrl = Just databaseUrl, brevoApiKey = Nothing, emailFrom = Just "hello@pixo.pics", origin = "https://pixo.pics", local = False, paddle = Nothing, storage = Nothing, moderationEmail = Nothing }
                        |> request "first" "GET" [ "api", "health" ]
                        |> expectResponses
                            [ health "first"
                                503
                                """{"status":"fail","releaseId":"0641607","region":"arn1","checks":{"database":{"status":"fail","output":"Not configured: BREVO_API_KEY and EMAIL_FROM must both be set."}}}"""
                            ]
            , test "is broken, and logs why, when the sender isn't configured on Vercel" <|
                \() ->
                    startWith { databaseUrl = Just databaseUrl, brevoApiKey = Just "xkeysib-secret", emailFrom = Nothing, origin = "https://pixo.pics", local = False, paddle = Nothing, storage = Nothing, moderationEmail = Nothing }
                        |> session "GET" [ ( "cookie", "session=c2Vzc2lvbg" ) ]
                        |> expectSent (\values -> ( logs values, List.map .status (responses values) ))
                            ( [ ( "error", "No email, so every route but health answers 503: BREVO_API_KEY and EMAIL_FROM must both be set." ) ]
                            , [ 503 ]
                            )
            , test "is broken, and logs why, when the sender is not an address" <|
                \() ->
                    startWith { databaseUrl = Just databaseUrl, brevoApiKey = Just "xkeysib-secret", emailFrom = Just "hello", origin = "https://pixo.pics", local = False, paddle = Nothing, storage = Nothing, moderationEmail = Nothing }
                        |> session "GET" [ ( "cookie", "session=c2Vzc2lvbg" ) ]
                        |> expectSent (\values -> ( logs values, List.map .status (responses values) ))
                            ( [ ( "error", "No email, so every route but health answers 503: EMAIL_FROM is not an email address." ) ]
                            , [ 503 ]
                            )
            , test "answers 503 and logs why when the origin is not a URL" <|
                \() ->
                    startWith { databaseUrl = Just databaseUrl, brevoApiKey = Just "xkeysib-secret", emailFrom = Just "hello@pixo.pics", origin = "pixo.pics", local = False, paddle = Nothing, storage = Nothing, moderationEmail = Nothing }
                        |> session "GET" [ ( "cookie", "session=c2Vzc2lvbg" ) ]
                        |> expectSent (\values -> ( logs values, List.map .status (responses values) ))
                            ( [ ( "error", "Bad origin: Expected an origin like https://pixo.pics, got: pixo.pics" ) ]
                            , [ 503 ]
                            )
            , test "fails the health check when the origin is not a URL" <|
                \() ->
                    startWith { databaseUrl = Just databaseUrl, brevoApiKey = Just "xkeysib-secret", emailFrom = Just "hello@pixo.pics", origin = "pixo.pics", local = False, paddle = Nothing, storage = Nothing, moderationEmail = Nothing }
                        |> request "first" "GET" [ "api", "health" ]
                        |> expectSent (responses >> List.map .status) [ 503 ]
            , test "still answers 404 at an unknown path when the origin is not a URL" <|
                \() ->
                    startWith { databaseUrl = Just databaseUrl, brevoApiKey = Just "xkeysib-secret", emailFrom = Just "hello@pixo.pics", origin = "pixo.pics", local = False, paddle = Nothing, storage = Nothing, moderationEmail = Nothing }
                        |> request "first" "GET" [ "api", "nowhere" ]
                        |> expectSent (responses >> List.map .status) [ 404 ]
            ]
        , describe "moderation settings"
            [ test "logs an error and sends no report email when MODERATION_EMAIL is not an address" <|
                \() ->
                    startWith { databaseUrl = Just databaseUrl, brevoApiKey = Just "xkeysib-secret", emailFrom = Just "hello@pixo.pics", origin = "https://pixo.pics", local = False, paddle = Nothing, storage = Nothing, moderationEmail = Just "mod" }
                        |> expectSent (logs >> List.filter (Tuple.first >> (==) "error"))
                            [ ( "error", "MODERATION_EMAIL is not an email address." ) ]
            ]
        , describe "every route"
            [ test "answers 429 to the 31st request in a minute from one address, whatever it asks" <|
                \() ->
                    start (Just databaseUrl)
                        |> repeat 30 (nowhereFrom "203.0.113.7")
                        |> nowhereFrom "203.0.113.7" "31"
                        |> expectSent (responses >> List.filter (\{ status } -> status == 429))
                            [ { resolve = "31"
                              , status = 429
                              , headers = ( "retry-after", "2" ) :: jsonHeaders
                              , body = """{"error":"Too many attempts."}"""
                              }
                            ]
            , test "counts each address apart" <|
                \() ->
                    start (Just databaseUrl)
                        |> repeat 30 (nowhereFrom "203.0.113.7")
                        |> nowhereFrom "198.51.100.4" "31"
                        |> expectSent (responses >> List.filter (\{ status } -> status == 429)) []
            , test "lets an address in again once its wait is over" <|
                \() ->
                    start (Just databaseUrl)
                        |> repeat 30 (nowhereFrom "203.0.113.7")
                        |> ProgramTest.advanceTime 2000
                        |> nowhereFrom "203.0.113.7" "31"
                        |> expectSent (responses >> List.filter (\{ status } -> status == 429)) []
            ]
        , describe "routing"
            [ test "answers 404 at an unknown path" <|
                \() ->
                    start (Just databaseUrl)
                        |> request "first" "GET" [ "api", "nowhere" ]
                        |> expectResponses
                            [ { resolve = "first"
                              , status = 404
                              , headers = jsonHeaders
                              , body = """{"error":"Not found."}"""
                              }
                            ]
            , test "answers 405 with the methods the path allows" <|
                \() ->
                    start (Just databaseUrl)
                        |> request "first" "POST" [ "api", "health" ]
                        |> expectResponses
                            [ { resolve = "first"
                              , status = 405
                              , headers = ( "allow", "GET" ) :: jsonHeaders
                              , body = """{"error":"Method not allowed."}"""
                              }
                            ]
            ]
        ]


type alias App =
    ProgramTest Main.Model Main.Msg (Effect.Effect Main.Msg)


type alias Response =
    { resolve : String
    , status : Int
    , headers : List ( String, String )
    , body : String
    }


type Sent
    = Responded Response
    | Logged ( String, String )
    | Asked Check
    | AskedWebhook WebhookCheck


type alias WebhookCheck =
    { resolve : String
    , secret : String
    , signed : String
    , signatures : List String
    }


type alias Check =
    { resolve : String
    , key : ( String, String, String )
    , signed : String
    , signature : String
    , signIn : ( String, String, ( Int, Bool ) )
    }


databaseUrl : String
databaseUrl =
    "postgresql://api:secret@ep-test-123456.eu-central-1.aws.neon.tech/app"


endpoint : String
endpoint =
    "https://api.eu-central-1.aws.neon.tech/sql"


notExempt : String
notExempt =
    """{"rows":[{"exempt":false}],"rowCount":1}"""


linked : String -> String
linked outcome =
    """{"rows":[{"outcome":\""""
        ++ outcome
        ++ """","token":"Zm9vYmFy"}],"rowCount":1}"""


type alias Email =
    { from : String
    , to : List String
    , subject : String
    , tags : List String
    , linked : Bool
    }


email : Decode.Decoder Email
email =
    Decode.map5 Email
        (Decode.at [ "sender", "email" ] Decode.string)
        (Decode.field "to" (Decode.list (Decode.field "email" Decode.string)))
        (Decode.field "subject" Decode.string)
        (Decode.field "tags" (Decode.list Decode.string))
        (Decode.map2
            (\html text ->
                List.all
                    (\content ->
                        String.contains "https://pixo.pics/verify#Zm9vYmFy" content
                    )
                    [ html, text ]
            )
            (Decode.field "htmlContent" Decode.string)
            (Decode.field "textContent" Decode.string)
        )


consumed : String -> String
consumed outcome =
    if outcome == "verified" then
        """{"rows":[{"outcome":"verified","session_token":"c2Vzc2lvbg","email":"ada@example.com","username":"ada"}],"rowCount":1}"""

    else
        """{"rows":[{"outcome":\""""
            ++ outcome
            ++ """","session_token":null,"email":null,"username":null}],"rowCount":1}"""


verify : String -> List ( String, String ) -> App -> App
verify method headers =
    requestWith { resolve = "first", method = method, path = [ "api", "auth", "verify" ], headers = headers, body = "" }


signIn : String -> App -> App
signIn body =
    requestWith
        { resolve = "first"
        , method = "POST"
        , path = [ "api", "auth", "signin" ]
        , headers = [ ( "content-type", "application/json" ), ( "x-real-ip", "203.0.113.7" ) ]
        , body = body
        }


session : String -> List ( String, String ) -> App -> App
session method headers =
    requestWith { resolve = "first", method = method, path = [ "api", "auth", "session" ], headers = headers, body = "" }


signInOptions : App -> App
signInOptions =
    signInOptionsAs "first"


signInOptionsAs : String -> App -> App
signInOptionsAs resolve =
    requestWith
        { resolve = resolve
        , method = "POST"
        , path = [ "api", "auth", "signin", "passkey", "options" ]
        , headers = [ ( "x-real-ip", "203.0.113.7" ) ]
        , body = ""
        }


passkeySignIn : String -> App -> App
passkeySignIn body =
    requestWith
        { resolve = "first"
        , method = "POST"
        , path = [ "api", "auth", "signin", "passkey" ]
        , headers = [ ( "content-type", "application/json" ) ]
        , body = body
        }


assertion : String
assertion =
    """{"id":"oKGio6SlpqeoqaqrrK2urw","clientDataJSON":"eyJ0eXBlIjoid2ViYXV0aG4uZ2V0IiwiY2hhbGxlbmdlIjoiWTJoaGJHeGxibWRsIiwib3JpZ2luIjoiaHR0cHM6Ly9waXhvLnBpY3MifQ","authenticatorData":"hc4asGtQ0lfqei0FVSxvDEzvgNF-FdmbKJXm3Ic-1uwdAAAAAA","signature":"MEQCIQCAgYKDhIWGh4iJiouMjY6PkJGSk5SVlpeYmZqbnJ2enwIfEBESExQVFhcYGRobHB0eHyAhIiMkJSYnKCkqKywtLg"}"""


keyFound : String
keyFound =
    """{"rows":[{"public_key":"pQECAyYgASFYIAECAwQFBgcICQoLDA0ODxAREhMUFRYXGBkaGxwdHh8gIlgg__79_Pv6-fj39vX08_Lx8O_u7ezr6uno5-bl5OPi4eA"}],"rowCount":1}"""


signatureChecked : Bool -> App -> App
signatureChecked valid =
    ProgramTest.simulateIncomingPort "interopToElm"
        (Encode.object
            [ ( "tag", Encode.string "SignatureChecked" )
            , ( "resolve", Encode.string "first" )
            , ( "signIn"
              , Encode.object
                    [ ( "challenge", Encode.string "Y2hhbGxlbmdl" )
                    , ( "credentialId", Encode.string "oKGio6SlpqeoqaqrrK2urw" )
                    , ( "signCount", Encode.int 0 )
                    , ( "backedUp", Encode.bool True )
                    ]
              )
            , ( "valid", Encode.bool valid )
            ]
        )


notVerified : Response
notVerified =
    { resolve = "first"
    , status = 401
    , headers = jsonHeaders
    , body = """{"error":"This passkey wasn't recognized."}"""
    }


saved : String
saved =
    """{"results":[{"rows":[{"user_id":"7b0c9a52-3d0e-4a53-9a7e-0f6f1f3f5b11"}]},{"rows":[{"credential_id":"oKGio6SlpqeoqaqrrK2urw","name":"Apple Passwords","created_at":"2026-11-10T09:30:00Z"}]},{"rows":[{"email":"ada@example.com","username":"ada"}]}]}"""


added : Response
added =
    { resolve = "first"
    , status = 201
    , headers = jsonHeaders
    , body = """{"id":"oKGio6SlpqeoqaqrrK2urw","name":"Apple Passwords","createdAt":"2026-11-10T09:30:00Z"}"""
    }


type alias Notice =
    { to : List String
    , subject : String
    , tags : List String
    , told : Bool
    }


notice : String -> Decode.Decoder Notice
notice name =
    Decode.map4 Notice
        (Decode.field "to" (Decode.list (Decode.field "email" Decode.string)))
        (Decode.field "subject" Decode.string)
        (Decode.field "tags" (Decode.list Decode.string))
        (Decode.map2
            (\html text ->
                List.all
                    (\content ->
                        String.contains name content
                            && String.contains "the account ada" content
                            && String.contains "https://pixo.pics/settings/account" content
                    )
                    [ html, text ]
            )
            (Decode.field "htmlContent" Decode.string)
            (Decode.field "textContent" Decode.string)
        )


passkeys : String -> List String -> String -> App -> App
passkeys method path body =
    requestWith
        { resolve = "first"
        , method = method
        , path = path
        , headers = [ ( "cookie", "session=c2Vzc2lvbg" ) ]
        , body = body
        }


queries : Decode.Decoder (List ( String, List String ))
queries =
    Decode.field "queries"
        (Decode.list
            (Decode.map2 Tuple.pair
                (Decode.field "query" Decode.string)
                (Decode.field "params" (Decode.list Decode.string))
            )
        )


started : String
started =
    """{"results":[{"rows":[{"user_id":"7b0c9a52-3d0e-4a53-9a7e-0f6f1f3f5b11"}]},{"rows":[],"rowCount":1},{"rows":[{"challenge":"Y2hhbGxlbmdl"}]},{"rows":[{"email":"ada@example.com","username":"ada"}]},{"rows":[{"credential_id":"b2xkZXI"}]}]}"""


attestation : String -> String
attestation clientDataJSON =
    """{"clientDataJSON":\""""
        ++ clientDataJSON
        ++ """","attestationObject":"o2NmbXRkbm9uZWdhdHRTdG10oGhhdXRoRGF0YViUhc4asGtQ0lfqei0FVSxvDEzvgNF-FdmbKJXm3Ic-1uxdAAAAAPv8MAcVTk7MjAtuAgVX170AEKChoqOkpaanqKmqq6ytrq-lAQIDJiABIVggAQIDBAUGBwgJCgsMDQ4PEBESExQVFhcYGRobHB0eHyAiWCD__v38-_r5-Pf29fTz8vHw7-7t7Ovq6ejn5uXk4-Lh4A","transports":["internal","teleport","hybrid"]}"""


clientData : String
clientData =
    "eyJ0eXBlIjoid2ViYXV0aG4uY3JlYXRlIiwiY2hhbGxlbmdlIjoiWTJoaGJHeGxibWRsIiwib3JpZ2luIjoiaHR0cHM6Ly9waXhvLnBpY3MiLCJjcm9zc09yaWdpbiI6ZmFsc2V9"


clientDataElsewhere : String
clientDataElsewhere =
    "eyJ0eXBlIjoid2ViYXV0aG4uY3JlYXRlIiwiY2hhbGxlbmdlIjoiWTJoaGJHeGxibWRsIiwib3JpZ2luIjoiaHR0cHM6Ly9ldmlsLmV4YW1wbGUifQ"


failed : String -> String
failed output =
    """{"status":"fail","releaseId":"0641607","region":"arn1","checks":{"database":{"status":"fail","latencyMs":0,"output":\""""
        ++ output
        ++ """"}}}"""


health : String -> Int -> String -> Response
health resolve status body =
    { resolve = resolve
    , status = status
    , headers = [ ( "content-type", "application/health+json" ), ( "cache-control", "no-store" ) ]
    , body = body
    }


jsonHeaders : List ( String, String )
jsonHeaders =
    [ ( "content-type", "application/json" ), ( "cache-control", "no-store" ) ]


paddleConfig : { environment : String, apiKey : String, webhookSecret : String, clientToken : String, proPriceId : String }
paddleConfig =
    { environment = "sandbox", apiKey = "pdl_sdbx_apikey_test", webhookSecret = "pdl_ntfset_test", clientToken = "test_token", proPriceId = "pri_pro" }


startPaid : App
startPaid =
    startWith { databaseUrl = Just databaseUrl, brevoApiKey = Just "xkeysib-secret", emailFrom = Just "hello@pixo.pics", origin = "https://pixo.pics", local = False, paddle = Just paddleConfig, storage = Nothing, moderationEmail = Just "mod@pixo.pics" }


freshSignature : String
freshSignature =
    "ts=0;h1=" ++ String.repeat 64 "a"


subscriptionUpdated : String
subscriptionUpdated =
    eventBody "subscription.updated" "pri_pro"


eventBody : String -> String -> String
eventBody eventType priceId =
    """{"event_id":"evt_01hv8x2a0e9xjn3k0q0p1y3z4a","event_type":\""""
        ++ eventType
        ++ """","occurred_at":"2026-11-17T10:00:01.000000Z","data":{"id":"sub_01hv8x2a0e9xjn3k0q0p1y3z4b","status":"active","customer_id":"ctm_01hv8x2a0e9xjn3k0q0p1y3z4c","custom_data":{"user_id":"6f1c1f0e-7f3a-4c2b-9a51-0d4b1d7c8e21"},"items":[{"price":{"id":\""""
        ++ priceId
        ++ """"}}],"current_billing_period":{"starts_at":"2026-11-17T10:00:00Z","ends_at":"2026-12-17T10:00:00Z"},"scheduled_change":null,"updated_at":"2026-11-17T10:00:00.500000Z"}}"""


webhook : String -> String -> App -> App
webhook resolve header =
    requestWith
        { resolve = resolve
        , method = "POST"
        , path = [ "api", "billing", "webhook" ]
        , headers = [ ( "paddle-signature", header ), ( "x-real-ip", "203.0.113.9" ) ]
        , body = subscriptionUpdated
        }


webhookChecked : String -> Bool -> App -> App
webhookChecked body valid =
    ProgramTest.simulateIncomingPort "interopToElm"
        (Encode.object
            [ ( "tag", Encode.string "WebhookChecked" )
            , ( "resolve", Encode.string "first" )
            , ( "body", Encode.string body )
            , ( "valid", Encode.bool valid )
            ]
        )


start : Maybe String -> App
start url =
    startWith { databaseUrl = url, brevoApiKey = Just "xkeysib-secret", emailFrom = Just "hello@pixo.pics", origin = "https://pixo.pics", local = False, paddle = Nothing, storage = Nothing, moderationEmail = Nothing }


startWith :
    { databaseUrl : Maybe String
    , brevoApiKey : Maybe String
    , emailFrom : Maybe String
    , origin : String
    , local : Bool
    , paddle : Maybe { environment : String, apiKey : String, webhookSecret : String, clientToken : String, proPriceId : String }
    , storage : Maybe { accountId : String, accessKeyId : String, secretAccessKey : String, bucket : String, uploadBucket : String, imagesOrigin : String }
    , moderationEmail : Maybe String
    }
    -> App
startWith config =
    ProgramTest.createWorker
        { init = Main.init
        , update = Main.update
        }
        |> ProgramTest.withSimulatedEffects simulateEffects
        |> ProgramTest.withSimulatedSubscriptions simulateSubscriptions
        |> ProgramTest.start
            (Encode.object
                [ ( "databaseUrl", Maybe.withDefault Encode.null (Maybe.map Encode.string config.databaseUrl) )
                , ( "neonEndpoint", Encode.null )
                , ( "brevoApiKey", Maybe.withDefault Encode.null (Maybe.map Encode.string config.brevoApiKey) )
                , ( "emailFrom", Maybe.withDefault Encode.null (Maybe.map Encode.string config.emailFrom) )
                , ( "origin", Encode.string config.origin )
                , ( "releaseId", Encode.string "0641607" )
                , ( "releaseUrl", Encode.null )
                , ( "region", Encode.string "arn1" )
                , ( "local", Encode.bool config.local )
                , ( "paddleEnvironment", paddleFlag .environment config.paddle )
                , ( "paddleApiKey", paddleFlag .apiKey config.paddle )
                , ( "paddleWebhookSecret", paddleFlag .webhookSecret config.paddle )
                , ( "paddleClientToken", paddleFlag .clientToken config.paddle )
                , ( "paddleProPriceId", paddleFlag .proPriceId config.paddle )
                , ( "r2AccountId", paddleFlag .accountId config.storage )
                , ( "r2AccessKeyId", paddleFlag .accessKeyId config.storage )
                , ( "r2SecretAccessKey", paddleFlag .secretAccessKey config.storage )
                , ( "r2Bucket", paddleFlag .bucket config.storage )
                , ( "r2UploadBucket", paddleFlag .uploadBucket config.storage )
                , ( "imagesOrigin", paddleFlag .imagesOrigin config.storage )
                , ( "moderationEmail", Maybe.withDefault Encode.null (Maybe.map Encode.string config.moderationEmail) )
                ]
            )


startPublishing : App
startPublishing =
    startWith
        { databaseUrl = Just databaseUrl
        , brevoApiKey = Just "xkeysib-secret"
        , emailFrom = Just "hello@pixo.pics"
        , origin = "https://pixo.pics"
        , local = False
        , paddle = Just paddleConfig
        , storage = Just { accountId = "acc", accessKeyId = "key", secretAccessKey = "secret", bucket = "pictures", uploadBucket = "uploads", imagesOrigin = "https://img.pixo.pics" }
        , moderationEmail = Just "mod@pixo.pics"
        }


paddleFlag : (a -> String) -> Maybe a -> Encode.Value
paddleFlag pick paddle =
    Maybe.withDefault Encode.null (Maybe.map (pick >> Encode.string) paddle)


repeat : Int -> (String -> App -> App) -> App -> App
repeat times send app =
    List.foldl (\n -> send (String.fromInt n)) app (List.range 1 times)


nowhereFrom : String -> String -> App -> App
nowhereFrom address resolve =
    requestWith { resolve = resolve, method = "GET", path = [ "api", "nowhere" ], headers = [ ( "x-real-ip", address ) ], body = "" }


request : String -> String -> List String -> App -> App
request resolve method path =
    requestWith { resolve = resolve, method = method, path = path, headers = [], body = "" }


signUp : String -> App -> App
signUp =
    signUpAs "first"


signUpAs : String -> String -> App -> App
signUpAs resolve body =
    requestWith
        { resolve = resolve
        , method = "POST"
        , path = [ "api", "auth", "signup" ]
        , headers = [ ( "content-type", "application/json" ), ( "x-real-ip", "203.0.113.7" ) ]
        , body = body
        }


healthFrom : String -> String -> App -> App
healthFrom address resolve =
    requestWith { resolve = resolve, method = "GET", path = [ "api", "health" ], headers = [ ( "x-real-ip", address ) ], body = "" }


requestWith :
    { resolve : String
    , method : String
    , path : List String
    , headers : List ( String, String )
    , body : String
    }
    -> App
    -> App
requestWith { resolve, method, path, headers, body } =
    ProgramTest.simulateIncomingPort "interopToElm"
        (Encode.object
            [ ( "tag", Encode.string "Request" )
            , ( "resolve", Encode.string resolve )
            , ( "request"
              , Encode.object
                    [ ( "method", Encode.string method )
                    , ( "path", Encode.list Encode.string path )
                    , ( "query", Encode.object [] )
                    , ( "headers", Encode.object (List.map (Tuple.mapSecond Encode.string) headers) )
                    , ( "body", Encode.string body )
                    ]
              )
            ]
        )


expectResponses : List Response -> App -> Expectation
expectResponses =
    expectSent responses


expectSent : (List Sent -> a) -> a -> App -> Expectation
expectSent pick expected =
    ProgramTest.expectOutgoingPortValues "interopFromElm" sent (pick >> Expect.equal expected)


responses : List Sent -> List Response
responses =
    List.filterMap
        (\value ->
            case value of
                Responded answer ->
                    Just answer

                _ ->
                    Nothing
        )


checks : List Sent -> List Check
checks =
    List.filterMap
        (\value ->
            case value of
                Asked check ->
                    Just check

                _ ->
                    Nothing
        )


webhookChecks : List Sent -> List WebhookCheck
webhookChecks =
    List.filterMap
        (\value ->
            case value of
                AskedWebhook check ->
                    Just check

                _ ->
                    Nothing
        )


logs : List Sent -> List ( String, String )
logs =
    List.filterMap
        (\value ->
            case value of
                Logged entry ->
                    Just entry

                _ ->
                    Nothing
        )


levels : List Sent -> List String
levels =
    logs >> List.map Tuple.first


sent : Decode.Decoder Sent
sent =
    Decode.field "tag" Decode.string
        |> Decode.andThen
            (\tag ->
                case tag of
                    "Respond" ->
                        Decode.map Responded
                            (Decode.map4 Response
                                (Decode.field "resolve" Decode.string)
                                (Decode.field "status" Decode.int)
                                (Decode.field "headers"
                                    (Decode.list
                                        (Decode.map2 Tuple.pair
                                            (Decode.index 0 Decode.string)
                                            (Decode.index 1 Decode.string)
                                        )
                                    )
                                )
                                (Decode.field "body" Decode.string)
                            )

                    "Log" ->
                        Decode.map Logged
                            (Decode.map2 Tuple.pair
                                (Decode.field "level" Decode.string)
                                (Decode.field "message" Decode.string)
                            )

                    "VerifySignature" ->
                        Decode.map Asked
                            (Decode.map5 Check
                                (Decode.field "resolve" Decode.string)
                                (Decode.field "key"
                                    (Decode.map3 (\kind x y -> ( kind, x, y ))
                                        (Decode.field "tag" Decode.string)
                                        (Decode.field "x" Decode.string)
                                        (Decode.field "y" Decode.string)
                                    )
                                )
                                (Decode.field "signed" Decode.string)
                                (Decode.field "signature" Decode.string)
                                (Decode.field "signIn"
                                    (Decode.map4 (\challenge credentialId signCount backedUp -> ( challenge, credentialId, ( signCount, backedUp ) ))
                                        (Decode.field "challenge" Decode.string)
                                        (Decode.field "credentialId" Decode.string)
                                        (Decode.field "signCount" Decode.int)
                                        (Decode.field "backedUp" Decode.bool)
                                    )
                                )
                            )

                    "VerifyWebhook" ->
                        Decode.map AskedWebhook
                            (Decode.map4 WebhookCheck
                                (Decode.field "resolve" Decode.string)
                                (Decode.field "secret" Decode.string)
                                (Decode.field "signed" Decode.string)
                                (Decode.field "signatures" (Decode.list Decode.string))
                            )

                    _ ->
                        Decode.fail ("Unknown tag: " ++ tag)
            )


simulateEffects : Effect.Effect Main.Msg -> ProgramTest.SimulatedEffect Main.Msg
simulateEffects effect =
    case effect of
        Effect.NoEffect ->
            SimulatedEffect.Cmd.none

        Effect.Batch effects ->
            SimulatedEffect.Cmd.batch (List.map simulateEffects effects)

        Effect.SendToJs value ->
            SimulatedEffect.Ports.send "interopFromElm"
                (TsEncode.encoder Interop.interop.fromElm value)

        Effect.Database { method, headers, url, body, timeout, onResponse } ->
            SimulatedEffect.Task.map3
                (\begin toResult end ->
                    case toResult (Time.posixToMillis end - Time.posixToMillis begin) of
                        Ok msg ->
                            msg

                        Err queryFailure ->
                            Main.DatabaseFailed queryFailure
                )
                SimulatedEffect.Time.now
                (SimulatedEffect.Http.task
                    { method = method
                    , headers = List.map (\( name, value ) -> SimulatedEffect.Http.header name value) headers
                    , url = url
                    , body = SimulatedEffect.Http.jsonBody body
                    , resolver = SimulatedEffect.Http.stringResolver (onResponse >> Ok)
                    , timeout = timeout
                    }
                )
                SimulatedEffect.Time.now
                |> SimulatedEffect.Task.perform identity

        Effect.Http { method, headers, url, body, timeout, onResponse } ->
            SimulatedEffect.Http.task
                { method = method
                , headers = List.map (\( name, value ) -> SimulatedEffect.Http.header name value) headers
                , url = url
                , body = simulatedBody body
                , resolver = SimulatedEffect.Http.stringResolver (onResponse >> Ok)
                , timeout = timeout
                }
                |> SimulatedEffect.Task.perform identity

        Effect.Succeed msg ->
            SimulatedEffect.Task.succeed msg
                |> SimulatedEffect.Task.perform identity

        Effect.Clock gotTime ->
            SimulatedEffect.Task.perform gotTime SimulatedEffect.Time.now


simulateSubscriptions : Main.Model -> ProgramTest.SimulatedSub Main.Msg
simulateSubscriptions _ =
    SimulatedEffect.Ports.subscribe "interopToElm"
        Decode.value
        (Decode.decodeValue (TsDecode.decoder Interop.interop.toElm) >> Main.GotInterop)


subscriptionsQuery : String
subscriptionsQuery =
    "SELECT status, scheduled_change, to_char(scheduled_change_at AT TIME ZONE 'UTC', 'YYYY-MM-DD\"T\"HH24:MI:SS\"Z\"') AS scheduled_change_at, to_char(current_period_ends_at AT TIME ZONE 'UTC', 'YYYY-MM-DD\"T\"HH24:MI:SS\"Z\"') AS current_period_ends_at, paddle_customer_id, paddle_subscription_id FROM subscriptions ORDER BY (status IN ('active', 'trialing', 'past_due')) DESC, paddle_updated_at DESC"


deletionQuery : String
deletionQuery =
    "SELECT CASE WHEN EXISTS (SELECT 1 FROM subscriptions WHERE status IN ('active', 'trialing', 'past_due') AND scheduled_change IS DISTINCT FROM 'cancel') THEN 'renewing' WHEN delete_account($1) THEN 'deleted' ELSE 'signed-out' END AS outcome"


deletionAnswer : String -> String
deletionAnswer outcome =
    """{"results":[{"rows":[{"user_id":"7b0c9a52-3d0e-4a53-9a7e-0f6f1f3f5b11"}]},{"rows":[{"outcome":\"""" ++ outcome ++ """"}]}]}"""


sessionAnswer : String -> String
sessionAnswer rows =
    """{"results":[{"rows":[{"user_id":"7b0c9a52-3d0e-4a53-9a7e-0f6f1f3f5b11"}]},{"rows":[{"email":"ada@example.com","username":"ada"}]},{"rows":[""" ++ rows ++ """]}]}"""


subscriptionRow : String -> String -> String -> String -> String
subscriptionRow status change changeAt periodEnds =
    """{"status":\"""" ++ status ++ """","scheduled_change":""" ++ change ++ ""","scheduled_change_at":""" ++ changeAt ++ ""","current_period_ends_at":""" ++ periodEnds ++ ""","paddle_customer_id":"ctm_01hv8x2a0e9xjn3k0q0p1y3z4c","paddle_subscription_id":"sub_01hv8x2a0e9xjn3k0q0p1y3z4b"}"""


checkoutStateAnswer : String -> String -> String
checkoutStateAnswer pro customerId =
    """{"results":[{"rows":[{"user_id":"6f1c1f0e-7f3a-4c2b-9a51-0d4b1d7c8e21"}]},{"rows":[{"pro":""" ++ pro ++ ""","customer_id":""" ++ customerId ++ """}]}]}"""


checkoutRequest : String -> App -> App
checkoutRequest resolve =
    requestWith
        { resolve = resolve
        , method = "POST"
        , path = [ "api", "billing", "checkout" ]
        , headers = [ ( "cookie", "session=c2Vzc2lvbg" ), ( "x-real-ip", "203.0.113.9" ) ]
        , body = ""
        }


portalRequest : App -> App
portalRequest =
    requestWith
        { resolve = "first"
        , method = "POST"
        , path = [ "api", "billing", "portal" ]
        , headers = [ ( "cookie", "session=c2Vzc2lvbg" ), ( "x-real-ip", "203.0.113.9" ) ]
        , body = ""
        }


deleteAccount : List ( String, String ) -> App -> App
deleteAccount headers =
    requestWith { resolve = "first", method = "DELETE", path = [ "api", "auth", "account" ], headers = headers, body = "" }


sharedHeaders : Int -> List ( String, String )
sharedHeaders seconds =
    [ ( "content-type", "application/json" )
    , ( "cache-control", "public, max-age=0, s-maxage=" ++ String.fromInt seconds ++ ", stale-while-revalidate=" ++ String.fromInt seconds )
    ]


expectPlainQuery : ( String, List (Maybe String) ) -> Test.Http.HttpRequest x a -> Expectation
expectPlainQuery expected =
    Test.Http.expectJsonBody
        (Decode.map2 Tuple.pair
            (Decode.field "query" Decode.string)
            (Decode.field "params" (Decode.list (Decode.nullable Decode.string)))
        )
        (Expect.equal expected)


feedRow : String -> String
feedRow id =
    """{"id":"%1","username":"ada","prompt":"a red fox","width":1024,"height":1024,"steps":4,"seed":"7","model":"prism-ml/bonsai-image-ternary-4B-mlx-2bit","tags":["fox"],"published_at":"2026-11-17T10:00:00Z"}"""
        |> String.replace "%1" id


feedRows : List String -> String
feedRows rows =
    """{"rows":[""" ++ String.join "," rows ++ """],"rowCount":""" ++ String.fromInt (List.length rows) ++ "}"


profilePicture : String -> String
profilePicture id =
    """{"id":"%1","prompt":"a red fox","width":1024,"height":1024,"steps":4,"seed":7,"model":"prism-ml/bonsai-image-ternary-4B-mlx-2bit","tags":["fox"],"published_at":"2026-11-17T10:00:00Z","sort_at":"2026-11-17T11:00:00+01:00"}"""
        |> String.replace "%1" id


profileRows : List String -> String
profileRows pictures =
    """{"rows":[{"username":"ada","pictures":[""" ++ String.join "," pictures ++ """]}],"rowCount":1}"""


pictureId : String
pictureId =
    "6f1c2a52-3d0e-4a53-9a7e-0f6f1f3f5b11"


otherPictureId : String
otherPictureId =
    "0a1b2c3d-4e5f-4a6b-8c7d-9e0f1a2b3c4d"


stagedFile : String -> String
stagedFile id =
    "https://acc.r2.cloudflarestorage.com/uploads/incoming/" ++ id ++ ".jpg"


publicFile : String -> String
publicFile id =
    "https://acc.r2.cloudflarestorage.com/pictures/" ++ id ++ ".jpg"


publishBody : String
publishBody =
    """{"prompt":"a red fox","width":1024,"height":1024,"steps":4,"seed":7,"model":"prism-ml/bonsai-image-ternary-4B-mlx-2bit","bytes":12,"tags":["fox"]}"""


pictureHeaders : List ( String, String )
pictureHeaders =
    [ ( "cookie", "session=c2Vzc2lvbg" ), ( "x-real-ip", "203.0.113.9" ) ]


publishRequest : String -> String -> App -> App
publishRequest resolve body =
    requestWith { resolve = resolve, method = "POST", path = [ "api", "pictures" ], headers = pictureHeaders, body = body }


confirmRequest : String -> App -> App
confirmRequest resolve =
    requestWith { resolve = resolve, method = "POST", path = [ "api", "pictures", pictureId, "published" ], headers = pictureHeaders, body = "" }


reportRequest : String -> String -> App -> App
reportRequest resolve body =
    requestWith { resolve = resolve, method = "POST", path = [ "api", "pictures", pictureId, "reports" ], headers = [ ( "x-real-ip", "203.0.113.9" ) ], body = body }


reportedRows : String -> String -> String -> String
reportedRows outcome first username =
    """{"rows":[{"outcome":\"""" ++ outcome ++ """","first":""" ++ first ++ ""","username":""" ++ username ++ """}],"rowCount":1}"""


unpublishRequest : App -> App
unpublishRequest =
    requestWith { resolve = "first", method = "DELETE", path = [ "api", "pictures", pictureId ], headers = pictureHeaders, body = "" }


signedInRows : String -> String
signedInRows rows =
    """{"results":[{"rows":[{"user_id":"7b0c9a52-3d0e-4a53-9a7e-0f6f1f3f5b11"}]},{"rows":[""" ++ rows ++ """]}]}"""


askedAnswer : String -> String -> String
askedAnswer pro id =
    signedInRows ("""{"pro":""" ++ pro ++ ""","id":""" ++ id ++ "}")


unconfirmedAnswer : String -> String -> String
unconfirmedAnswer bytes publishedAt =
    signedInRows ("""{"bytes":""" ++ bytes ++ ""","published_at":""" ++ publishedAt ++ "}")


markedAnswer : String -> String
markedAnswer publishedAt =
    signedInRows ("""{"published_at":""" ++ publishedAt ++ "}")


droppedAnswer : String -> String
droppedAnswer id =
    signedInRows ("""{"id":\"""" ++ id ++ """"}""")


foundAnswer : String -> String
foundAnswer found =
    signedInRows ("""{"found":""" ++ found ++ "}")


stagedAnswer : String -> String -> Http.Response String
stagedAnswer bytes contentType =
    Test.Http.httpResponse
        { statusCode = 200
        , headers = [ ( "content-length", bytes ), ( "content-type", contentType ), ( "etag", "\"abc\"" ) ]
        , body = ""
        }


copiedAnswer : String -> Http.Response String
copiedAnswer etag =
    Test.Http.httpResponse
        { statusCode = 200
        , headers = []
        , body = "<CopyObjectResult><ETag>" ++ etag ++ "</ETag></CopyObjectResult>"
        }


publishedResponse : Response
publishedResponse =
    { resolve = "first"
    , status = 200
    , headers = jsonHeaders
    , body = """{"id":"6f1c2a52-3d0e-4a53-9a7e-0f6f1f3f5b11","url":"https://img.pixo.pics/6f1c2a52-3d0e-4a53-9a7e-0f6f1f3f5b11.jpg","publishedAt":"2026-11-17T10:00:00Z"}"""
    }


authenticateQuery : ( String, List String )
authenticateQuery =
    ( "SELECT authenticate($1) AS user_id", [ "c2Vzc2lvbg" ] )


expectQueries : List ( String, List String ) -> Test.Http.HttpRequest x a -> Expectation
expectQueries expected =
    Test.Http.expectJsonBody
        (Decode.field "queries"
            (Decode.list
                (Decode.map2 Tuple.pair
                    (Decode.field "query" Decode.string)
                    (Decode.field "params" (Decode.list Decode.string))
                )
            )
        )
        (Expect.equal expected)


markQuery : String
markQuery =
    "UPDATE published_pictures SET published_at = now() WHERE id = $1 AND published_at IS NULL RETURNING to_char(published_at AT TIME ZONE 'UTC', 'YYYY-MM-DD\"T\"HH24:MI:SS\"Z\"') AS published_at"


dropQuery : String
dropQuery =
    "DELETE FROM published_pictures WHERE id = $1 RETURNING id"


beforeDeletionQuery : String
beforeDeletionQuery =
    "SELECT EXISTS (SELECT 1 FROM subscriptions WHERE status IN ('active', 'trialing', 'past_due') AND scheduled_change IS DISTINCT FROM 'cancel') AS renewing, COALESCE((SELECT json_agg(id) FROM published_pictures), '[]'::json) AS pictures"


beforeDeletionAnswer : Bool -> List String -> String
beforeDeletionAnswer renewing ids =
    signedInRows
        ("""{"renewing":"""
            ++ (if renewing then
                    "true"

                else
                    "false"
               )
            ++ ""","pictures":"""
            ++ Encode.encode 0 (Encode.list Encode.string ids)
            ++ "}"
        )


simulatedBody : Effect.Body -> SimulatedEffect.Http.Body
simulatedBody body =
    case body of
        Effect.JsonBody value ->
            SimulatedEffect.Http.jsonBody value

        Effect.FormBody pairs ->
            SimulatedEffect.Http.stringBody "application/x-www-form-urlencoded" (Effect.encodeForm pairs)

        Effect.EmptyBody ->
            SimulatedEffect.Http.emptyBody
