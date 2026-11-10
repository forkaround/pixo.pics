module MainTest exposing (suite)

import Brevo
import Expect exposing (Expectation)
import InteropDefinitions as Interop
import Json.Decode as Decode
import Json.Encode as Encode
import Main
import ProgramTest exposing (ProgramTest)
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
                              , ( "warn", "Health check slow: the database took 1500 ms." )
                              ]
                            , [ health "first"
                                    200
                                    """{"status":"warn","releaseId":"0641607","region":"arn1","checks":{"database":{"status":"warn","latencyMs":1500,"output":"Slower than 1000 ms."}}}"""
                              ]
                            )
            , test "fails when its role is exempt from row-level security" <|
                \() ->
                    start (Just databaseUrl)
                        |> request "first" "GET" [ "api", "health" ]
                        |> ProgramTest.simulateHttpOk "POST" endpoint """{"rows":[{"exempt":true}],"rowCount":1}"""
                        |> expectSent (\values -> ( levels values, responses values ))
                            ( [ "info", "error" ]
                            , [ health "first" 503 (failed "Unexpected result.") ]
                            )
            , test "fails when Neon can't be reached" <|
                \() ->
                    start (Just databaseUrl)
                        |> request "first" "GET" [ "api", "health" ]
                        |> ProgramTest.simulateHttpResponse "POST" endpoint Test.Http.networkError
                        |> expectResponses [ health "first" 503 (failed "Unreachable.") ]
            , test "fails when Neon takes too long" <|
                \() ->
                    start (Just databaseUrl)
                        |> request "first" "GET" [ "api", "health" ]
                        |> ProgramTest.simulateHttpResponse "POST" endpoint Test.Http.timeout
                        |> expectResponses [ health "first" 503 (failed "Timed out after 5000 ms.") ]
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
                        |> ProgramTest.simulateHttpOk "POST" endpoint allowed
                        |> ProgramTest.simulateHttpOk "POST" endpoint (linked "sign-up")
                        |> ProgramTest.simulateHttpOk "POST" Brevo.endpoint """{"messageId":"<1@smtp-relay.mailin.fr>"}"""
                        |> expectResponses
                            [ { resolve = "first", status = 202, headers = jsonHeaders, body = "{}" } ]
            , test "limits by address, then by the cleaned email" <|
                \() ->
                    start (Just databaseUrl)
                        |> signUp """{"email":" Ada@Example.com ","username":"Ada"}"""
                        |> ProgramTest.expectHttpRequest "POST"
                            endpoint
                            (Test.Http.expectJsonBody
                                (Decode.field "queries" (Decode.list (Decode.field "params" (Decode.list Decode.string))))
                                (Expect.equal
                                    [ [ "magic-link:ip:203.0.113.7", "10", "900" ]
                                    , [ "magic-link:email:ada@example.com", "3", "900" ]
                                    ]
                                )
                            )
            , test "asks for a link that lasts 15 minutes" <|
                \() ->
                    start (Just databaseUrl)
                        |> signUp """{"email":" Ada@Example.com ","username":"Ada"}"""
                        |> ProgramTest.simulateHttpOk "POST" endpoint allowed
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
                        |> ProgramTest.simulateHttpOk "POST" endpoint allowed
                        |> ProgramTest.simulateHttpOk "POST" endpoint (linked "sign-up")
                        |> ProgramTest.expectHttpRequest "POST"
                            Brevo.endpoint
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
                        |> ProgramTest.simulateHttpOk "POST" endpoint allowed
                        |> ProgramTest.simulateHttpOk "POST" endpoint (linked "sign-in")
                        |> ProgramTest.ensureHttpRequest "POST"
                            Brevo.endpoint
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
                        |> ProgramTest.simulateHttpOk "POST" Brevo.endpoint """{"messageId":"<1@smtp-relay.mailin.fr>"}"""
                        |> expectResponses
                            [ { resolve = "first", status = 202, headers = jsonHeaders, body = "{}" } ]
            , test "answers 409 when the username is taken" <|
                \() ->
                    start (Just databaseUrl)
                        |> signUp """{"email":"ada@example.com","username":"ada"}"""
                        |> ProgramTest.simulateHttpOk "POST" endpoint allowed
                        |> ProgramTest.simulateHttpOk "POST" endpoint """{"rows":[{"outcome":"username-taken","token":null}],"rowCount":1}"""
                        |> expectResponses
                            [ { resolve = "first"
                              , status = 409
                              , headers = jsonHeaders
                              , body = """{"error":"That username is taken."}"""
                              }
                            ]
            , test "answers 429 with the longest wait when a limit is reached" <|
                \() ->
                    start (Just databaseUrl)
                        |> signUp """{"email":"ada@example.com","username":"ada"}"""
                        |> ProgramTest.simulateHttpOk "POST"
                            endpoint
                            """{"results":[{"rows":[{"allowed":true,"remaining":9,"retry_after":0}]},{"rows":[{"allowed":false,"remaining":0,"retry_after":240}]}]}"""
                        |> expectResponses
                            [ { resolve = "first"
                              , status = 429
                              , headers = ( "retry-after", "240" ) :: jsonHeaders
                              , body = """{"error":"Too many attempts. Try again later."}"""
                              }
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
                        |> ProgramTest.simulateHttpOk "POST" endpoint allowed
                        |> ProgramTest.simulateHttpOk "POST" endpoint (linked "sign-up")
                        |> ProgramTest.simulateHttpResponse "POST"
                            Brevo.endpoint
                            (Test.Http.httpResponse
                                { statusCode = 401
                                , headers = []
                                , body = """{"code":"unauthorized","message":"Key not found"}"""
                                }
                            )
                        |> expectSent (\values -> ( logs values, responses values ))
                            ( [ ( "info", "Sending queries to " ++ endpoint )
                              , ( "error", "Magic link failed at its email: Brevo refused the email with HTTP 401 (unauthorized): Key not found" )
                              ]
                            , [ { resolve = "first"
                                , status = 502
                                , headers = jsonHeaders
                                , body = """{"error":"The email could not be sent. Try again."}"""
                                }
                              ]
                            )
            , test "answers 503 and logs why when the database fails" <|
                \() ->
                    start (Just databaseUrl)
                        |> signUp """{"email":"ada@example.com","username":"ada"}"""
                        |> ProgramTest.simulateHttpResponse "POST" endpoint Test.Http.networkError
                        |> expectSent (\values -> ( levels values, List.map .status (responses values) ))
                            ( [ "info", "error" ], [ 503 ] )
            , test "answers 503 at once when email isn't configured" <|
                \() ->
                    startWith { databaseUrl = Just databaseUrl, brevoApiKey = Nothing }
                        |> signUp """{"email":"ada@example.com","username":"ada"}"""
                        |> expectResponses
                            [ { resolve = "first"
                              , status = 503
                              , headers = jsonHeaders
                              , body = """{"error":"Not available right now. Try again later."}"""
                              }
                            ]
            , test "answers 503 and logs when the database gives an outcome a sign-up can't have" <|
                \() ->
                    start (Just databaseUrl)
                        |> signUp """{"email":"ada@example.com","username":"ada"}"""
                        |> ProgramTest.simulateHttpOk "POST" endpoint allowed
                        |> ProgramTest.simulateHttpOk "POST" endpoint """{"rows":[{"outcome":"no-account","token":null}],"rowCount":1}"""
                        |> expectSent (\values -> ( levels values, List.map .status (responses values) ))
                            ( [ "info", "error" ], [ 503 ] )
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
                            ( [ "info", "error" ], [ 503 ] )
            ]
        , describe "POST /api/auth/signin"
            [ test "emails a sign-in link to an address that has an account" <|
                \() ->
                    start (Just databaseUrl)
                        |> signIn """{"email":" Ada@Example.com "}"""
                        |> ProgramTest.ensureHttpRequest "POST"
                            endpoint
                            (Test.Http.expectJsonBody
                                (Decode.field "queries" (Decode.list (Decode.field "params" (Decode.list Decode.string))))
                                (Expect.equal
                                    [ [ "magic-link:ip:203.0.113.7", "10", "900" ]
                                    , [ "magic-link:email:ada@example.com", "3", "900" ]
                                    ]
                                )
                            )
                        |> ProgramTest.simulateHttpOk "POST" endpoint allowed
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
                            Brevo.endpoint
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
                        |> ProgramTest.simulateHttpOk "POST" Brevo.endpoint """{"messageId":"<1@smtp-relay.mailin.fr>"}"""
                        |> expectResponses
                            [ { resolve = "first", status = 202, headers = jsonHeaders, body = "{}" } ]
            , test "answers 202 and emails nobody when the address has no account" <|
                \() ->
                    start (Just databaseUrl)
                        |> signIn """{"email":"ada@example.com"}"""
                        |> ProgramTest.simulateHttpOk "POST" endpoint allowed
                        |> ProgramTest.simulateHttpOk "POST" endpoint """{"rows":[{"outcome":"no-account","token":null}],"rowCount":1}"""
                        |> ProgramTest.ensureHttpRequests "POST" Brevo.endpoint (List.length >> Expect.equal 0)
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
                        |> ProgramTest.simulateHttpOk "POST" endpoint allowed
                        |> ProgramTest.simulateHttpOk "POST" endpoint (linked "sign-up")
                        |> expectSent (\values -> ( levels values, List.map .status (responses values) ))
                            ( [ "info", "error" ], [ 503 ] )
            ]
        , describe "GET /api/auth/session"
            [ test "answers with the account of the session in the cookie" <|
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
                                    ]
                                )
                            )
                        |> ProgramTest.simulateHttpOk "POST"
                            endpoint
                            """{"results":[{"rows":[{"user_id":"7b0c9a52-3d0e-4a53-9a7e-0f6f1f3f5b11"}]},{"rows":[{"email":"ada@example.com","username":"ada"}]}]}"""
                        |> expectResponses
                            [ { resolve = "first"
                              , status = 200
                              , headers = jsonHeaders
                              , body = """{"email":"ada@example.com","username":"ada"}"""
                              }
                            ]
            , test "answers 401 when the session is over" <|
                \() ->
                    start (Just databaseUrl)
                        |> session "GET" [ ( "cookie", "session=c2Vzc2lvbg" ) ]
                        |> ProgramTest.simulateHttpOk "POST" endpoint """{"results":[{"rows":[{"user_id":null}]},{"rows":[]}]}"""
                        |> expectResponses
                            [ { resolve = "first", status = 401, headers = jsonHeaders, body = """{"error":"Not signed in."}""" } ]
            , test "answers 401 at once without a cookie" <|
                \() ->
                    start (Just databaseUrl)
                        |> session "GET" []
                        |> expectResponses
                            [ { resolve = "first", status = 401, headers = jsonHeaders, body = """{"error":"Not signed in."}""" } ]
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
                            ( [ "info", "error" ], [ 503 ] )
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
                            Brevo.endpoint
                            (Test.Http.expectJsonBody (notice "Apple Passwords")
                                (Expect.equal
                                    { to = [ "ada@example.com" ]
                                    , subject = "A passkey was added to your pixo account"
                                    , tags = [ "passkey-added" ]
                                    , told = True
                                    }
                                )
                            )
                        |> ProgramTest.simulateHttpOk "POST" Brevo.endpoint """{"messageId":"<1@smtp-relay.mailin.fr>"}"""
                        |> expectResponses [ added ]
            , test "answers 201 all the same, and logs why, when the email fails" <|
                \() ->
                    start (Just databaseUrl)
                        |> passkeys "POST" [ "api", "auth", "passkeys" ] (attestation clientData)
                        |> ProgramTest.simulateHttpOk "POST" endpoint saved
                        |> ProgramTest.simulateHttpResponse "POST" Brevo.endpoint Test.Http.networkError
                        |> expectSent (\values -> ( levels values, responses values ))
                            ( [ "info", "error" ], [ added ] )
            , test "answers 201 without an email when email isn't configured" <|
                \() ->
                    startWith { databaseUrl = Just databaseUrl, brevoApiKey = Nothing }
                        |> passkeys "POST" [ "api", "auth", "passkeys" ] (attestation clientData)
                        |> ProgramTest.simulateHttpOk "POST" endpoint saved
                        |> expectResponses [ added ]
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
                              , body = """{"error":"This passkey request expired or was used already. Try again."}"""
                              }
                            ]
            , test "answers 422, asks the database nothing and logs why when the passkey is from another site" <|
                \() ->
                    start (Just databaseUrl)
                        |> passkeys "POST" [ "api", "auth", "passkeys" ] (attestation clientDataElsewhere)
                        |> ProgramTest.ensureHttpRequests "POST" endpoint (List.length >> Expect.equal 0)
                        |> expectSent (\values -> ( logs values, responses values ))
                            ( [ ( "info", "Sending queries to " ++ endpoint )
                              , ( "warn", "Passkey refused: The client data comes from https://evil.example, not from https://pixo.pics." )
                              ]
                            , [ { resolve = "first"
                                , status = 422
                                , headers = jsonHeaders
                                , body = """{"error":"This passkey could not be verified. Try again."}"""
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
                            Brevo.endpoint
                            (Test.Http.expectJsonBody (notice "Apple Passwords")
                                (Expect.equal
                                    { to = [ "ada@example.com" ]
                                    , subject = "A passkey was removed from your pixo account"
                                    , tags = [ "passkey-removed" ]
                                    , told = True
                                    }
                                )
                            )
                        |> ProgramTest.simulateHttpOk "POST" Brevo.endpoint """{"messageId":"<1@smtp-relay.mailin.fr>"}"""
                        |> expectResponses
                            [ { resolve = "first", status = 200, headers = jsonHeaders, body = "{}" } ]
            , test "answers 404 and emails nobody when the account has no such passkey" <|
                \() ->
                    start (Just databaseUrl)
                        |> passkeys "DELETE" [ "api", "auth", "passkeys", "oKGio6SlpqeoqaqrrK2urw" ] ""
                        |> ProgramTest.simulateHttpOk "POST"
                            endpoint
                            """{"results":[{"rows":[{"user_id":"7b0c9a52-3d0e-4a53-9a7e-0f6f1f3f5b11"}]},{"rows":[]},{"rows":[{"email":"ada@example.com","username":"ada"}]}]}"""
                        |> ProgramTest.ensureHttpRequests "POST" Brevo.endpoint (List.length >> Expect.equal 0)
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
            [ test "answers with a challenge that lasts 5 minutes, limited by address" <|
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
                                (Expect.equal
                                    ( "SELECT l.retry_after, CASE WHEN l.allowed THEN create_authentication_challenge($4) END AS challenge FROM rate_limit($1, $2, $3) AS l"
                                    , [ "passkey:ip:203.0.113.7", "20", "900", "300" ]
                                    )
                                )
                            )
                        |> ProgramTest.simulateHttpOk "POST" endpoint """{"rows":[{"retry_after":0,"challenge":"Y2hhbGxlbmdl"}],"rowCount":1}"""
                        |> expectResponses
                            [ { resolve = "first"
                              , status = 200
                              , headers = jsonHeaders
                              , body = """{"challenge":"Y2hhbGxlbmdl","rpId":"pixo.pics"}"""
                              }
                            ]
            , test "answers 429 when the address asks too often" <|
                \() ->
                    start (Just databaseUrl)
                        |> signInOptions
                        |> ProgramTest.simulateHttpOk "POST" endpoint """{"rows":[{"retry_after":45,"challenge":null}],"rowCount":1}"""
                        |> expectResponses
                            [ { resolve = "first"
                              , status = 429
                              , headers = ( "retry-after", "45" ) :: jsonHeaders
                              , body = """{"error":"Too many attempts. Try again later."}"""
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
                        |> expectSent (\values -> ( logs values |> List.drop 1, responses values ))
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
        , describe "init"
            [ test "warns when email isn't configured" <|
                \() ->
                    startWith { databaseUrl = Just databaseUrl, brevoApiKey = Nothing }
                        |> expectSent logs
                            [ ( "info", "Sending queries to " ++ endpoint )
                            , ( "warn", "No email, so nothing is sent: BREVO_API_KEY and EMAIL_FROM must both be set." )
                            ]
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
    ProgramTest Main.Model Main.Msg Main.Effect


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


allowed : String
allowed =
    """{"results":[{"rows":[{"allowed":true,"remaining":9,"retry_after":0}]},{"rows":[{"allowed":true,"remaining":2,"retry_after":0}]}]}"""


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
                        String.contains "https://pixo.pics/auth/verify#Zm9vYmFy" content
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
    requestWith
        { resolve = "first"
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
    , body = """{"error":"This passkey could not be verified. Try again, or sign in with an emailed link."}"""
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
                            && String.contains "https://pixo.pics/dashboard" content
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


start : Maybe String -> App
start url =
    startWith { databaseUrl = url, brevoApiKey = Just "xkeysib-secret" }


startWith : { databaseUrl : Maybe String, brevoApiKey : Maybe String } -> App
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
                , ( "emailFrom", Encode.string "hello@pixo.pics" )
                , ( "origin", Encode.string "https://pixo.pics" )
                , ( "releaseId", Encode.string "0641607" )
                , ( "releaseUrl", Encode.null )
                , ( "region", Encode.string "arn1" )
                ]
            )


request : String -> String -> List String -> App -> App
request resolve method path =
    requestWith { resolve = resolve, method = method, path = path, headers = [], body = "" }


signUp : String -> App -> App
signUp body =
    requestWith
        { resolve = "first"
        , method = "POST"
        , path = [ "api", "auth", "signup" ]
        , headers = [ ( "content-type", "application/json" ), ( "x-real-ip", "203.0.113.7" ) ]
        , body = body
        }


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

                    _ ->
                        Decode.fail ("Unknown tag: " ++ tag)
            )


simulateEffects : Main.Effect -> ProgramTest.SimulatedEffect Main.Msg
simulateEffects effect =
    case effect of
        Main.NoEffect ->
            SimulatedEffect.Cmd.none

        Main.Batch effects ->
            SimulatedEffect.Cmd.batch (List.map simulateEffects effects)

        Main.SendToJs value ->
            SimulatedEffect.Ports.send "interopFromElm"
                (TsEncode.encoder Interop.interop.fromElm value)

        Main.Database { method, headers, url, body, timeout, onResponse } ->
            SimulatedEffect.Time.now
                |> SimulatedEffect.Task.andThen
                    (\begin ->
                        SimulatedEffect.Http.task
                            { method = method
                            , headers = List.map (\( name, value ) -> SimulatedEffect.Http.header name value) headers
                            , url = url
                            , body = SimulatedEffect.Http.jsonBody body
                            , resolver = SimulatedEffect.Http.stringResolver (onResponse >> Ok)
                            , timeout = timeout
                            }
                            |> SimulatedEffect.Task.andThen
                                (\toMsg ->
                                    SimulatedEffect.Time.now
                                        |> SimulatedEffect.Task.map
                                            (\end -> toMsg (Time.posixToMillis end - Time.posixToMillis begin))
                                )
                    )
                |> SimulatedEffect.Task.perform identity

        Main.Email { method, headers, url, body, timeout, onResponse } ->
            SimulatedEffect.Http.task
                { method = method
                , headers = List.map (\( name, value ) -> SimulatedEffect.Http.header name value) headers
                , url = url
                , body = SimulatedEffect.Http.jsonBody body
                , resolver = SimulatedEffect.Http.stringResolver (onResponse >> Ok)
                , timeout = timeout
                }
                |> SimulatedEffect.Task.perform identity


simulateSubscriptions : Main.Model -> ProgramTest.SimulatedSub Main.Msg
simulateSubscriptions _ =
    SimulatedEffect.Ports.subscribe "interopToElm"
        Decode.value
        (Decode.decodeValue (TsDecode.decoder Interop.interop.toElm) >> Main.GotInterop)
