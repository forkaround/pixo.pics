module ApiClientTest exposing (suite)

import Api
import ApiClient
import Data.LinkToken as LinkToken
import Expect
import Http
import Test exposing (Test, describe, test)
import Test.Http


suite : Test
suite =
    describe "ApiClient"
        [ describe "request"
            [ test "tells a 401 from every other failure" <|
                \() ->
                    answer (response 401 """{"error":"Not signed in."}""")
                        |> Expect.equal (Err (ApiClient.Unauthorized "Not signed in."))
            , test "keeps the reason the server gives with another status" <|
                \() ->
                    answer (response 503 """{"error":"Unavailable right now."}""")
                        |> Expect.equal (Err (ApiClient.Failed "Unavailable right now."))
            , test "reads how long to wait from a 429" <|
                \() ->
                    answer (responseWith 429 [ ( "retry-after", "30" ) ] """{"error":"Too many attempts."}""")
                        |> Expect.equal (Err (ApiClient.Throttled 30 "Too many attempts."))
            , test "reads the wait whatever the header's case" <|
                \() ->
                    answer (responseWith 429 [ ( "Retry-After", "30" ) ] """{"error":"Too many attempts."}""")
                        |> Expect.equal (Err (ApiClient.Throttled 30 "Too many attempts."))
            , test "fails a 429 that says no wait" <|
                \() ->
                    answer (response 429 """{"error":"Too many attempts."}""")
                        |> Expect.equal (Err (ApiClient.Failed "Too many attempts."))
            , test "tells a refused input from a failure" <|
                \() ->
                    answer (response 404 """{"error":"This link is not valid any more. Sign up again to get a new one."}""")
                        |> Expect.equal (Err (ApiClient.Refused "This link is not valid any more. Sign up again to get a new one."))
            , test "fails when the server can't be reached" <|
                \() ->
                    answer Test.Http.networkError
                        |> Expect.equal (Err (ApiClient.Failed "Network offline"))
            , test "decodes the endpoint's response from an error status" <|
                \() ->
                    (ApiClient.request Api.health () identity).onResponse
                        (response 503 """{"status":"fail","checks":{"database":{"status":"fail","output":"Database unreachable"}}}""")
                        |> Result.map .status
                        |> Expect.equal (Ok Api.Fail)
            , test "sends a link's token as a bearer token" <|
                \() ->
                    LinkToken.fromString "Zm9vYmFy"
                        |> Maybe.map (\token -> (ApiClient.request Api.verification () identity |> ApiClient.withToken token).headers)
                        |> Expect.equal (Just [ ( "authorization", "Bearer Zm9vYmFy" ) ])
            ]
        ]


answer : Http.Response String -> Result ApiClient.Error Api.Account
answer =
    (ApiClient.request Api.session () identity).onResponse


response : Int -> String -> Http.Response String
response statusCode =
    responseWith statusCode []


responseWith : Int -> List ( String, String ) -> String -> Http.Response String
responseWith statusCode headers body =
    Test.Http.httpResponse { statusCode = statusCode, headers = headers, body = body }
