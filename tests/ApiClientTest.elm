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
                    answer (response 503 """{"error":"Not available right now. Try again later."}""")
                        |> Expect.equal (Err (ApiClient.Failed "Not available right now. Try again later."))
            , test "fails when the server can't be reached" <|
                \() ->
                    answer Test.Http.networkError
                        |> Expect.equal (Err (ApiClient.Failed "Could not reach the server. Start both apps with `bun dev`."))
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
response statusCode body =
    Test.Http.httpResponse { statusCode = statusCode, headers = [], body = body }
