module ResponseTest exposing (suite)

import Expect
import Response
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "Response"
        [ test "builds an error as JSON that is not cached" <|
            \() ->
                Response.error 404 "Not found."
                    |> Expect.equal
                        { status = 404
                        , headers = [ ( "content-type", "application/json" ), ( "cache-control", "no-store" ) ]
                        , body = "{\"error\":\"Not found.\"}"
                        }
        , test "puts a header first" <|
            \() ->
                Response.error 405 "Method not allowed."
                    |> Response.withHeader "allow" "GET"
                    |> .headers
                    |> List.head
                    |> Expect.equal (Just ( "allow", "GET" ))
        , test "tells when to retry" <|
            \() ->
                let
                    response =
                        Response.tooManyRequests 12
                in
                ( response.status, List.head response.headers )
                    |> Expect.equal ( 429, Just ( "retry-after", "12" ) )
        ]
