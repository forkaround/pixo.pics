module RequestTest exposing (suite)

import Data.LinkToken as LinkToken
import Dict
import Expect
import Request exposing (Request)
import Test exposing (Test, describe, test)


requestWith : List ( String, String ) -> Request
requestWith headers =
    { method = "GET"
    , path = []
    , query = Dict.empty
    , headers = Dict.fromList headers
    , body = ""
    }


suite : Test
suite =
    describe "Request"
        [ test "takes the address from x-real-ip, trimmed" <|
            \() ->
                requestWith [ ( "x-real-ip", " 203.0.113.7 " ), ( "x-forwarded-for", "198.51.100.1" ) ]
                    |> Request.clientAddress
                    |> Expect.equal "203.0.113.7"
        , test "falls back to the first forwarded address" <|
            \() ->
                requestWith [ ( "x-forwarded-for", "198.51.100.1, 10.0.0.1" ) ]
                    |> Request.clientAddress
                    |> Expect.equal "198.51.100.1"
        , test "says unknown without either header" <|
            \() ->
                requestWith []
                    |> Request.clientAddress
                    |> Expect.equal "unknown"
        , test "reads a bearer token" <|
            \() ->
                requestWith [ ( "authorization", "Bearer Zm9vYmFy" ) ]
                    |> Request.linkToken
                    |> Maybe.map LinkToken.toString
                    |> Expect.equal (Just "Zm9vYmFy")
        , test "takes the scheme in any case" <|
            \() ->
                requestWith [ ( "authorization", "bearer Zm9vYmFy" ) ]
                    |> Request.linkToken
                    |> Maybe.map LinkToken.toString
                    |> Expect.equal (Just "Zm9vYmFy")
        , test "finds no token in another scheme" <|
            \() ->
                requestWith [ ( "authorization", "Basic Zm9vYmFy" ) ]
                    |> Request.linkToken
                    |> Maybe.map LinkToken.toString
                    |> Expect.equal Nothing
        , test "finds no token in an empty bearer" <|
            \() ->
                requestWith [ ( "authorization", "Bearer " ) ]
                    |> Request.linkToken
                    |> Maybe.map LinkToken.toString
                    |> Expect.equal Nothing
        ]
