module OriginTest exposing (suite)

import Expect
import Origin
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "Origin"
        [ test "keeps the URL, its host for passkeys, and that it is https" <|
            \() ->
                Origin.parse "https://pixo.pics"
                    |> Result.map (\origin -> ( Origin.toString origin, Origin.rpId origin, Origin.isSecure origin ))
                    |> Expect.equal (Ok ( "https://pixo.pics", "pixo.pics", True ))
        , test "leaves out the port, and is not secure over http" <|
            \() ->
                Origin.parse "http://localhost:5173"
                    |> Result.map (\origin -> ( Origin.toString origin, Origin.rpId origin, Origin.isSecure origin ))
                    |> Expect.equal (Ok ( "http://localhost:5173", "localhost", False ))
        , test "refuses what is not a URL" <|
            \() ->
                Origin.parse "pixo.pics"
                    |> Result.map Origin.toString
                    |> Expect.equal (Err "Expected an origin like https://pixo.pics, got: pixo.pics")
        ]
