module OriginTest exposing (suite)

import Data.Origin as Origin
import Expect
import Path
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
        , test "drops a trailing slash" <|
            \() ->
                Origin.parse "https://pixo.pics/"
                    |> Result.map Origin.toString
                    |> Expect.equal (Ok "https://pixo.pics")
        , test "lower-cases the host" <|
            \() ->
                Origin.parse "https://Pixo.PICS"
                    |> Result.map (\origin -> ( Origin.toString origin, Origin.rpId origin ))
                    |> Expect.equal (Ok ( "https://pixo.pics", "pixo.pics" ))
        , test "drops the port its scheme implies" <|
            \() ->
                Origin.parse "https://pixo.pics:443"
                    |> Result.map Origin.toString
                    |> Expect.equal (Ok "https://pixo.pics")
        , test "refuses a path" <|
            \() ->
                Origin.parse "https://pixo.pics/app"
                    |> Result.map Origin.toString
                    |> Expect.equal (Err "Expected an origin without a path, like https://pixo.pics, got: https://pixo.pics/app")
        , test "refuses a query" <|
            \() ->
                Origin.parse "https://pixo.pics?next=1"
                    |> Result.map Origin.toString
                    |> Expect.equal (Err "Expected an origin without a path, like https://pixo.pics, got: https://pixo.pics?next=1")
        , test "writes a path after the origin" <|
            \() ->
                Origin.parse "https://pixo.pics"
                    |> Result.map (\origin -> Origin.href origin Path.Dashboard)
                    |> Expect.equal (Ok "https://pixo.pics/dashboard")
        ]
