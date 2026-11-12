module PathTest exposing (suite)

import Data.LinkToken as LinkToken
import Expect
import Path exposing (Path)
import Test exposing (Test, describe, test)
import Url


suite : Test
suite =
    describe "Path"
        [ describe "parse"
            (List.map
                (\path ->
                    test ("finds " ++ Path.toString path) <|
                        \() ->
                            Url.fromString ("https://pixo.pics" ++ Path.toString path)
                                |> Maybe.map Path.parse
                                |> Expect.equal (Just path)
                )
                paths
                ++ [ test "takes an empty fragment for no token" <|
                        \() ->
                            Url.fromString "https://pixo.pics/verify#"
                                |> Maybe.map Path.parse
                                |> Expect.equal (Just (Path.Verify Nothing))
                   , test "takes a trailing slash for the same page" <|
                        \() ->
                            Url.fromString "https://pixo.pics/library/"
                                |> Maybe.map Path.parse
                                |> Expect.equal (Just Path.Library)
                   , test "opens the models at the settings" <|
                        \() ->
                            Url.fromString "https://pixo.pics/settings"
                                |> Maybe.map Path.parse
                                |> Expect.equal (Just Path.Models)
                   , test "leaves a query string out of it" <|
                        \() ->
                            Url.fromString "https://pixo.pics/login?from=email"
                                |> Maybe.map Path.parse
                                |> Expect.equal (Just Path.SignIn)
                   , test "takes an unknown address for not found" <|
                        \() ->
                            Url.fromString "https://pixo.pics/nowhere/at/all"
                                |> Maybe.map Path.parse
                                |> Expect.equal (Just Path.NotFound)
                   ]
            )
        ]


paths : List Path
paths =
    collect (Just Path.Explore) []


collect : Maybe Path -> List Path -> List Path
collect path found =
    case path of
        Just current ->
            collect (next current) (current :: found)

        Nothing ->
            List.reverse found


next : Path -> Maybe Path
next path =
    case path of
        Path.Explore ->
            Just Path.Studio

        Path.Studio ->
            Just Path.Library

        Path.Library ->
            Just Path.Models

        Path.Models ->
            Just Path.Server

        Path.Server ->
            Just Path.Account

        Path.Account ->
            Just Path.SignUp

        Path.SignUp ->
            Just Path.SignIn

        Path.SignIn ->
            Just (Path.Verify Nothing)

        Path.Verify Nothing ->
            LinkToken.fromString "Zm9vYmFy"
                |> Maybe.map (\token -> Path.Verify (Just token))

        Path.Verify (Just _) ->
            Just Path.NotFound

        Path.NotFound ->
            Nothing
