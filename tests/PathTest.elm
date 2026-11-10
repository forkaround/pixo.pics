module PathTest exposing (suite)

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
                                |> Maybe.andThen Path.parse
                                |> Expect.equal (Just path)
                )
                paths
                ++ [ test "takes an empty fragment for no token" <|
                        \() ->
                            Url.fromString "https://pixo.pics/auth/verify#"
                                |> Maybe.andThen Path.parse
                                |> Expect.equal (Just (Path.Verify Nothing))
                   , test "finds nothing at an unknown address" <|
                        \() ->
                            Url.fromString "https://pixo.pics/nowhere"
                                |> Maybe.andThen Path.parse
                                |> Expect.equal Nothing
                   ]
            )
        ]


paths : List Path
paths =
    collect (Just Path.Home) []


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
        Path.Home ->
            Just Path.SignUp

        Path.SignUp ->
            Just Path.SignIn

        Path.SignIn ->
            Just (Path.Verify Nothing)

        Path.Verify Nothing ->
            Just (Path.Verify (Just "Zm9vYmFy"))

        Path.Verify (Just _) ->
            Just Path.Dashboard

        Path.Dashboard ->
            Nothing
