module ApiTest exposing (suite)

import Api
import Expect
import Json.Decode as Decode
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "Api"
        [ describe "parse"
            (List.map
                (\route ->
                    test ("finds " ++ Api.methodToString (Api.method route) ++ " " ++ Api.toString route) <|
                        \() ->
                            Api.parse (Api.methodToString (Api.method route)) (Api.path route)
                                |> Expect.equal (Ok route)
                )
                routes
                ++ [ test "names the methods a path does allow" <|
                        \() ->
                            Api.parse "POST" [ "api", "health" ]
                                |> Expect.equal (Err (Api.MethodNotAllowed [ Api.Get ]))
                   , test "finds nothing at an unknown path" <|
                        \() ->
                            Api.parse "GET" [ "api", "nowhere" ]
                                |> Expect.equal (Err Api.NotFound)
                   ]
            )
        , describe "health"
            [ test "decodes what it encodes" <|
                \() ->
                    Api.encodeResponse Api.health report
                        |> Decode.decodeValue (Api.responseDecoder Api.health)
                        |> Expect.equal (Ok report)
            ]
        ]


routes : List Api.Route
routes =
    [ Api.GetHealth ]


report : Api.Health
report =
    Api.healthOf
        { releaseId = Just "064160704a6b52143928d03db8e50fcd5dfe0205"
        , releaseUrl = Just "https://github.com/forkaround/pixo.pics/commit/064160704a6b52143928d03db8e50fcd5dfe0205"
        , region = Just "arn1"
        }
        { status = Api.Warn
        , latencyMs = Just 1200
        , output = Just "Slow"
        }
