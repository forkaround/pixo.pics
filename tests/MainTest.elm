module MainTest exposing (suite)

import Expect
import Http
import InteropDefinitions as Interop
import Json.Decode as Decode
import Json.Encode as Encode
import Main
import ProgramTest exposing (ProgramTest)
import SimulatedEffect.Cmd
import SimulatedEffect.Http
import SimulatedEffect.Ports
import Test exposing (Test, describe, test)
import Test.Html.Selector exposing (class, text)
import Test.Http
import TsJson.Decode as TsDecode
import TsJson.Encode as TsEncode


suite : Test
suite =
    describe "Main"
        [ describe "Server card"
            [ test "asks the server for its health on start" <|
                \() ->
                    start defaultFlags
                        |> ProgramTest.expectHttpRequestWasMade "GET" "/api/health"
            , test "names every check while the first report loads" <|
                \() ->
                    start defaultFlags
                        |> ProgramTest.expectViewHas [ text "Database" ]
            , test "shows the report" <|
                \() ->
                    start defaultFlags
                        |> ProgramTest.simulateHttpOk "GET" "/api/health" passing
                        |> ProgramTest.ensureViewHas [ text "27 ms" ]
                        |> ProgramTest.ensureViewHas [ text "0641607" ]
                        |> ProgramTest.expectViewHas [ text "Region arn1" ]
            , test "shows a failing report, which comes with a 503" <|
                \() ->
                    start defaultFlags
                        |> ProgramTest.simulateHttpResponse "GET" "/api/health" (response 503 failing)
                        |> ProgramTest.ensureViewHas [ class "bg-red-500" ]
                        |> ProgramTest.expectViewHas [ text "Database unreachable" ]
            , test "shows the error the server answers with" <|
                \() ->
                    start defaultFlags
                        |> ProgramTest.simulateHttpResponse "GET" "/api/health" (response 500 """{"error":"Out of order"}""")
                        |> ProgramTest.expectViewHas [ text "Out of order" ]
            , test "says so when the server can't be reached" <|
                \() ->
                    start defaultFlags
                        |> ProgramTest.simulateHttpResponse "GET" "/api/health" Test.Http.networkError
                        |> ProgramTest.expectViewHas [ text "Could not reach the server" ]
            , test "keeps the last report while it refreshes" <|
                \() ->
                    start defaultFlags
                        |> ProgramTest.simulateHttpOk "GET" "/api/health" passing
                        |> ProgramTest.clickButton "Refresh"
                        |> ProgramTest.ensureHttpRequestWasMade "GET" "/api/health"
                        |> ProgramTest.expectViewHas [ text "27 ms" ]
            ]
        , describe "Theme"
            [ test "follows the system when none is stored" <|
                \() ->
                    start { defaultFlags | systemDark = True }
                        |> ProgramTest.expectViewHas [ class "dark" ]
            , test "prefers the stored theme to the system's" <|
                \() ->
                    start { theme = Just "light", systemDark = True }
                        |> ProgramTest.expectViewHasNot [ class "dark" ]
            , test "follows the system when it changes" <|
                \() ->
                    start defaultFlags
                        |> ProgramTest.ensureViewHasNot [ class "dark" ]
                        |> ProgramTest.simulateIncomingPort "interopToElm"
                            (Encode.object
                                [ ( "tag", Encode.string "SystemThemeChanged" )
                                , ( "dark", Encode.bool True )
                                ]
                            )
                        |> ProgramTest.expectViewHas [ class "dark" ]
            , test "applies the selected theme and saves it" <|
                \() ->
                    start defaultFlags
                        |> ProgramTest.clickButton "Dark"
                        |> ProgramTest.ensureViewHas [ class "dark" ]
                        |> ProgramTest.expectOutgoingPortValues "interopFromElm"
                            localStorageWrite
                            (Expect.equal [ { tag = "WriteToLocalStorage", key = "theme", value = "dark" } ])
            ]
        ]


type alias Flags =
    { theme : Maybe String
    , systemDark : Bool
    }


defaultFlags : Flags
defaultFlags =
    { theme = Nothing, systemDark = False }


start : Flags -> ProgramTest Main.Model Main.Msg Main.Effect
start flags =
    ProgramTest.createElement
        { init = Main.init
        , update = Main.update
        , view = Main.view
        }
        |> ProgramTest.withSimulatedEffects simulateEffects
        |> ProgramTest.withSimulatedSubscriptions simulateSubscriptions
        |> ProgramTest.start
            (Encode.object
                [ ( "theme", Maybe.withDefault Encode.null (Maybe.map Encode.string flags.theme) )
                , ( "systemDark", Encode.bool flags.systemDark )
                ]
            )


simulateEffects : Main.Effect -> ProgramTest.SimulatedEffect Main.Msg
simulateEffects effect =
    case effect of
        Main.NoEffect ->
            SimulatedEffect.Cmd.none

        Main.Request { method, url, body, onResponse } ->
            SimulatedEffect.Http.request
                { method = method
                , headers = []
                , url = url
                , body =
                    case body of
                        Just value ->
                            SimulatedEffect.Http.jsonBody value

                        Nothing ->
                            SimulatedEffect.Http.emptyBody
                , expect = SimulatedEffect.Http.expectStringResponse merge (onResponse >> Ok)
                , timeout = Nothing
                , tracker = Nothing
                }

        Main.SendToJs value ->
            SimulatedEffect.Ports.send "interopFromElm"
                (TsEncode.encoder Interop.interop.fromElm value)


simulateSubscriptions : Main.Model -> ProgramTest.SimulatedSub Main.Msg
simulateSubscriptions _ =
    SimulatedEffect.Ports.subscribe "interopToElm"
        Decode.value
        (Decode.decodeValue (TsDecode.decoder Interop.interop.toElm) >> Main.GotInterop)


merge : Result a a -> a
merge result =
    case result of
        Ok value ->
            value

        Err value ->
            value


response : Int -> String -> Http.Response String
response statusCode body =
    Test.Http.httpResponse { statusCode = statusCode, headers = [], body = body }


localStorageWrite : Decode.Decoder { tag : String, key : String, value : String }
localStorageWrite =
    Decode.map3 (\tag key value -> { tag = tag, key = key, value = value })
        (Decode.field "tag" Decode.string)
        (Decode.at [ "data", "key" ] Decode.string)
        (Decode.at [ "data", "value" ] Decode.string)


passing : String
passing =
    """
    {
      "status": "pass",
      "releaseId": "064160704a6b52143928d03db8e50fcd5dfe0205",
      "region": "arn1",
      "links": {
        "release": "https://github.com/forkaround/pixo.pics/commit/064160704a6b52143928d03db8e50fcd5dfe0205"
      },
      "checks": { "database": { "status": "pass", "latencyMs": 27 } }
    }
    """


failing : String
failing =
    """
    {
      "status": "fail",
      "checks": { "database": { "status": "fail", "output": "Database unreachable" } }
    }
    """
