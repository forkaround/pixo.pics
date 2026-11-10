module MainTest exposing (suite)

import Expect exposing (Expectation)
import InteropDefinitions as Interop
import Json.Decode as Decode
import Json.Encode as Encode
import Main
import ProgramTest exposing (ProgramTest)
import SimulatedEffect.Cmd
import SimulatedEffect.Http
import SimulatedEffect.Ports
import SimulatedEffect.Task
import SimulatedEffect.Time
import Test exposing (Test, describe, test)
import Test.Http
import Time
import TsJson.Decode as TsDecode
import TsJson.Encode as TsEncode


suite : Test
suite =
    describe "Main"
        [ describe "GET /api/health"
            [ test "passes when the database answers" <|
                \() ->
                    start (Just databaseUrl)
                        |> request "first" "GET" [ "api", "health" ]
                        |> ProgramTest.simulateHttpOk "POST" endpoint notExempt
                        |> expectResponses
                            [ health "first"
                                200
                                """{"status":"pass","releaseId":"0641607","region":"arn1","checks":{"database":{"status":"pass","latencyMs":0}}}"""
                            ]
            , test "asks Neon with the connection string from the flags" <|
                \() ->
                    start (Just databaseUrl)
                        |> request "first" "GET" [ "api", "health" ]
                        |> ProgramTest.expectHttpRequest "POST"
                            endpoint
                            (Test.Http.hasHeader "Neon-Connection-String" databaseUrl)
            , test "warns when the database is slow" <|
                \() ->
                    start (Just databaseUrl)
                        |> request "first" "GET" [ "api", "health" ]
                        |> ProgramTest.advanceTime 1500
                        |> ProgramTest.simulateHttpOk "POST" endpoint notExempt
                        |> expectSent (\values -> ( logs values, responses values ))
                            ( [ ( "info", "Sending queries to " ++ endpoint )
                              , ( "warn", "Health check slow: the database took 1500 ms." )
                              ]
                            , [ health "first"
                                    200
                                    """{"status":"warn","releaseId":"0641607","region":"arn1","checks":{"database":{"status":"warn","latencyMs":1500,"output":"Slower than 1000 ms."}}}"""
                              ]
                            )
            , test "fails when its role is exempt from row-level security" <|
                \() ->
                    start (Just databaseUrl)
                        |> request "first" "GET" [ "api", "health" ]
                        |> ProgramTest.simulateHttpOk "POST" endpoint """{"rows":[{"exempt":true}],"rowCount":1}"""
                        |> expectSent (\values -> ( levels values, responses values ))
                            ( [ "info", "error" ]
                            , [ health "first" 503 (failed "Unexpected result.") ]
                            )
            , test "fails when Neon can't be reached" <|
                \() ->
                    start (Just databaseUrl)
                        |> request "first" "GET" [ "api", "health" ]
                        |> ProgramTest.simulateHttpResponse "POST" endpoint Test.Http.networkError
                        |> expectResponses [ health "first" 503 (failed "Unreachable.") ]
            , test "fails when Neon takes too long" <|
                \() ->
                    start (Just databaseUrl)
                        |> request "first" "GET" [ "api", "health" ]
                        |> ProgramTest.simulateHttpResponse "POST" endpoint Test.Http.timeout
                        |> expectResponses [ health "first" 503 (failed "Timed out after 5000 ms.") ]
            , test "fails at once when the database isn't configured" <|
                \() ->
                    start Nothing
                        |> request "first" "GET" [ "api", "health" ]
                        |> expectSent (\values -> ( levels values, responses values ))
                            ( [ "warn" ]
                            , [ health "first"
                                    503
                                    """{"status":"fail","releaseId":"0641607","region":"arn1","checks":{"database":{"status":"fail","output":"Not configured: DATABASE_URL (a URL) and API_DATABASE_PASSWORD must both be set."}}}"""
                              ]
                            )
            , test "answers each request through its own resolver" <|
                \() ->
                    start (Just databaseUrl)
                        |> request "first" "GET" [ "api", "health" ]
                        |> request "second" "GET" [ "api", "nowhere" ]
                        |> ProgramTest.simulateHttpOk "POST" endpoint notExempt
                        |> expectSent (responses >> List.map (\{ resolve, status } -> ( resolve, status )))
                            [ ( "second", 404 ), ( "first", 200 ) ]
            ]
        , describe "routing"
            [ test "answers 404 at an unknown path" <|
                \() ->
                    start (Just databaseUrl)
                        |> request "first" "GET" [ "api", "nowhere" ]
                        |> expectResponses
                            [ { resolve = "first"
                              , status = 404
                              , headers = jsonHeaders
                              , body = """{"error":"Not found."}"""
                              }
                            ]
            , test "answers 405 with the methods the path allows" <|
                \() ->
                    start (Just databaseUrl)
                        |> request "first" "POST" [ "api", "health" ]
                        |> expectResponses
                            [ { resolve = "first"
                              , status = 405
                              , headers = ( "allow", "GET" ) :: jsonHeaders
                              , body = """{"error":"Method not allowed."}"""
                              }
                            ]
            ]
        ]


type alias App =
    ProgramTest Main.Model Main.Msg Main.Effect


type alias Response =
    { resolve : String
    , status : Int
    , headers : List ( String, String )
    , body : String
    }


type Sent
    = Responded Response
    | Logged ( String, String )


databaseUrl : String
databaseUrl =
    "postgresql://api:secret@ep-test-123456.eu-central-1.aws.neon.tech/app"


endpoint : String
endpoint =
    "https://api.eu-central-1.aws.neon.tech/sql"


notExempt : String
notExempt =
    """{"rows":[{"exempt":false}],"rowCount":1}"""


failed : String -> String
failed output =
    """{"status":"fail","releaseId":"0641607","region":"arn1","checks":{"database":{"status":"fail","latencyMs":0,"output":\""""
        ++ output
        ++ """"}}}"""


health : String -> Int -> String -> Response
health resolve status body =
    { resolve = resolve
    , status = status
    , headers = [ ( "content-type", "application/health+json" ), ( "cache-control", "no-store" ) ]
    , body = body
    }


jsonHeaders : List ( String, String )
jsonHeaders =
    [ ( "content-type", "application/json" ), ( "cache-control", "no-store" ) ]


start : Maybe String -> App
start url =
    ProgramTest.createWorker
        { init = Main.init
        , update = Main.update
        }
        |> ProgramTest.withSimulatedEffects simulateEffects
        |> ProgramTest.withSimulatedSubscriptions simulateSubscriptions
        |> ProgramTest.start
            (Encode.object
                [ ( "databaseUrl", Maybe.withDefault Encode.null (Maybe.map Encode.string url) )
                , ( "neonEndpoint", Encode.null )
                , ( "releaseId", Encode.string "0641607" )
                , ( "releaseUrl", Encode.null )
                , ( "region", Encode.string "arn1" )
                ]
            )


request : String -> String -> List String -> App -> App
request resolve method path =
    ProgramTest.simulateIncomingPort "interopToElm"
        (Encode.object
            [ ( "tag", Encode.string "Request" )
            , ( "resolve", Encode.string resolve )
            , ( "request"
              , Encode.object
                    [ ( "method", Encode.string method )
                    , ( "path", Encode.list Encode.string path )
                    , ( "query", Encode.object [] )
                    , ( "headers", Encode.object [] )
                    , ( "body", Encode.string "" )
                    ]
              )
            ]
        )


expectResponses : List Response -> App -> Expectation
expectResponses =
    expectSent responses


expectSent : (List Sent -> a) -> a -> App -> Expectation
expectSent pick expected =
    ProgramTest.expectOutgoingPortValues "interopFromElm" sent (pick >> Expect.equal expected)


responses : List Sent -> List Response
responses =
    List.filterMap
        (\value ->
            case value of
                Responded answer ->
                    Just answer

                Logged _ ->
                    Nothing
        )


logs : List Sent -> List ( String, String )
logs =
    List.filterMap
        (\value ->
            case value of
                Responded _ ->
                    Nothing

                Logged entry ->
                    Just entry
        )


levels : List Sent -> List String
levels =
    logs >> List.map Tuple.first


sent : Decode.Decoder Sent
sent =
    Decode.field "tag" Decode.string
        |> Decode.andThen
            (\tag ->
                case tag of
                    "Respond" ->
                        Decode.map Responded
                            (Decode.map4 Response
                                (Decode.field "resolve" Decode.string)
                                (Decode.field "status" Decode.int)
                                (Decode.field "headers"
                                    (Decode.list
                                        (Decode.map2 Tuple.pair
                                            (Decode.index 0 Decode.string)
                                            (Decode.index 1 Decode.string)
                                        )
                                    )
                                )
                                (Decode.field "body" Decode.string)
                            )

                    "Log" ->
                        Decode.map Logged
                            (Decode.map2 Tuple.pair
                                (Decode.field "level" Decode.string)
                                (Decode.field "message" Decode.string)
                            )

                    _ ->
                        Decode.fail ("Unknown tag: " ++ tag)
            )


simulateEffects : Main.Effect -> ProgramTest.SimulatedEffect Main.Msg
simulateEffects effect =
    case effect of
        Main.NoEffect ->
            SimulatedEffect.Cmd.none

        Main.Batch effects ->
            SimulatedEffect.Cmd.batch (List.map simulateEffects effects)

        Main.SendToJs value ->
            SimulatedEffect.Ports.send "interopFromElm"
                (TsEncode.encoder Interop.interop.fromElm value)

        Main.Database { method, headers, url, body, timeout, onResponse } ->
            SimulatedEffect.Time.now
                |> SimulatedEffect.Task.andThen
                    (\begin ->
                        SimulatedEffect.Http.task
                            { method = method
                            , headers = List.map (\( name, value ) -> SimulatedEffect.Http.header name value) headers
                            , url = url
                            , body = SimulatedEffect.Http.jsonBody body
                            , resolver = SimulatedEffect.Http.stringResolver (onResponse >> Ok)
                            , timeout = timeout
                            }
                            |> SimulatedEffect.Task.andThen
                                (\toMsg ->
                                    SimulatedEffect.Time.now
                                        |> SimulatedEffect.Task.map
                                            (\end -> toMsg (Time.posixToMillis end - Time.posixToMillis begin))
                                )
                    )
                |> SimulatedEffect.Task.perform identity


simulateSubscriptions : Main.Model -> ProgramTest.SimulatedSub Main.Msg
simulateSubscriptions _ =
    SimulatedEffect.Ports.subscribe "interopToElm"
        Decode.value
        (Decode.decodeValue (TsDecode.decoder Interop.interop.toElm) >> Main.GotInterop)
