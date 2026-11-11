module NeonTest exposing (suite)

import Expect exposing (Expectation)
import Http
import Json.Decode as Decode
import Json.Encode as Encode
import Services.Neon as Neon
import Test exposing (Test, describe, test)
import Test.Http


suite : Test
suite =
    describe "Neon"
        [ describe "connect"
            [ test "sends queries to the API host of the database's region" <|
                \() ->
                    connected
                        (\connection ->
                            Neon.endpoint connection
                                |> Expect.equal "https://api.eu-central-1.aws.neon.tech/sql"
                        )
            , test "refuses a connection string without a host" <|
                \() ->
                    Neon.connect "ep-test-123456.eu-central-1.aws.neon.tech"
                        |> Expect.err
            ]
        , describe "run"
            [ test "sends values as text parameters, apart from the SQL" <|
                \() ->
                    connected
                        (\connection ->
                            Neon.run connection
                                (Neon.execute "UPDATE users SET username = $1, age = $2, bio = $3 WHERE id = $4"
                                    [ Neon.text "'; DROP TABLE users; --", Neon.int 30, Neon.null, Neon.bool True ]
                                )
                                identity
                                |> .body
                                |> Encode.encode 0
                                |> Expect.equal """{"query":"UPDATE users SET username = $1, age = $2, bio = $3 WHERE id = $4","params":["'; DROP TABLE users; --","30",null,"true"]}"""
                        )
            , test "decodes the rows" <|
                \() ->
                    answer (Neon.select "SELECT n FROM numbers" [] number)
                        (response 200 """{"rows":[{"n":1},{"n":2}],"rowCount":2}""")
                        |> Expect.equal (Ok [ 1, 2 ])
            , test "gives the row count of a statement without rows" <|
                \() ->
                    answer (Neon.execute "DELETE FROM numbers" [])
                        (response 200 """{"rows":[],"rowCount":3}""")
                        |> Expect.equal (Ok 3)
            , test "fails one unless exactly one row comes back" <|
                \() ->
                    answer (Neon.one "SELECT n FROM numbers" [] number)
                        (response 200 """{"rows":[],"rowCount":0}""")
                        |> Expect.equal (Err (Neon.UnexpectedResult "Expected one row, got 0."))
            , test "reads the Postgres error from a 400" <|
                \() ->
                    answer (Neon.select "SELECT n FROM numbers" [] number)
                        (response 400 """{"message":"permission denied for table numbers","code":"42501"}""")
                        |> Result.mapError Neon.errorToString
                        |> Expect.equal (Err "permission denied for table numbers (42501)")
            , test "keeps the status and body of any other error" <|
                \() ->
                    answer (Neon.select "SELECT n FROM numbers" [] number)
                        (response 502 "Bad gateway")
                        |> Expect.equal (Err (Neon.ServerError 502 "Bad gateway"))
            ]
        , describe "commit"
            [ test "sends the queries in order and gives each its own result" <|
                \() ->
                    connected
                        (\connection ->
                            let
                                request =
                                    Neon.commit connection
                                        (Neon.transaction Tuple.pair
                                            |> Neon.with (Neon.one "SELECT 1 AS n" [] number)
                                            |> Neon.with (Neon.one "SELECT 2 AS n" [] number)
                                        )
                                        identity
                            in
                            ( Encode.encode 0 request.body
                            , request.onResponse
                                (response 200 """{"results":[{"rows":[{"n":1}]},{"rows":[{"n":2}]}]}""")
                            )
                                |> Expect.equal
                                    ( """{"queries":[{"query":"SELECT 1 AS n","params":[]},{"query":"SELECT 2 AS n","params":[]}]}"""
                                    , Ok ( 1, 2 )
                                    )
                        )
            ]
        ]


connected : (Neon.Connection -> Expectation) -> Expectation
connected expect =
    case Neon.connect "postgresql://api:secret@ep-test-123456.eu-central-1.aws.neon.tech/app" of
        Ok connection ->
            expect connection

        Err problem ->
            Expect.fail problem


answer : Neon.Query a -> Http.Response String -> Result Neon.Error a
answer query httpResponse =
    case Neon.connect "postgresql://api:secret@ep-test-123456.eu-central-1.aws.neon.tech/app" of
        Ok connection ->
            (Neon.run connection query identity).onResponse httpResponse

        Err problem ->
            Err (Neon.BadUrl problem)


number : Decode.Decoder Int
number =
    Decode.field "n" Decode.int


response : Int -> String -> Http.Response String
response statusCode body =
    Test.Http.httpResponse { statusCode = statusCode, headers = [], body = body }
