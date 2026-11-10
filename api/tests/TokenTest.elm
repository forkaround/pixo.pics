module TokenTest exposing (suite)

import Expect
import Json.Decode as Decode
import LinkToken
import SessionToken
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "Tokens"
        [ describe "SessionToken"
            [ test "keeps the text it was made from" <|
                \() ->
                    SessionToken.fromString "c2Vzc2lvbg"
                        |> Maybe.map SessionToken.toString
                        |> Expect.equal (Just "c2Vzc2lvbg")
            , test "is never empty" <|
                \() ->
                    ( SessionToken.fromString ""
                        |> Maybe.map SessionToken.toString
                    , Decode.decodeString SessionToken.decoder "\"\""
                        |> Result.toMaybe
                        |> Maybe.map SessionToken.toString
                    )
                        |> Expect.equal ( Nothing, Nothing )
            ]
        , describe "LinkToken"
            [ test "keeps the text it was made from" <|
                \() ->
                    Decode.decodeString LinkToken.decoder "\"Zm9vYmFy\""
                        |> Result.map LinkToken.toString
                        |> Expect.equal (Ok "Zm9vYmFy")
            , test "is never empty" <|
                \() ->
                    ( LinkToken.fromString ""
                        |> Maybe.map LinkToken.toString
                    , Decode.decodeString LinkToken.decoder "\"\""
                        |> Result.toMaybe
                        |> Maybe.map LinkToken.toString
                    )
                        |> Expect.equal ( Nothing, Nothing )
            ]
        ]
