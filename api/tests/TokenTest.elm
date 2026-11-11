module TokenTest exposing (suite)

import Data.LinkToken as LinkToken
import Data.SessionToken as SessionToken
import Data.UserId as UserId
import Expect
import Json.Decode as Decode
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
        , describe "UserId"
            [ test "keeps the text it was decoded from" <|
                \() ->
                    Decode.decodeString UserId.decoder "\"7b0c9a52-3d0e-4a53-9a7e-0f6f1f3f5b11\""
                        |> Result.map UserId.toString
                        |> Expect.equal (Ok "7b0c9a52-3d0e-4a53-9a7e-0f6f1f3f5b11")
            , test "refuses the empty string" <|
                \() ->
                    Decode.decodeString UserId.decoder "\"\""
                        |> Result.toMaybe
                        |> Maybe.map UserId.toString
                        |> Expect.equal Nothing
            ]
        ]
