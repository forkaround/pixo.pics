module DataTest exposing (suite)

import Data.Challenge as Challenge
import Data.CredentialId as CredentialId
import Data.Dimension as Dimension
import Data.Email as Email
import Data.Prompt as Prompt
import Data.Seed as Seed
import Data.Steps as Steps
import Data.Username as Username
import Expect
import Json.Decode as Decode
import Json.Encode as Encode
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "Data"
        [ describe "Email"
            [ test "decodes, trimmed and lower-cased" <|
                \() ->
                    Decode.decodeString Email.decoder "\" Ada@Example.COM \""
                        |> Result.mapError (\_ -> ())
                        |> Result.map Email.toString
                        |> Expect.equal (Ok "ada@example.com")
            , test "refuses what is not an address" <|
                \() ->
                    Decode.decodeString Email.decoder "\"ada\""
                        |> Result.toMaybe
                        |> Expect.equal Nothing
            , test "encodes as its text" <|
                \() ->
                    Email.parse "ada@example.com"
                        |> Result.map (Email.encode >> Encode.encode 0)
                        |> Expect.equal (Ok "\"ada@example.com\"")
            ]
        , describe "Username"
            [ test "decodes, trimmed and lower-cased" <|
                \() ->
                    Decode.decodeString Username.decoder "\" Ada_99 \""
                        |> Result.mapError (\_ -> ())
                        |> Result.map Username.toString
                        |> Expect.equal (Ok "ada_99")
            , test "refuses one that is too short" <|
                \() ->
                    Decode.decodeString Username.decoder "\"ad\""
                        |> Result.toMaybe
                        |> Expect.equal Nothing
            , test "encodes as its text" <|
                \() ->
                    Username.parse "ada_99"
                        |> Result.map (Username.encode >> Encode.encode 0)
                        |> Expect.equal (Ok "\"ada_99\"")
            ]
        , describe "CredentialId"
            [ test "refuses the empty string" <|
                \() ->
                    CredentialId.fromString ""
                        |> Maybe.map CredentialId.toString
                        |> Expect.equal Nothing
            , test "decodes and keeps its text" <|
                \() ->
                    Decode.decodeString CredentialId.decoder "\"Y2hhbGxlbmdl\""
                        |> Result.mapError (\_ -> ())
                        |> Result.map CredentialId.toString
                        |> Expect.equal (Ok "Y2hhbGxlbmdl")
            ]
        , describe "Challenge"
            [ test "refuses the empty string" <|
                \() ->
                    Challenge.fromString ""
                        |> Maybe.map Challenge.toString
                        |> Expect.equal Nothing
            , test "decodes and keeps its text" <|
                \() ->
                    Decode.decodeString Challenge.decoder "\"Y2hhbGxlbmdl\""
                        |> Result.mapError (\_ -> ())
                        |> Result.map Challenge.toString
                        |> Expect.equal (Ok "Y2hhbGxlbmdl")
            ]
        , describe "Dimension"
            [ test "takes what is on its grid, ends included" <|
                \() ->
                    List.filterMap Dimension.fromInt [ 256, 576, 1024 ]
                        |> List.map Dimension.toInt
                        |> Expect.equal [ 256, 576, 1024 ]
            , test "refuses what is too small, too large or off its grid" <|
                \() ->
                    List.filterMap Dimension.fromInt [ 192, 1088, 500, 272 ]
                        |> Expect.equal []
            , test "is always a multiple of 16, which the model needs" <|
                \() ->
                    List.range Dimension.smallest Dimension.largest
                        |> List.filterMap Dimension.fromInt
                        |> List.filter (\dimension -> modBy 16 (Dimension.toInt dimension) /= 0)
                        |> Expect.equal []
            ]
        , describe "Steps"
            [ test "takes 1 to 50" <|
                \() ->
                    List.filterMap Steps.fromInt [ 0, 1, 4, 50, 51 ]
                        |> List.map Steps.toInt
                        |> Expect.equal [ 1, 4, 50 ]
            ]
        , describe "Seed"
            [ test "reads a whole number, trimmed" <|
                \() ->
                    Seed.fromString " 1234 "
                        |> Maybe.map Seed.toString
                        |> Expect.equal (Just "1234")
            , test "refuses what the model would wrap around" <|
                \() ->
                    List.filterMap Seed.fromString [ "4294967296", "-1", "1e3", "" ]
                        |> Expect.equal []
            ]
        , describe "Prompt"
            [ test "is trimmed" <|
                \() ->
                    Prompt.parse "  a bonsai\n"
                        |> Maybe.map Prompt.toString
                        |> Expect.equal (Just "a bonsai")
            , test "is never blank" <|
                \() ->
                    Prompt.parse " \n "
                        |> Expect.equal Nothing
            ]
        ]
