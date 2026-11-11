module DataTest exposing (suite)

import Data.Challenge as Challenge
import Data.CredentialId as CredentialId
import Data.Email as Email
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
        ]
