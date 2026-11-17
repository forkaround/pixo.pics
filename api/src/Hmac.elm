module Hmac exposing (sha256, toHex, utf8)

import Bitwise
import Bytes
import Bytes.Decode as Decode
import Bytes.Encode as Encode
import SHA256


utf8 : String -> List Int
utf8 text =
    let
        bytes =
            Encode.encode (Encode.string text)
    in
    Decode.decode (Decode.loop ( Bytes.width bytes, [] ) byteStep) bytes
        |> Maybe.withDefault []


byteStep : ( Int, List Int ) -> Decode.Decoder (Decode.Step ( Int, List Int ) (List Int))
byteStep ( remaining, collected ) =
    if remaining <= 0 then
        Decode.succeed (Decode.Done (List.reverse collected))

    else
        Decode.map (\byte -> Decode.Loop ( remaining - 1, byte :: collected )) Decode.unsignedInt8


sha256 : List Int -> List Int -> List Int
sha256 key message =
    let
        shortKey =
            if List.length key > 64 then
                digest key

            else
                key

        padded =
            shortKey ++ List.repeat (64 - List.length shortKey) 0

        inner =
            digest (List.map (Bitwise.xor 0x36) padded ++ message)
    in
    digest (List.map (Bitwise.xor 0x5C) padded ++ inner)


digest : List Int -> List Int
digest bytes =
    SHA256.toByteValues (SHA256.fromByteValues bytes)


toHex : List Int -> String
toHex bytes =
    bytes
        |> List.map hexByte
        |> String.concat


hexByte : Int -> String
hexByte byte =
    String.fromList [ hexDigit (byte // 16), hexDigit (modBy 16 byte) ]


hexDigit : Int -> Char
hexDigit n =
    String.slice n (n + 1) "0123456789abcdef"
        |> String.toList
        |> List.head
        |> Maybe.withDefault '0'
