module Data.PictureId exposing (PictureId, decoder, encode, fromString, toString)

import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode


type PictureId
    = PictureId String


fromString : String -> Maybe PictureId
fromString raw =
    let
        value =
            String.toLower raw
    in
    if String.length value == 36 && List.all valid (List.indexedMap Tuple.pair (String.toList value)) then
        Just (PictureId value)

    else
        Nothing


valid : ( Int, Char ) -> Bool
valid ( index, char ) =
    if List.member index [ 8, 13, 18, 23 ] then
        char == '-'

    else
        Char.isDigit char || List.member char [ 'a', 'b', 'c', 'd', 'e', 'f' ]


toString : PictureId -> String
toString (PictureId value) =
    value


decoder : Decoder PictureId
decoder =
    Decode.string
        |> Decode.andThen
            (\value ->
                case fromString value of
                    Just id ->
                        Decode.succeed id

                    Nothing ->
                        Decode.fail "Expected a picture id."
            )


encode : PictureId -> Encode.Value
encode (PictureId value) =
    Encode.string value
