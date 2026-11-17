module Data.Tag exposing (Tag, decoder, encode, fromString, toString)

import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode


type Tag
    = Tag String


fromString : String -> Maybe Tag
fromString raw =
    let
        clean =
            raw
                |> String.toLower
                |> String.words
                |> String.join " "
                |> trimEnds
    in
    if
        String.length clean
            >= 2
            && String.length clean
            <= 24
            && List.length (String.words clean)
            <= 3
            && String.all allowed clean
    then
        Just (Tag clean)

    else
        Nothing


toString : Tag -> String
toString (Tag value) =
    value


decoder : Decoder Tag
decoder =
    Decode.string
        |> Decode.andThen
            (\value ->
                case fromString value of
                    Just tag ->
                        Decode.succeed tag

                    Nothing ->
                        Decode.fail "Expected a tag."
            )


encode : Tag -> Encode.Value
encode (Tag value) =
    Encode.string value


allowed : Char -> Bool
allowed char =
    isLetter char || char == ' ' || char == '-'


isLetter : Char -> Bool
isLetter char =
    Char.isAlphaNum char || Char.toCode char > 127


trimEnds : String -> String
trimEnds text =
    text
        |> String.toList
        |> dropWhile (not << isLetter)
        |> List.reverse
        |> dropWhile (not << isLetter)
        |> List.reverse
        |> String.fromList


dropWhile : (a -> Bool) -> List a -> List a
dropWhile drop list =
    case list of
        first :: rest ->
            if drop first then
                dropWhile drop rest

            else
                list

        [] ->
            []
