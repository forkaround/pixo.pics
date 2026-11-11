module Data.Username exposing (Username, decoder, encode, parse, toString)

import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode


type Username
    = Username String


parse : String -> Result String Username
parse value =
    let
        clean =
            String.toLower (String.trim value)
    in
    if isUsername clean then
        Ok (Username clean)

    else
        Err "A username has 3 to 20 characters: letters, digits and underscores."


decoder : Decoder Username
decoder =
    Decode.string
        |> Decode.andThen
            (\value ->
                case parse value of
                    Ok parsed ->
                        Decode.succeed parsed

                    Err problem ->
                        Decode.fail problem
            )


encode : Username -> Encode.Value
encode (Username value) =
    Encode.string value


toString : Username -> String
toString (Username value) =
    value


isUsername : String -> Bool
isUsername username =
    (String.length username >= 3)
        && (String.length username <= 20)
        && String.all (\char -> Char.isLower char || Char.isDigit char || char == '_') username
