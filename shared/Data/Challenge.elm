module Data.Challenge exposing (Challenge, decoder, encode, fromString, toString)

import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode


type Challenge
    = Challenge String


fromString : String -> Maybe Challenge
fromString value =
    if String.isEmpty value then
        Nothing

    else
        Just (Challenge value)


toString : Challenge -> String
toString (Challenge value) =
    value


decoder : Decoder Challenge
decoder =
    Decode.string
        |> Decode.andThen
            (\value ->
                case fromString value of
                    Just challenge ->
                        Decode.succeed challenge

                    Nothing ->
                        Decode.fail "Expected a challenge, not an empty string."
            )


encode : Challenge -> Encode.Value
encode (Challenge value) =
    Encode.string value
