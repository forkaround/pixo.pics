module LinkToken exposing (LinkToken, decoder, fromString, toString)

import Json.Decode as Decode exposing (Decoder)


type LinkToken
    = LinkToken String


fromString : String -> Maybe LinkToken
fromString value =
    if String.isEmpty value then
        Nothing

    else
        Just (LinkToken value)


toString : LinkToken -> String
toString (LinkToken value) =
    value


decoder : Decoder LinkToken
decoder =
    Decode.string
        |> Decode.andThen
            (\value ->
                case fromString value of
                    Just token ->
                        Decode.succeed token

                    Nothing ->
                        Decode.fail "Expected a link token, not an empty string."
            )
