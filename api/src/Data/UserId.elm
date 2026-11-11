module Data.UserId exposing (UserId, decoder, toString)

import Json.Decode as Decode exposing (Decoder)


type UserId
    = UserId String


fromString : String -> Maybe UserId
fromString value =
    if String.isEmpty value then
        Nothing

    else
        Just (UserId value)


toString : UserId -> String
toString (UserId value) =
    value


decoder : Decoder UserId
decoder =
    Decode.string
        |> Decode.andThen
            (\value ->
                case fromString value of
                    Just id ->
                        Decode.succeed id

                    Nothing ->
                        Decode.fail "Expected a user id, not an empty string."
            )
