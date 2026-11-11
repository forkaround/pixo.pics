module Data.SessionToken exposing (SessionToken, decoder, fromString, toString)

import Json.Decode as Decode exposing (Decoder)


type SessionToken
    = SessionToken String


fromString : String -> Maybe SessionToken
fromString value =
    if String.isEmpty value then
        Nothing

    else
        Just (SessionToken value)


toString : SessionToken -> String
toString (SessionToken value) =
    value


decoder : Decoder SessionToken
decoder =
    Decode.string
        |> Decode.andThen
            (\value ->
                case fromString value of
                    Just token ->
                        Decode.succeed token

                    Nothing ->
                        Decode.fail "Expected a session token, not an empty string."
            )
