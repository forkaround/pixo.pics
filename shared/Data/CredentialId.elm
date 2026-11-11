module Data.CredentialId exposing (CredentialId, decoder, encode, fromString, toString)

import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode


type CredentialId
    = CredentialId String


fromString : String -> Maybe CredentialId
fromString value =
    if String.isEmpty value then
        Nothing

    else
        Just (CredentialId value)


toString : CredentialId -> String
toString (CredentialId value) =
    value


decoder : Decoder CredentialId
decoder =
    Decode.string
        |> Decode.andThen
            (\value ->
                case fromString value of
                    Just id ->
                        Decode.succeed id

                    Nothing ->
                        Decode.fail "Expected a credential id, not an empty string."
            )


encode : CredentialId -> Encode.Value
encode (CredentialId value) =
    Encode.string value
