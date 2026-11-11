module Data.Email exposing (Email, decoder, encode, parse, toString)

import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode


type Email
    = Email String


parse : String -> Result String Email
parse value =
    let
        clean =
            String.toLower (String.trim value)
    in
    if isEmail clean then
        Ok (Email clean)

    else
        Err "Enter a valid email address."


decoder : Decoder Email
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


encode : Email -> Encode.Value
encode (Email value) =
    Encode.string value


toString : Email -> String
toString (Email value) =
    value


isEmail : String -> Bool
isEmail email =
    case String.split "@" email of
        [ local, domain ] ->
            not (String.isEmpty local)
                && (String.length email <= 254)
                && not (String.any isForbidden email)
                && List.all (not << String.isEmpty) (String.split "." domain)
                && (List.length (String.split "." domain) >= 2)

        _ ->
            False


isForbidden : Char -> Bool
isForbidden char =
    let
        code =
            Char.toCode char
    in
    code <= 32 || code == 127 || List.member char [ '<', '>', '(', ')', '[', ']', '\\', ',', ';', ':', '"' ]
