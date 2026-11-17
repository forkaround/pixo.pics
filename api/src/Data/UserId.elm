module Data.UserId exposing (UserId, decoder, fromString, toString)

import Json.Decode as Decode exposing (Decoder)


type UserId
    = UserId String


fromString : String -> Maybe UserId
fromString value =
    let
        lower =
            String.toLower value
    in
    if String.length lower == 36 && List.all (isUuidChar lower) (List.range 0 35) then
        Just (UserId lower)

    else
        Nothing


isUuidChar : String -> Int -> Bool
isUuidChar value index =
    case String.uncons (String.dropLeft index value) of
        Just ( c, _ ) ->
            if List.member index [ 8, 13, 18, 23 ] then
                c == '-'

            else
                Char.isDigit c || (c >= 'a' && c <= 'f')

        Nothing ->
            False


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
                        Decode.fail "Expected a user id, a UUID."
            )
