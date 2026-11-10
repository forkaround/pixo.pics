module Username exposing (Username, parse, toString)


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


toString : Username -> String
toString (Username value) =
    value


isUsername : String -> Bool
isUsername username =
    (String.length username >= 3)
        && (String.length username <= 20)
        && String.all (\char -> Char.isLower char || Char.isDigit char || char == '_') username
