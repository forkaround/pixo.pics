module SignUp exposing (validate, validateEmail)

import Api


validate : Api.SignUp -> Result String Api.SignUp
validate { email, username } =
    let
        cleanUsername =
            String.toLower (String.trim username)
    in
    validateEmail email
        |> Result.andThen
            (\cleanEmail ->
                if isUsername cleanUsername then
                    Ok { email = cleanEmail, username = cleanUsername }

                else
                    Err "A username has 3 to 20 characters: letters, digits and underscores."
            )


validateEmail : String -> Result String String
validateEmail email =
    let
        cleanEmail =
            String.toLower (String.trim email)
    in
    if isEmail cleanEmail then
        Ok cleanEmail

    else
        Err "Enter a valid email address."


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


isUsername : String -> Bool
isUsername username =
    (String.length username >= 3)
        && (String.length username <= 20)
        && String.all (\char -> Char.isLower char || Char.isDigit char || char == '_') username
