module EmailAddress exposing (EmailAddress, parse, toString)


type EmailAddress
    = EmailAddress String


parse : String -> Result String EmailAddress
parse value =
    let
        clean =
            String.toLower (String.trim value)
    in
    if isEmail clean then
        Ok (EmailAddress clean)

    else
        Err "Enter a valid email address."


toString : EmailAddress -> String
toString (EmailAddress value) =
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
