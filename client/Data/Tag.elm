module Data.Tag exposing (Tag, fromString, toString)


type Tag
    = Tag String


fromString : String -> Maybe Tag
fromString raw =
    let
        clean =
            raw
                |> String.toLower
                |> String.words
                |> String.join " "
                |> trimEnds
    in
    if
        String.length clean
            >= 2
            && String.length clean
            <= 24
            && List.length (String.words clean)
            <= 3
            && String.all allowed clean
    then
        Just (Tag clean)

    else
        Nothing


toString : Tag -> String
toString (Tag value) =
    value


allowed : Char -> Bool
allowed char =
    isLetter char || char == ' ' || char == '-'


isLetter : Char -> Bool
isLetter char =
    Char.isAlphaNum char || Char.toCode char > 127


trimEnds : String -> String
trimEnds text =
    text
        |> String.toList
        |> dropWhile (not << isLetter)
        |> List.reverse
        |> dropWhile (not << isLetter)
        |> List.reverse
        |> String.fromList


dropWhile : (a -> Bool) -> List a -> List a
dropWhile drop list =
    case list of
        first :: rest ->
            if drop first then
                dropWhile drop rest

            else
                list

        [] ->
            []
