module Data.Prompt exposing (Prompt, parse, toString)


type Prompt
    = Prompt String


parse : String -> Maybe Prompt
parse value =
    let
        clean =
            String.trim value
    in
    if String.isEmpty clean then
        Nothing

    else
        Just (Prompt clean)


toString : Prompt -> String
toString (Prompt value) =
    value
