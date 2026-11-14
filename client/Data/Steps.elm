module Data.Steps exposing (Steps, default, fewest, fromInt, fromString, most, toInt, toString)


type Steps
    = Steps Int


fewest : Int
fewest =
    1


most : Int
most =
    50


default : Steps
default =
    Steps 4


fromInt : Int -> Maybe Steps
fromInt count =
    if count >= fewest && count <= most then
        Just (Steps count)

    else
        Nothing


fromString : String -> Maybe Steps
fromString value =
    let
        clean =
            String.trim value
    in
    if not (String.isEmpty clean) && String.all Char.isDigit clean then
        String.toInt clean
            |> Maybe.andThen fromInt

    else
        Nothing


toString : Steps -> String
toString (Steps count) =
    String.fromInt count


toInt : Steps -> Int
toInt (Steps count) =
    count
