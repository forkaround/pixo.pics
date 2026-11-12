module Data.Steps exposing (Steps, default, fewest, fromInt, most, toInt)


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


toInt : Steps -> Int
toInt (Steps count) =
    count
