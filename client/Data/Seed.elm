module Data.Seed exposing (Seed, fromString, generator, largest, toInt, toString)

import Random


type Seed
    = Seed Int


largest : Int
largest =
    4294967295


fromString : String -> Maybe Seed
fromString value =
    let
        clean =
            String.trim value
    in
    if not (String.isEmpty clean) && String.all Char.isDigit clean then
        String.toInt clean
            |> Maybe.andThen
                (\number ->
                    if number <= largest then
                        Just (Seed number)

                    else
                        Nothing
                )

    else
        Nothing


generator : Random.Generator Seed
generator =
    Random.map Seed (Random.int 0 Random.maxInt)


toInt : Seed -> Int
toInt (Seed number) =
    number


toString : Seed -> String
toString (Seed number) =
    String.fromInt number
