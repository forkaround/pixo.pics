module Data.Dimension exposing (Dimension, default, fromInt, largest, smallest, step, toInt)


type Dimension
    = Dimension Int


smallest : Int
smallest =
    256


largest : Int
largest =
    1024


step : Int
step =
    64


default : Dimension
default =
    Dimension 512


fromInt : Int -> Maybe Dimension
fromInt pixels =
    if pixels >= smallest && pixels <= largest && modBy step pixels == 0 then
        Just (Dimension pixels)

    else
        Nothing


toInt : Dimension -> Int
toInt (Dimension pixels) =
    pixels
