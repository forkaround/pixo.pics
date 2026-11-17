module PublicationTest exposing (suite)

import Api
import Data.Publication as Publication
import Data.Tag as Tag
import Expect
import Test exposing (Test, describe, test)


valid : Api.Publication
valid =
    { prompt = "a red fox"
    , width = 1024
    , height = 768
    , steps = 4
    , seed = 7
    , model = "prism-ml/bonsai-image-ternary-4B-mlx-2bit"
    , bytes = 312345
    , tags = List.filterMap Tag.fromString [ "fox", "snow" ]
    }


refuses : String -> String -> Api.Publication -> Test
refuses name message raw =
    test name <|
        \() ->
            Publication.parse raw
                |> Result.map (\_ -> ())
                |> Expect.equal (Err message)


suite : Test
suite =
    describe "Publication"
        [ test "accepts what the studio makes, with the prompt trimmed" <|
            \() ->
                Publication.parse { valid | prompt = "  a red fox " }
                    |> Result.map (\p -> { prompt = p.prompt, size = ( p.width, p.height ), bytes = p.bytes, tags = List.map Tag.toString p.tags })
                    |> Expect.equal (Ok { prompt = "a red fox", size = ( 1024, 768 ), bytes = 312345, tags = [ "fox", "snow" ] })
        , test "drops duplicate tags, keeping the first" <|
            \() ->
                Publication.parse { valid | tags = List.filterMap Tag.fromString [ "fox", "snow", "fox" ] }
                    |> Result.map (.tags >> List.map Tag.toString)
                    |> Expect.equal (Ok [ "fox", "snow" ])
        , refuses "refuses no bytes" "The picture must be at most 2 MB." { valid | bytes = 0 }
        , refuses "refuses too many bytes" "The picture must be at most 2 MB." { valid | bytes = 2097153 }
        , refuses "refuses a blank prompt" "The prompt must be 1 to 2000 characters." { valid | prompt = "   " }
        , refuses "refuses a prompt of 2001 characters" "The prompt must be 1 to 2000 characters." { valid | prompt = String.repeat 2001 "a" }
        , refuses "refuses a width of 100" "The size is not one the studio makes." { valid | width = 100 }
        , refuses "refuses a height of 1088" "The size is not one the studio makes." { valid | height = 1088 }
        , refuses "refuses a width that is not a multiple of 64" "The size is not one the studio makes." { valid | width = 1000 }
        , refuses "refuses no steps" "Steps must be 1 to 50." { valid | steps = 0 }
        , refuses "refuses 51 steps" "Steps must be 1 to 50." { valid | steps = 51 }
        , refuses "refuses a negative seed" "The seed is out of range." { valid | seed = -1 }
        , refuses "refuses a seed past 32 bits" "The seed is out of range." { valid | seed = 4294967296 }
        , refuses "refuses no model" "The model name is not valid." { valid | model = "" }
        , refuses "refuses a model name of 201 characters" "The model name is not valid." { valid | model = String.repeat 201 "m" }
        , refuses "refuses nine tags"
            "A picture has at most 8 tags."
            { valid
                | tags =
                    List.filterMap Tag.fromString [ "one", "two", "three", "four", "five", "six", "seven", "eight", "nine" ]
            }
        ]
