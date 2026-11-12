module Bm25Test exposing (suite)

import Bm25
import Expect
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "Bm25"
        [ describe "tokens"
            [ test "are lower-case words, without the punctuation" <|
                \() ->
                    Bm25.tokens "A red FOX, in the snow: golden-hour!"
                        |> Expect.equal [ "a", "red", "fox", "in", "the", "snow", "golden", "hour" ]
            , test "keep letters from other alphabets" <|
                \() ->
                    Bm25.tokens "東京 at night, Москва"
                        |> Expect.equal [ "東京", "at", "night", "москва" ]
            , test "lose their accents" <|
                \() ->
                    Bm25.tokens "Kraków, Łódź, a café in São Paulo: crème brûlée, Ærø, Straße"
                        |> Expect.equal [ "krakow", "lodz", "a", "cafe", "in", "sao", "paulo", "creme", "brulee", "aero", "strasse" ]
            , test "lose an accent that is written as its own character" <|
                \() ->
                    Bm25.tokens "Kraków"
                        |> Expect.equal [ "krakow" ]
            ]
        , describe "search"
            [ test "finds the documents that have the word" <|
                \() ->
                    Bm25.search "fox" prompts
                        |> Expect.equal [ 1 ]
            , test "finds nothing for a word no document has" <|
                \() ->
                    Bm25.search "submarine" prompts
                        |> Expect.equal []
            , test "finds nothing without a word to look for" <|
                \() ->
                    Bm25.search " ,. " prompts
                        |> Expect.equal []
            , test "takes the last word for the start of one, as it is typed" <|
                \() ->
                    Bm25.search "light" prompts
                        |> Expect.equal [ 2 ]
            , test "takes the words before it whole" <|
                \() ->
                    Bm25.search "light sunset" prompts
                        |> Expect.equal [ 3, 2 ]
            , test "puts the document with more of the words first" <|
                \() ->
                    Bm25.search "lighthouse sunset" prompts
                        |> Expect.equal [ 2, 3 ]
            , test "counts a rare word for more than a common one" <|
                \() ->
                    Bm25.search "snow a fox" prompts
                        |> List.head
                        |> Expect.equal (Just 1)
            , test "prefers the shorter of two documents that say it as often" <|
                \() ->
                    Bm25.search "bonsai"
                        (Bm25.fromList
                            [ ( 1, "a bonsai on a misty mountain top at dawn" )
                            , ( 2, "a bonsai" )
                            ]
                        )
                        |> Expect.equal [ 2, 1 ]
            , test "puts the newer of two equal documents first" <|
                \() ->
                    Bm25.search "bonsai" (Bm25.fromList [ ( 1, "a bonsai" ), ( 2, "a bonsai" ) ])
                        |> Expect.equal [ 2, 1 ]
            , test "finds a word whatever accents it is typed or written with" <|
                \() ->
                    List.map
                        (\query ->
                            Bm25.search query
                                (Bm25.fromList
                                    [ ( 1, "the old town of krakow at dawn" )
                                    , ( 2, "Kraków in the snow" )
                                    , ( 3, "a bowl of ramen" )
                                    ]
                                )
                        )
                        [ "krakow", "kraków", "KRAKÓW", "krak" ]
                        |> Expect.equal (List.repeat 4 [ 2, 1 ])
            , test "finds nothing in an empty index" <|
                \() ->
                    Bm25.search "bonsai" Bm25.empty
                        |> Expect.equal []
            ]
        ]


prompts : Bm25.Index
prompts =
    Bm25.fromList
        [ ( 1, "a red fox in a snowy forest, snow on its fur" )
        , ( 2, "a lighthouse on a cliff at sunset, oil painting" )
        , ( 3, "a city skyline at sunset" )
        , ( 4, "a bowl of ramen" )
        ]
