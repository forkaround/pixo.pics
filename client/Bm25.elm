module Bm25 exposing (Index, empty, fromList, search, tokens)

import Dict exposing (Dict)


type Index
    = Index
        { documents : List Document
        , average : Float
        }


type alias Document =
    { id : Int
    , length : Int
    , terms : Dict String Int
    }


type Term
    = Whole String
    | Start String


saturation : Float
saturation =
    1.2


normalization : Float
normalization =
    0.75


empty : Index
empty =
    fromList []


fromList : List ( Int, String ) -> Index
fromList texts =
    let
        documents =
            List.map document texts
    in
    Index
        { documents = documents
        , average =
            if List.isEmpty documents then
                0

            else
                toFloat (List.sum (List.map .length documents)) / toFloat (List.length documents)
        }


document : ( Int, String ) -> Document
document ( id, text ) =
    let
        words =
            tokens text
    in
    { id = id
    , length = List.length words
    , terms = List.foldl (\word -> Dict.update word (Maybe.withDefault 0 >> (+) 1 >> Just)) Dict.empty words
    }


tokens : String -> List String
tokens text =
    String.toLower text
        |> String.foldr (\char folded -> fold char ++ folded) ""
        |> String.words
        |> List.filter (not << String.isEmpty)


fold : Char -> String
fold char =
    let
        code =
            Char.toCode char
    in
    if code < 128 then
        if Char.isAlphaNum char then
            String.fromChar char

        else
            " "

    else if code >= 0x0300 && code <= 0x036F then
        ""

    else
        Maybe.withDefault (String.fromChar char) (Dict.get char plain)


plain : Dict Char String
plain =
    List.concatMap (\( letters, accented ) -> List.map (\char -> ( char, letters )) (String.toList accented))
        [ ( "a", "àáâãäåāăąǎ" )
        , ( "ae", "æ" )
        , ( "c", "çćĉċč" )
        , ( "d", "ďđð" )
        , ( "e", "èéêëēĕėęě" )
        , ( "g", "ĝğġģ" )
        , ( "h", "ĥħ" )
        , ( "i", "ìíîïĩīĭįı" )
        , ( "j", "ĵ" )
        , ( "k", "ķ" )
        , ( "l", "ĺļľŀł" )
        , ( "n", "ñńņňŉ" )
        , ( "o", "òóôõöøōŏőǒ" )
        , ( "oe", "œ" )
        , ( "r", "ŕŗř" )
        , ( "s", "śŝşšș" )
        , ( "ss", "ß" )
        , ( "t", "ţťŧț" )
        , ( "th", "þ" )
        , ( "u", "ùúûüũūŭůűųǔ" )
        , ( "w", "ŵ" )
        , ( "y", "ýÿŷ" )
        , ( "z", "źżž" )
        ]
        |> Dict.fromList


search : String -> Index -> List Int
search query (Index { documents, average }) =
    let
        total =
            toFloat (List.length documents)
    in
    terms (tokens query)
        |> List.map (weigh total average documents)
        |> List.foldl
            (\scores totals ->
                Dict.foldl (\id score -> Dict.update id (Maybe.withDefault 0 >> (+) score >> Just)) totals scores
            )
            Dict.empty
        |> Dict.toList
        |> List.sortBy (\( id, score ) -> ( negate score, negate id ))
        |> List.map Tuple.first


terms : List String -> List Term
terms words =
    case List.reverse words of
        [] ->
            []

        last :: before ->
            List.reverse (Start last :: List.map Whole (unique before))


unique : List String -> List String
unique words =
    List.foldr
        (\word found ->
            if List.member word found then
                found

            else
                word :: found
        )
        []
        words


weigh : Float -> Float -> List Document -> Term -> Dict Int Float
weigh total average documents term =
    let
        found =
            List.filterMap
                (\known ->
                    case frequency term known.terms of
                        0 ->
                            Nothing

                        count ->
                            Just ( known, toFloat count )
                )
                documents

        matching =
            toFloat (List.length found)

        rarity =
            logBase e (1 + (total - matching + 0.5) / (matching + 0.5))
    in
    List.map
        (\( known, count ) ->
            ( known.id
            , rarity
                * count
                * (saturation + 1)
                / (count + saturation * (1 - normalization + normalization * toFloat known.length / average))
            )
        )
        found
        |> Dict.fromList


frequency : Term -> Dict String Int -> Int
frequency term counts =
    case term of
        Whole word ->
            Maybe.withDefault 0 (Dict.get word counts)

        Start prefix ->
            Dict.foldl
                (\word count sum ->
                    if String.startsWith prefix word then
                        sum + count

                    else
                        sum
                )
                0
                counts
