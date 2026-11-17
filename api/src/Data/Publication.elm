module Data.Publication exposing (Publication, maxBytes, parse)

import Api
import Data.Tag as Tag exposing (Tag)


type alias Publication =
    { prompt : String
    , width : Int
    , height : Int
    , steps : Int
    , seed : Int
    , model : String
    , bytes : Int
    , tags : List Tag
    }


maxBytes : Int
maxBytes =
    2097152


parse : Api.Publication -> Result String Publication
parse raw =
    let
        prompt =
            String.trim raw.prompt
    in
    if raw.bytes < 1 || raw.bytes > maxBytes then
        Err "The picture must be at most 2 MB."

    else if String.isEmpty prompt || String.length prompt > 2000 then
        Err "The prompt must be 1 to 2000 characters."

    else if not (size raw.width) || not (size raw.height) then
        Err "The size is not one the studio makes."

    else if raw.steps < 1 || raw.steps > 50 then
        Err "Steps must be 1 to 50."

    else if raw.seed < 0 || raw.seed > 4294967295 then
        Err "The seed is out of range."

    else if String.isEmpty raw.model || String.length raw.model > 200 then
        Err "The model name is not valid."

    else if List.length raw.tags > 8 then
        Err "A picture has at most 8 tags."

    else
        Ok
            { prompt = prompt
            , width = raw.width
            , height = raw.height
            , steps = raw.steps
            , seed = raw.seed
            , model = raw.model
            , bytes = raw.bytes
            , tags = unique raw.tags
            }


size : Int -> Bool
size value =
    value >= 256 && value <= 1024 && modBy 64 value == 0


unique : List Tag -> List Tag
unique tags =
    List.foldl
        (\tag kept ->
            if List.member tag kept then
                kept

            else
                kept ++ [ tag ]
        )
        []
        tags
