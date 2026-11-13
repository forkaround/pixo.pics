module Ceremony exposing (Ceremony(..), problem)

import Html exposing (Html, text)
import UI.Field as Field


type Ceremony
    = Resting (Maybe String)
    | Waiting


problem : Ceremony -> Html msg
problem ceremony =
    case ceremony of
        Resting (Just reason) ->
            Field.error [] [ text reason ]

        _ ->
            text ""
