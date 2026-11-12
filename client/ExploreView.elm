module ExploreView exposing (Hero(..), view)

import Html exposing (Html, div, h2, p, text)
import Html.Attributes exposing (attribute, class, href, style)
import Overlay exposing (solid)
import Path
import UI.Button as Button


type Hero
    = Shown
    | Reserved
    | Hidden


view : Hero -> Html msg
view hero =
    div [ class "flex flex-1 flex-col gap-4 p-4" ]
        [ viewHero hero
        , div [ class "columns-2 gap-4 md:columns-3 xl:columns-4 2xl:columns-5", attribute "aria-hidden" "true" ]
            (List.map viewTile tiles)
        ]


viewHero : Hero -> Html msg
viewHero hero =
    case hero of
        Shown ->
            banner []

        Reserved ->
            banner [ class "invisible", attribute "aria-hidden" "true" ]

        Hidden ->
            text ""


banner : List (Html.Attribute msg) -> Html msg
banner attrs =
    Html.section
        (class "flex flex-col items-start gap-4 rounded-xl bg-linear-to-br from-violet-600 via-fuchsia-600 to-orange-500 p-6 text-white shadow-sm md:flex-row md:items-center md:justify-between md:p-8"
            :: attrs
        )
        [ div [ class "flex flex-col gap-1" ]
            [ h2 [ class "text-xl font-semibold tracking-tight text-balance md:text-2xl" ]
                [ text "See what people are making" ]
            , p [ class "text-sm text-white/85 text-pretty" ]
                [ text "Every picture here was made in a browser. Sign up to share yours." ]
            ]
        , Button.new
            |> Button.text "Sign up"
            |> Button.toAnchor [ href (Path.toString Path.SignUp), solid, class "shrink-0" ]
        ]


type alias Tile =
    { shape : String
    , colors : String
    }


viewTile : Tile -> Html msg
viewTile tile =
    div
        [ class "mb-4 break-inside-avoid rounded-xl bg-linear-to-br shadow-sm ring-1 ring-foreground/10"
        , class tile.colors
        , style "aspect-ratio" tile.shape
        ]
        []


tiles : List Tile
tiles =
    [ Tile "4 / 5" "from-rose-300 to-orange-400"
    , Tile "1 / 1" "from-sky-300 to-indigo-500"
    , Tile "16 / 9" "from-emerald-300 to-teal-600"
    , Tile "9 / 16" "from-amber-200 to-rose-500"
    , Tile "2 / 3" "from-violet-300 to-fuchsia-600"
    , Tile "1 / 1" "from-lime-200 to-emerald-500"
    , Tile "2 / 1" "from-cyan-200 to-blue-600"
    , Tile "4 / 5" "from-fuchsia-300 to-purple-700"
    , Tile "16 / 9" "from-orange-200 to-red-500"
    , Tile "2 / 3" "from-teal-200 to-cyan-600"
    , Tile "1 / 1" "from-pink-300 to-rose-600"
    , Tile "9 / 16" "from-indigo-300 to-violet-700"
    , Tile "3 / 1" "from-yellow-200 to-amber-500"
    , Tile "4 / 5" "from-blue-300 to-sky-600"
    , Tile "1 / 1" "from-stone-300 to-stone-500"
    , Tile "16 / 9" "from-purple-300 to-pink-500"
    , Tile "2 / 3" "from-green-300 to-lime-600"
    , Tile "1 / 1" "from-red-300 to-orange-600"
    , Tile "4 / 5" "from-slate-300 to-slate-600"
    , Tile "2 / 1" "from-rose-200 to-fuchsia-500"
    ]
