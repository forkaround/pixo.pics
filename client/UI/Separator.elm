module UI.Separator exposing
    ( Separator
    , new
    , toHtml
    , vertical
    )

import Html exposing (Attribute, Html)
import Html.Attributes exposing (attribute, class)


type Separator
    = Separator { orientation : Orientation }


type Orientation
    = Horizontal
    | Vertical


new : Separator
new =
    Separator { orientation = Horizontal }


vertical : Separator -> Separator
vertical (Separator config) =
    Separator { config | orientation = Vertical }


toHtml : List (Attribute msg) -> Separator -> Html msg
toHtml attrs (Separator config) =
    Html.div
        ([ attribute "data-slot" "separator"
         , attribute "role" "none"
         , attribute "data-orientation" (orientationName config.orientation)
         , class "shrink-0 bg-border data-[orientation=horizontal]:h-px data-[orientation=horizontal]:w-full data-[orientation=vertical]:w-px data-[orientation=vertical]:self-stretch"
         ]
            ++ attrs
        )
        []


orientationName : Orientation -> String
orientationName orientation =
    case orientation of
        Horizontal ->
            "horizontal"

        Vertical ->
            "vertical"
