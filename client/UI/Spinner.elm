module UI.Spinner exposing (view)

import Html exposing (Attribute, Html)
import Html.Attributes exposing (attribute, class)


view : List (Attribute msg) -> Html msg
view attrs =
    Html.span
        ([ attribute "data-slot" "spinner"
         , attribute "role" "status"
         , attribute "aria-label" "Loading"
         , class "inline-block size-4 shrink-0 animate-spin rounded-full border-2 border-current border-t-transparent"
         ]
            ++ attrs
        )
        []
