module Logo exposing (wordmark)

import Html.Attributes exposing (attribute)
import Svg exposing (Attribute, Svg, path, svg)
import Svg.Attributes exposing (d, fill, stroke, strokeWidth, viewBox)


wordmark : List (Attribute msg) -> Svg msg
wordmark attrs =
    svg
        ([ viewBox "-1.25 -1.25 150 42.5"
         , fill "none"
         , stroke "currentColor"
         , strokeWidth "5"
         , attribute "aria-hidden" "true"
         ]
            ++ attrs
        )
        [ path
            [ d "M1.25 41.25 L1.25 1.25 L17 1.25 C22.2467 1.25 26.5 5.5033 26.5 10.75 C26.5 15.9967 22.2467 20.25 17 20.25 L1.25 20.25 M40 1.25 L60 1.25 M50 1.25 L50 38.75 M40 38.75 L60 38.75 M74.5 1.25 L102.5 1.25 L74.5 38.75 L102.5 38.75 Z M128.25 1.25 L136.25 1.25 C141.7728 1.25 146.25 5.7272 146.25 11.25 L146.25 28.75 C146.25 34.2728 141.7728 38.75 136.25 38.75 L128.25 38.75 C122.7272 38.75 118.25 34.2728 118.25 28.75 L118.25 11.25 C118.25 5.7272 122.7272 1.25 128.25 1.25 Z"
            , attribute "data-collapsed" "M6 38.75 L34 1.25 L6 1.25 C6 1.25 6 1.25 6 1.25 C6 1.25 6 1.25 6 1.25 L34 38.75 M6 1.25 L34 1.25 M6 1.25 L34 38.75 M6 38.75 L34 38.75 M6 1.25 L34 1.25 L6 38.75 L34 38.75 Z M6 1.25 L34 1.25 C34 1.25 34 1.25 34 1.25 L6 38.75 C6 38.75 6 38.75 6 38.75 L34 38.75 C34 38.75 34 38.75 34 38.75 L6 1.25 C6 1.25 6 1.25 6 1.25 Z"
            ]
            []
        ]
