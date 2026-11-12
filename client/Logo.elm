module Logo exposing (wordmark)

import Html.Attributes exposing (attribute)
import Svg exposing (Attribute, Svg, path, rect, svg)
import Svg.Attributes exposing (d, fill, height, rx, stroke, strokeWidth, viewBox, width, x, y)


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
        [ path [ d "M1.25 41.25V1.25H17a9.5 9.5 0 0 1 0 19H1.25" ] []
        , path [ d "M40 1.25h20M50 1.25v37.5M40 38.75h20" ] []
        , path [ d "M74.5 1.25h28l-28 37.5h28z" ] []
        , rect [ x "118.25", y "1.25", width "28", height "37.5", rx "10" ] []
        ]
