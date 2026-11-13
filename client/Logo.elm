module Logo exposing
    ( Hourglass
    , sidebar
    , wordmark
    )

import Html.Attributes exposing (attribute)
import Svg exposing (Attribute, Svg, defs, g, mask, path, rect, svg)
import Svg.Attributes exposing (class, d, fill, height, id, maskUnits, overflow, stroke, strokeWidth, style, viewBox, width, x, y)
import Svg.Keyed


wordmark : List (Attribute msg) -> Svg msg
wordmark attrs =
    view attrs [ path [ d expanded ] [], path [ d glass ] [] ]


type alias Hourglass =
    { running : Bool
    , run : Int
    , turn : Int
    }


sidebar : Float -> Hourglass -> List (Attribute msg) -> Svg msg
sidebar progress hourglass attrs =
    view (overflow "visible" :: attrs)
        [ g
            [ class "logo-letters"
            , class
                (if hourglass.running then
                    "logo-letters-yield"

                 else
                    ""
                )
            ]
            [ path [ d (interpolate progress), class "hidden md:motion-safe:block" ] []
            , path [ d expanded, class "md:hidden md:motion-reduce:group-data-[state=expanded]:block" ] []
            , path [ d collapsed, class "hidden md:motion-reduce:group-data-[state=collapsed]:block" ] []
            ]
        , g [ class "logo-glass" ]
            [ g
                [ class "logo-glass-turn"
                , style ("transform: rotate(" ++ String.fromInt (hourglass.turn * 180) ++ "deg)")
                ]
                [ defs []
                    [ bulb "logo-sand-top" "M 74.5 1.25 L 102.5 1.25 L 88.5 20 Z"
                    , bulb "logo-sand-bottom" "M 74.5 38.75 L 102.5 38.75 L 88.5 20 Z"
                    ]
                , Svg.Keyed.node "g"
                    [ class "logo-sand fill-current motion-reduce:hidden"
                    , stroke "none"
                    , class
                        (if hourglass.running then
                            "logo-sand-running"

                         else
                            ""
                        )
                    ]
                    [ ( String.fromInt hourglass.run ++ "-" ++ String.fromInt hourglass.turn
                      , sand (modBy 2 hourglass.turn == 0)
                      )
                    ]
                , path [ d glass ] []
                ]
            ]
        ]


bulb : String -> String -> Svg msg
bulb name outline =
    mask [ id name, maskUnits "userSpaceOnUse", x "70", y "-5", width "37", height "50" ]
        [ path [ d outline, fill "white", stroke "none" ] [] ]


sand : Bool -> Svg msg
sand upright =
    let
        top attrs =
            g [ Svg.Attributes.mask "url(#logo-sand-top)" ]
                [ rect ([ x "70", y "2.75", width "37", height "13.07" ] ++ attrs) [] ]

        bottom attrs =
            g [ Svg.Attributes.mask "url(#logo-sand-bottom)" ]
                [ rect ([ x "70", y "24.18", width "37", height "13.07" ] ++ attrs) [] ]

        draining =
            class "logo-sand-drain"

        filling =
            class "logo-sand-fill"
    in
    g []
        (if upright then
            [ top [ draining, style "transform-origin: 88.5px 15.82px" ]
            , bottom [ filling, style "transform-origin: 88.5px 37.25px" ]
            ]

         else
            [ bottom [ draining, style "transform-origin: 88.5px 24.18px" ]
            , top [ filling, style "transform-origin: 88.5px 2.75px" ]
            ]
        )


view : List (Attribute msg) -> List (Svg msg) -> Svg msg
view attrs children =
    svg
        ([ viewBox "-1.25 -1.25 150 42.5"
         , fill "none"
         , stroke "currentColor"
         , strokeWidth "5"
         , attribute "aria-hidden" "true"
         ]
            ++ attrs
        )
        children


interpolate : Float -> String
interpolate progress =
    List.map2
        (\from to ->
            case ( String.toFloat from, String.toFloat to ) of
                ( Just start, Just end ) ->
                    String.fromFloat (start + (end - start) * progress)

                _ ->
                    from
        )
        (String.words expanded)
        (String.words collapsed)
        |> String.join " "


glass : String
glass =
    "M 74.5 1.25 L 102.5 1.25 L 74.5 38.75 L 102.5 38.75 Z"


expanded : String
expanded =
    "M 1.25 41.25 L 1.25 1.25 L 17 1.25 C 22.2467 1.25 26.5 5.5033 26.5 10.75 C 26.5 15.9967 22.2467 20.25 17 20.25 L 1.25 20.25 M 40 1.25 L 60 1.25 M 50 1.25 L 50 38.75 M 40 38.75 L 60 38.75 M 128.25 1.25 L 136.25 1.25 C 141.7728 1.25 146.25 5.7272 146.25 11.25 L 146.25 28.75 C 146.25 34.2728 141.7728 38.75 136.25 38.75 L 128.25 38.75 C 122.7272 38.75 118.25 34.2728 118.25 28.75 L 118.25 11.25 C 118.25 5.7272 122.7272 1.25 128.25 1.25 Z"


collapsed : String
collapsed =
    "M 6 38.75 L 34 1.25 L 6 1.25 C 6 1.25 6 1.25 6 1.25 C 6 1.25 6 1.25 6 1.25 L 34 38.75 M 6 1.25 L 34 1.25 M 6 1.25 L 34 38.75 M 6 38.75 L 34 38.75 M 6 1.25 L 34 1.25 C 34 1.25 34 1.25 34 1.25 L 6 38.75 C 6 38.75 6 38.75 6 38.75 L 34 38.75 C 34 38.75 34 38.75 34 38.75 L 6 1.25 C 6 1.25 6 1.25 6 1.25 Z"
