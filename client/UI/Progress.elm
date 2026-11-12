module UI.Progress exposing
    ( Progress
    , formatValue
    , label
    , new
    , toHtml
    , value
    )

import Html exposing (Attribute, Html)
import Html.Attributes exposing (attribute, class)


type Progress msg
    = Progress
        { percentage : Float
        , label : Maybe (List (Html msg))
        , value : Maybe (Float -> String)
        }



-- CONSTRUCTOR


new : Float -> Progress msg
new percentage =
    Progress
        { percentage = clamp 0 100 percentage
        , label = Nothing
        , value = Nothing
        }



-- SETTERS


label : List (Html msg) -> Progress msg -> Progress msg
label children (Progress config) =
    Progress { config | label = Just children }


value : Progress msg -> Progress msg
value =
    formatValue (\percentage -> String.fromFloat percentage ++ "%")


formatValue : (Float -> String) -> Progress msg -> Progress msg
formatValue format (Progress config) =
    Progress { config | value = Just format }



-- RENDERING


toHtml : List (Attribute msg) -> Progress msg -> Html msg
toHtml attrs (Progress config) =
    let
        percentage =
            String.fromFloat config.percentage ++ "%"
    in
    Html.div
        ([ attribute "data-slot" "progress"
         , attribute "role" "progressbar"
         , attribute "aria-valuemin" "0"
         , attribute "aria-valuemax" "100"
         , attribute "aria-valuenow" (String.fromFloat config.percentage)
         , attribute "aria-valuetext" (Maybe.withDefault (\_ -> percentage) config.value config.percentage)
         , attribute "style" ("--progress-value: " ++ percentage)
         , class "flex flex-wrap gap-3"
         ]
            ++ attrs
        )
        (List.filterMap identity
            [ Maybe.map viewLabel config.label
            , Maybe.map (viewValue config.percentage) config.value
            , Just viewTrack
            ]
        )


viewLabel : List (Html msg) -> Html msg
viewLabel children =
    Html.span
        [ attribute "data-slot" "progress-label"
        , class "text-sm font-medium"
        ]
        children


viewValue : Float -> (Float -> String) -> Html msg
viewValue percentage format =
    Html.span
        [ attribute "data-slot" "progress-value"
        , class "ml-auto text-sm text-muted-foreground tabular-nums"
        ]
        [ Html.text (format percentage) ]


viewTrack : Html msg
viewTrack =
    Html.div
        [ attribute "data-slot" "progress-track"
        , class "relative flex h-1 w-full items-center overflow-x-hidden rounded-full bg-muted"
        ]
        [ Html.div
            [ attribute "data-slot" "progress-indicator"
            , class "h-full w-(--progress-value) bg-primary transition-all"
            ]
            []
        ]
