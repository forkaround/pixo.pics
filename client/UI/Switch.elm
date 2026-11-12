module UI.Switch exposing
    ( Switch
    , new
    , small
    , toHtml
    )

import Html exposing (Attribute, Html)
import Html.Attributes exposing (attribute, class, type_)


type Switch
    = Switch { size : Size }


type Size
    = Medium
    | Small



-- CONSTRUCTOR


new : Switch
new =
    Switch { size = Medium }



-- SIZES


small : Switch -> Switch
small (Switch config) =
    Switch { config | size = Small }



-- RENDERING


toHtml : List (Attribute msg) -> Switch -> Html msg
toHtml attrs (Switch config) =
    Html.span
        [ attribute "data-size" (sizeName config.size)
        , class "relative inline-grid shrink-0 items-center *:col-start-1 *:row-start-1"
        ]
        [ Html.input
            ([ type_ "checkbox"
             , attribute "role" "switch"
             , attribute "data-slot" "switch"
             , class "peer inline-flex shrink-0 appearance-none rounded-full border border-transparent bg-input transition-all outline-none checked:bg-primary focus-visible:border-ring focus-visible:ring-3 focus-visible:ring-ring/50 disabled:cursor-not-allowed disabled:opacity-50 aria-invalid:border-destructive aria-invalid:ring-3 aria-invalid:ring-destructive/20 dark:bg-input/80 dark:checked:bg-primary dark:aria-invalid:border-destructive/50 dark:aria-invalid:ring-destructive/40"
             , class (trackClass config.size)
             ]
                ++ attrs
            )
            []
        , Html.span
            [ attribute "data-slot" "switch-thumb"
            , class "pointer-events-none block translate-x-px rounded-full bg-background ring-0 transition-transform peer-checked:translate-x-[calc(100%-1px)] peer-disabled:opacity-50 dark:bg-foreground dark:peer-checked:bg-primary-foreground"
            , class (thumbClass config.size)
            ]
            []
        ]


sizeName : Size -> String
sizeName size =
    case size of
        Medium ->
            "default"

        Small ->
            "sm"


trackClass : Size -> String
trackClass size =
    case size of
        Medium ->
            "h-[1.15rem] w-8"

        Small ->
            "h-3.5 w-6"


thumbClass : Size -> String
thumbClass size =
    case size of
        Medium ->
            "size-4"

        Small ->
            "size-3"
