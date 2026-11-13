module SupportView exposing
    ( layers
    , needsWebGpu
    , unsupportedReason
    , unsupportedTitle
    )

import Html exposing (Html, div)
import Html.Attributes exposing (attribute, class)
import Pipeline


type Layer
    = CheckingLayer
    | SupportedLayer
    | UnsupportedLayer


layers :
    List (Html.Attribute msg)
    -> Pipeline.Device
    -> { checking : Html msg, supported : Html msg, unsupported : Html msg }
    -> Html msg
layers attributes device shown =
    let
        active =
            case device of
                Pipeline.Unchecked ->
                    CheckingLayer

                Pipeline.Checking ->
                    CheckingLayer

                Pipeline.Unsupported _ ->
                    UnsupportedLayer

                Pipeline.Supported _ ->
                    SupportedLayer

        layer which content =
            div
                (class "col-start-1 row-start-1 self-start"
                    :: (if which == active then
                            []

                        else
                            [ class "invisible", attribute "aria-hidden" "true" ]
                       )
                )
                [ content ]
    in
    div (class "grid" :: attributes)
        [ layer CheckingLayer shown.checking
        , layer SupportedLayer shown.supported
        , layer UnsupportedLayer shown.unsupported
        ]


unsupportedTitle : String
unsupportedTitle =
    "Cannot generate images on this device"


unsupportedReason : String
unsupportedReason =
    "Pixo needs a WebGPU device"


needsWebGpu : String
needsWebGpu =
    "Needs a WebGPU device to run."
