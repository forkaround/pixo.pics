module SupportView exposing
    ( layers
    , needsShaderF16
    , needsWebGpu
    , promptLayers
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
    | NoShaderF16Layer


layers :
    List (Html.Attribute msg)
    -> Pipeline.Device
    -> { checking : Html msg, supported : Html msg, unsupported : Html msg }
    -> Html msg
layers attributes device shown =
    stack attributes
        (imageLayer device)
        [ ( CheckingLayer, shown.checking )
        , ( SupportedLayer, shown.supported )
        , ( UnsupportedLayer, shown.unsupported )
        ]


promptLayers :
    List (Html.Attribute msg)
    -> Pipeline.Device
    -> { checking : Html msg, supported : Html msg, unsupported : Html msg, noShaderF16 : Html msg }
    -> Html msg
promptLayers attributes device shown =
    stack attributes
        (promptLayer device)
        [ ( CheckingLayer, shown.checking )
        , ( SupportedLayer, shown.supported )
        , ( UnsupportedLayer, shown.unsupported )
        , ( NoShaderF16Layer, shown.noShaderF16 )
        ]


stack : List (Html.Attribute msg) -> Layer -> List ( Layer, Html msg ) -> Html msg
stack attributes active shown =
    let
        layer ( which, content ) =
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
    div (class "grid" :: attributes) (List.map layer shown)


imageLayer : Pipeline.Device -> Layer
imageLayer device =
    case device of
        Pipeline.Unchecked ->
            CheckingLayer

        Pipeline.Checking ->
            CheckingLayer

        Pipeline.Unsupported _ ->
            UnsupportedLayer

        Pipeline.Supported _ _ ->
            SupportedLayer


promptLayer : Pipeline.Device -> Layer
promptLayer device =
    case device of
        Pipeline.Unchecked ->
            CheckingLayer

        Pipeline.Checking ->
            CheckingLayer

        Pipeline.Unsupported _ ->
            UnsupportedLayer

        Pipeline.Supported Pipeline.CanPrompt _ ->
            SupportedLayer

        Pipeline.Supported Pipeline.NoShaderF16 _ ->
            NoShaderF16Layer


unsupportedTitle : String
unsupportedTitle =
    "Cannot generate images on this device"


unsupportedReason : String
unsupportedReason =
    "Pixo needs a WebGPU device"


needsWebGpu : String
needsWebGpu =
    "Needs a WebGPU device to run."


needsShaderF16 : String
needsShaderF16 =
    "Needs 16-bit GPU shaders."
