module PipelineView exposing (view)

import Data.Dimension as Dimension
import Data.Prompt as Prompt
import Data.Seed as Seed
import Data.Steps as Steps
import Html exposing (Html, div, form, img, p, span, text)
import Html.Attributes exposing (alt, attribute, checked, class, disabled, download, for, height, href, id, name, placeholder, selected, src, type_, value, width)
import Html.Events exposing (on, onCheck, onClick, onInput, onSubmit, targetValue)
import Json.Decode as Decode
import Phosphor exposing (IconWeight(..))
import Pipeline exposing (Component(..), Engine(..), Message(..), Model, Msg(..), Seeding(..), Stage(..))
import UI.Button as Button
import UI.Card as Card
import UI.Field as Field
import UI.Input as Input
import UI.NativeSelect as NativeSelect
import UI.Progress as Progress
import UI.Skeleton as Skeleton
import UI.Slider as Slider
import UI.Switch as Switch
import UI.Textarea as Textarea


view : Model -> List (Html Msg)
view model =
    case model.engine of
        Unchecked ->
            [ viewChecking ]

        Checking ->
            [ viewChecking ]

        Unsupported ->
            [ Card.new
                |> Card.title [ text "This browser can't make pictures" ]
                |> Card.description
                    [ text "The model runs on your own graphics card, through WebGPU. Try a recent Chrome, Edge or Safari." ]
                |> Card.toHtml [ class "w-full max-w-md" ]
            ]

        _ ->
            viewModel model :: viewSettings model :: List.map viewPicture model.pictures


viewChecking : Html Msg
viewChecking =
    Card.new
        |> Card.title [ text "Model" ]
        |> Card.content [ attribute "aria-hidden" "true" ]
            [ div [ class "flex flex-col gap-3" ]
                [ Skeleton.view [ class "h-4 w-56" ]
                , Skeleton.view [ class "h-8 w-full" ]
                ]
            ]
        |> Card.toHtml [ class "w-full max-w-md" ]


viewModel : Model -> Html Msg
viewModel model =
    Card.new
        |> Card.title [ text "Model" ]
        |> Card.description [ text (modelStatus model.engine) ]
        |> Card.content [] [ viewEngine model ]
        |> Card.toHtml [ class "w-full max-w-md" ]


modelStatus : Engine -> String
modelStatus engine =
    case engine of
        Loading chosen _ ->
            "Loading " ++ Pipeline.weightsName chosen ++ "…"

        Ready loaded ->
            Pipeline.weightsName loaded ++ " is in memory."

        Generating loaded _ _ ->
            Pipeline.weightsName loaded ++ " is in memory."

        Releasing ->
            "Releasing the model…"

        _ ->
            "It runs in this browser, on your graphics card. Your prompts stay here."


viewEngine : Model -> Html Msg
viewEngine model =
    case model.engine of
        Loading _ downloads ->
            div [ class "flex flex-col gap-4" ]
                (List.map (viewDownload downloads) Pipeline.components
                    ++ [ Button.new
                            |> Button.text "Cancel"
                            |> Button.outline
                            |> Button.toHtml [ onClick CancelClicked ]
                       ]
                )

        Ready _ ->
            viewRelease False

        Generating _ _ _ ->
            viewRelease True

        Releasing ->
            viewRelease True

        _ ->
            viewLoad (model.engine == Clearing) model


viewRelease : Bool -> Html Msg
viewRelease busy =
    Button.new
        |> Button.text "Release from memory"
        |> Button.outline
        |> Button.toHtml [ onClick ReleaseClicked, disabled busy, class "w-full" ]


viewLoad : Bool -> Model -> Html Msg
viewLoad clearing { settings, message } =
    Field.group []
        [ Field.view []
            [ Field.label [ for "weights" ] [ text "Weights" ]
            , NativeSelect.new
                |> NativeSelect.options
                    (List.map
                        (\known ->
                            NativeSelect.option
                                [ value (Pipeline.weightsId known), selected (known == settings.weights) ]
                                [ text (Pipeline.weightsName known) ]
                        )
                        Pipeline.weights
                    )
                |> NativeSelect.toHtml [ id "weights", name "weights", on "change" (Decode.map WeightsChosen targetValue) ]
            , Field.description []
                [ text ("A download of about " ++ Pipeline.weightsSize settings.weights ++ ", from Hugging Face, the first time.") ]
            ]
        , viewSwitch "keep" "Keep a copy in this browser" settings.keep KeepToggled
        , viewSwitch "fresh" "Download again" settings.fresh FreshToggled
        , viewMessage message
        , Button.new
            |> Button.text "Load model"
            |> Button.toHtml [ onClick LoadClicked, disabled clearing ]
        , Button.new
            |> Button.text
                (if clearing then
                    "Removing…"

                 else
                    "Remove downloaded models"
                )
            |> Button.ghost
            |> Button.toHtml [ onClick ClearClicked, disabled clearing ]
        ]


viewSwitch : String -> String -> Bool -> (Bool -> Msg) -> Html Msg
viewSwitch id_ label on toMsg =
    Field.horizontal []
        [ Field.label [ for id_ ] [ text label ]
        , Switch.new
            |> Switch.toHtml [ id id_, name id_, checked on, onCheck toMsg ]
        ]


viewDownload : List Pipeline.Download -> Component -> Html Msg
viewDownload downloads component =
    let
        found =
            Pipeline.download component downloads
    in
    Progress.new (Maybe.withDefault 0 (Maybe.map percentage found))
        |> Progress.label [ text (componentName component) ]
        |> Progress.formatValue (\_ -> Maybe.withDefault "Waiting" (Maybe.map amount found))
        |> Progress.toHtml []


percentage : Pipeline.Download -> Float
percentage { loaded, total } =
    if total > 0 then
        100 * toFloat loaded / toFloat total

    else
        0


amount : Pipeline.Download -> String
amount { loaded, total, cached } =
    megabytes loaded
        ++ " of "
        ++ megabytes total
        ++ " MB"
        ++ (if cached then
                ", from this browser"

            else
                ""
           )


megabytes : Int -> String
megabytes bytes =
    String.fromInt (round (toFloat bytes / 1000000))


componentName : Component -> String
componentName component =
    case component of
        TextEncoder ->
            "Text encoder"

        Transformer ->
            "Transformer"

        Vae ->
            "Decoder"


viewSettings : Model -> Html Msg
viewSettings { engine, settings, message } =
    Card.new
        |> Card.title [ text "Picture" ]
        |> Card.description [ text "Describe it, then pick its size and how long to work on it." ]
        |> Card.content []
            [ form [ onSubmit GenerateClicked ]
                [ Field.group []
                    ([ Field.view []
                        [ Field.label [ for "prompt" ] [ text "Prompt" ]
                        , Textarea.view
                            [ id "prompt"
                            , name "prompt"
                            , placeholder "A bonsai tree on a misty mountain top"
                            , value settings.prompt
                            , onInput PromptChanged
                            ]
                        ]
                     , viewSlider "Width"
                        (String.fromInt (Dimension.toInt settings.width) ++ " px")
                        { value = Dimension.toInt settings.width
                        , min = Dimension.smallest
                        , max = Dimension.largest
                        , step = Dimension.step
                        }
                        WidthChanged
                     , viewSlider "Height"
                        (String.fromInt (Dimension.toInt settings.height) ++ " px")
                        { value = Dimension.toInt settings.height
                        , min = Dimension.smallest
                        , max = Dimension.largest
                        , step = Dimension.step
                        }
                        HeightChanged
                     , viewSlider "Steps"
                        (String.fromInt (Steps.toInt settings.steps))
                        { value = Steps.toInt settings.steps
                        , min = Steps.fewest
                        , max = Steps.most
                        , step = 1
                        }
                        StepsChanged
                     , viewSwitch "random-seed" "Random seed" (settings.seeding == RandomSeed) RandomSeedToggled
                     ]
                        ++ viewSeed settings.seeding
                        ++ [ case engine of
                                Ready _ ->
                                    viewMessage message

                                _ ->
                                    text ""
                           , viewGenerate engine
                           ]
                    )
                ]
            ]
        |> Card.toHtml [ class "w-full max-w-md" ]


viewSlider : String -> String -> { value : Int, min : Int, max : Int, step : Int } -> (Float -> Msg) -> Html Msg
viewSlider label shown scale toMsg =
    Field.view []
        [ Field.label [ class "w-full flex-col items-stretch gap-3" ]
            [ span [ class "flex items-baseline justify-between" ]
                [ text label
                , span [ class "font-normal text-muted-foreground tabular-nums" ] [ text shown ]
                ]
            , Slider.new
                |> Slider.value (toFloat scale.value)
                |> Slider.onChange toMsg
                |> Slider.min (toFloat scale.min)
                |> Slider.max (toFloat scale.max)
                |> Slider.step (toFloat scale.step)
                |> Slider.toHtml []
            ]
        ]


viewSeed : Seeding -> List (Html Msg)
viewSeed seeding =
    case seeding of
        RandomSeed ->
            []

        FixedSeed seed ->
            [ Field.view []
                [ Field.label [ for "seed" ] [ text "Seed" ]
                , Input.view
                    [ id "seed"
                    , name "seed"
                    , attribute "inputmode" "numeric"
                    , value seed
                    , onInput SeedChanged
                    ]
                , Field.description [] [ text "The same seed and prompt make the same picture." ]
                ]
            ]


viewGenerate : Engine -> Html Msg
viewGenerate engine =
    case engine of
        Ready _ ->
            Button.new
                |> Button.text "Generate"
                |> Button.toHtml [ type_ "submit" ]

        Generating _ job (Denoising done) ->
            let
                steps =
                    Steps.toInt job.steps
            in
            div [ class "flex flex-col gap-4" ]
                [ Progress.new (100 * toFloat done / toFloat steps)
                    |> Progress.label
                        [ text
                            (if done < steps then
                                "Step " ++ String.fromInt (done + 1) ++ " of " ++ String.fromInt steps

                             else
                                "Finishing the picture…"
                            )
                        ]
                    |> Progress.toHtml []
                , Button.new
                    |> Button.text "Cancel"
                    |> Button.outline
                    |> Button.toHtml [ type_ "button", onClick CancelClicked ]
                ]

        Generating _ _ Stopping ->
            Button.new
                |> Button.text "Stopping…"
                |> Button.outline
                |> Button.toHtml [ type_ "button", disabled True ]

        _ ->
            div [ class "flex flex-col gap-2" ]
                [ Button.new
                    |> Button.text "Generate"
                    |> Button.toHtml [ type_ "submit", disabled True ]
                , p [ class "text-sm text-muted-foreground" ] [ text "Load the model first." ]
                ]


viewMessage : Maybe Message -> Html Msg
viewMessage message =
    case message of
        Just (Problem reason) ->
            Field.error [] [ text reason ]

        Just (Info note) ->
            p [ class "text-sm text-muted-foreground", attribute "role" "status" ] [ text note ]

        Nothing ->
            text ""


viewPicture : Pipeline.Picture -> Html Msg
viewPicture { url, job } =
    let
        prompt =
            Prompt.toString job.prompt

        seed =
            Seed.toString job.seed
    in
    Card.new
        |> Card.content []
            [ div [ class "flex flex-col gap-3" ]
                [ img
                    [ src url
                    , alt prompt
                    , width (Dimension.toInt job.width)
                    , height (Dimension.toInt job.height)
                    , class "h-auto w-full rounded-lg"
                    ]
                    []
                , p [ class "text-sm" ] [ text prompt ]
                , p [ class "text-sm text-muted-foreground tabular-nums" ]
                    [ text
                        (String.fromInt (Dimension.toInt job.width)
                            ++ " × "
                            ++ String.fromInt (Dimension.toInt job.height)
                            ++ " · "
                            ++ String.fromInt (Steps.toInt job.steps)
                            ++ " steps · seed "
                            ++ seed
                        )
                    ]
                ]
            ]
        |> Card.footer [ class "gap-2" ]
            [ Button.new
                |> Button.text "Download"
                |> Button.outline
                |> Button.small
                |> Button.startIcon (Phosphor.downloadSimple Regular |> Phosphor.toHtml [ attribute "aria-hidden" "true" ])
                |> Button.toAnchor [ href url, download ("pixo-" ++ seed ++ ".png") ]
            , Button.new
                |> Button.text "Use this seed"
                |> Button.ghost
                |> Button.small
                |> Button.toHtml [ onClick (SeedReused job.seed) ]
            ]
        |> Card.toHtml [ class "w-full max-w-md" ]
