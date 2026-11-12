module StudioView exposing (canvas, controls, downloads, message)

import Data.Dimension as Dimension exposing (Dimension)
import Data.Prompt as Prompt
import Data.Seed as Seed
import Data.Steps as Steps
import Html exposing (Html, button, div, figcaption, figure, form, img, p, span, text)
import Html.Attributes exposing (alt, attribute, checked, class, disabled, download, for, height, href, id, name, placeholder, src, style, title, type_, value, width)
import Html.Events exposing (onCheck, onClick, onInput, onSubmit)
import Library
import Overlay exposing (glass, solid)
import Path
import Phosphor exposing (IconWeight(..))
import Pipeline exposing (Component(..), Engine(..), Message(..), Model, Msg(..), Seeding(..), Stage(..))
import UI.Button as Button
import UI.Empty as Empty
import UI.Field as Field
import UI.Input as Input
import UI.Progress as Progress
import UI.Select as Select
import UI.Slider as Slider
import UI.Switch as Switch
import UI.Textarea as Textarea



-- CONTROLS


controls : Model -> Html Msg
controls model =
    let
        settings =
            model.settings
    in
    form [ onSubmit GenerateClicked, class "flex min-h-full flex-col" ]
        [ Field.group [ class "flex-1 p-4" ]
            ([ viewWeights model
             , Field.view []
                [ Field.label [ for "prompt" ] [ text "Prompt" ]
                , Textarea.view
                    [ id "prompt"
                    , name "prompt"
                    , placeholder "A bonsai tree on a misty mountain top"
                    , value settings.prompt
                    , onInput PromptChanged
                    , class "min-h-28"
                    ]
                ]
             , viewPresets settings
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
             , Field.horizontal []
                [ Field.label [ for "random-seed" ] [ text "Random seed" ]
                , Switch.new
                    |> Switch.toHtml
                        [ id "random-seed"
                        , name "random-seed"
                        , checked (settings.seeding == RandomSeed)
                        , onCheck RandomSeedToggled
                        ]
                ]
             ]
                ++ viewSeed settings.seeding
            )
        , div [ class "sticky bottom-0 z-10 flex flex-col gap-3 border-t bg-sidebar p-4" ]
            [ message model.message
            , viewGenerate model
            ]
        ]


viewWeights : Model -> Html Msg
viewWeights model =
    let
        chosen =
            model.settings.weights
    in
    Field.view []
        [ Field.label [ for "weights" ] [ text "Model" ]
        , Select.new
            |> Select.id "weights"
            |> Select.onSelect WeightsChosen
            |> Select.value (Just chosen)
            |> Select.items (List.map (\known -> Select.item known [ text (Pipeline.weightsName known) ]) Pipeline.weights)
            |> Select.toHtml [ class "w-full" ]
        , Field.description []
            [ text
                (if Pipeline.inMemory model.engine == Just chosen then
                    "In memory, ready to make pictures. "

                 else if Pipeline.isDownloaded chosen model then
                    "Downloaded: it loads from this browser. "

                 else
                    "A download of about " ++ Pipeline.weightsSize chosen ++ ", from Hugging Face, the first time. "
                )
            , Html.a [ href (Path.toString Path.Models) ] [ text "Manage models" ]
            ]
        ]


viewPresets : Pipeline.Settings -> Html Msg
viewPresets settings =
    Field.view []
        [ Field.title [] [ text "Size" ]
        , div [ class "grid grid-cols-2 gap-2", attribute "role" "group", attribute "aria-label" "Size" ]
            (List.map (viewPreset settings) Pipeline.presets)
        ]


viewPreset : Pipeline.Settings -> Pipeline.Preset -> Html Msg
viewPreset settings preset =
    let
        across =
            toFloat (Dimension.toInt preset.width)

        down =
            toFloat (Dimension.toInt preset.height)

        side length =
            String.fromFloat (18 * min 1 length) ++ "px"
    in
    button
        [ type_ "button"
        , onClick (PresetChosen preset)
        , title (preset.use ++ ", " ++ size preset.width preset.height)
        , attribute "aria-pressed"
            (if settings.width == preset.width && settings.height == preset.height then
                "true"

             else
                "false"
            )
        , class "flex items-center gap-2 rounded-md border border-border bg-background px-2 py-1.5 text-start text-sm outline-none transition-colors hover:bg-accent hover:text-accent-foreground focus-visible:ring-2 focus-visible:ring-ring/50 aria-pressed:border-primary aria-pressed:bg-accent aria-pressed:text-accent-foreground dark:bg-input/30"
        ]
        [ span [ class "flex size-5 shrink-0 items-center justify-center", attribute "aria-hidden" "true" ]
            [ span
                [ class "rounded-[3px] border-[1.5px] border-current"
                , style "width" (side (across / down))
                , style "height" (side (down / across))
                ]
                []
            ]
        , span [ class "flex min-w-0 flex-col leading-tight" ]
            [ span [ class "truncate font-medium" ] [ text preset.name ]
            , span [ class "text-xs text-muted-foreground tabular-nums" ] [ text preset.ratio ]
            ]
        ]


size : Dimension -> Dimension -> String
size across down =
    String.fromInt (Dimension.toInt across) ++ " × " ++ String.fromInt (Dimension.toInt down)


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


viewGenerate : Model -> Html Msg
viewGenerate model =
    case model.engine of
        Empty ->
            viewStart model

        Ready _ ->
            viewStart model

        Loading chosen parts _ ->
            let
                loading =
                    Pipeline.current parts
            in
            div [ class "flex flex-col gap-4" ]
                [ Progress.new (Maybe.withDefault 0 (Maybe.map percentage loading))
                    |> Progress.label
                        [ text
                            (case loading of
                                Just part ->
                                    "Loading the " ++ String.toLower (componentName part.component)

                                Nothing ->
                                    "Loading " ++ Pipeline.weightsName chosen ++ "…"
                            )
                        ]
                    |> Progress.formatValue (\part -> String.fromInt (floor part) ++ "%")
                    |> Progress.toHtml []
                , Button.new
                    |> Button.text "Cancel"
                    |> Button.outline
                    |> Button.toHtml [ onClick CancelClicked ]
                ]

        Generating _ job (Denoising done) ->
            let
                steps =
                    Steps.toInt job.steps
            in
            div [ class "flex flex-col gap-4" ]
                [ Progress.new (100 * toFloat done / toFloat steps)
                    |> Progress.label [ text (stepLabel done steps) ]
                    |> Progress.toHtml []
                , Button.new
                    |> Button.text "Cancel"
                    |> Button.outline
                    |> Button.toHtml [ onClick CancelClicked ]
                ]

        Generating _ _ Stopping ->
            Button.new
                |> Button.text "Stopping…"
                |> Button.outline
                |> Button.toHtml [ disabled True ]

        Unsupported ->
            viewWaiting "This browser can't make pictures."

        Releasing (Just _) ->
            viewWaiting "Making room for the model…"

        Releasing Nothing ->
            viewWaiting "Releasing the model…"

        Clearing ->
            viewWaiting "Removing the downloaded models…"

        Unchecked ->
            viewWaiting ""

        Checking ->
            viewWaiting ""


viewStart : Model -> Html Msg
viewStart model =
    Button.new
        |> Button.text
            (if Pipeline.isDownloaded model.settings.weights model || Pipeline.inMemory model.engine == Just model.settings.weights then
                "Generate"

             else
                "Download and generate"
            )
        |> Button.startIcon (Phosphor.sparkle Regular |> Phosphor.toHtml [ attribute "aria-hidden" "true" ])
        |> Button.toHtml [ type_ "submit" ]


viewWaiting : String -> Html Msg
viewWaiting reason =
    div [ class "flex flex-col gap-2" ]
        [ Button.new
            |> Button.text "Generate"
            |> Button.toHtml [ type_ "submit", disabled True ]
        , p [ class "min-h-5 text-sm text-muted-foreground" ] [ text reason ]
        ]


stepLabel : Int -> Int -> String
stepLabel done steps =
    if done < steps then
        "Step " ++ String.fromInt (done + 1) ++ " of " ++ String.fromInt steps

    else
        "Finishing the picture…"


message : Maybe Message -> Html msg
message shown =
    case shown of
        Just (Problem reason) ->
            Field.error [] [ text reason ]

        Just (Info note) ->
            p [ class "text-sm text-muted-foreground", attribute "role" "status" ] [ text note ]

        Nothing ->
            text ""


downloads : List Pipeline.Download -> List (Html msg)
downloads parts =
    List.map
        (\component ->
            let
                found =
                    Pipeline.download component parts
            in
            Progress.new (Maybe.withDefault 0 (Maybe.map percentage found))
                |> Progress.label [ text (componentName component) ]
                |> Progress.formatValue (\_ -> Maybe.withDefault "Waiting" (Maybe.map amount found))
                |> Progress.toHtml []
        )
        Pipeline.components


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



-- CANVAS


canvas : { pipeline : Msg -> msg, library : Library.Msg -> msg } -> Library.Model -> Model -> Html msg
canvas toMsg library model =
    let
        pictures =
            List.map (viewPicture toMsg library model) model.pictures
    in
    div [ class "mx-auto flex w-full max-w-3xl flex-1 flex-col items-center gap-10 p-4 md:p-8" ]
        (case ( model.engine, pictures ) of
            ( Unsupported, _ ) ->
                [ Empty.new
                    |> Empty.icon (Phosphor.warning Regular |> Phosphor.toHtml [ attribute "aria-hidden" "true" ])
                    |> Empty.title [ text "This browser can't make pictures" ]
                    |> Empty.description
                        [ text "The model runs on your own graphics card, through WebGPU. Try a recent Chrome, Edge or Safari." ]
                    |> Empty.toHtml []
                ]

            ( Generating _ job stage, _ ) ->
                viewPending job stage :: pictures

            ( _, [] ) ->
                [ Empty.new
                    |> Empty.icon (Phosphor.paintBrush Regular |> Phosphor.toHtml [ attribute "aria-hidden" "true" ])
                    |> Empty.title [ text "Nothing here yet" ]
                    |> Empty.description
                        [ text "Describe a picture, pick its size and press Generate. It is made in this browser, and stays here until you save it to the library." ]
                    |> Empty.toHtml []
                ]

            _ ->
                pictures
        )


viewPending : Pipeline.Job -> Stage -> Html msg
viewPending job stage =
    div [ class "flex w-full flex-col items-center gap-3", attribute "role" "status" ]
        [ div (class "animate-pulse rounded-xl bg-muted" :: frame job) []
        , p [ class "text-sm text-muted-foreground" ]
            [ text
                (case stage of
                    Denoising done ->
                        stepLabel done (Steps.toInt job.steps)

                    Stopping ->
                        "Stopping…"
                )
            ]
        ]


frame : Pipeline.Job -> List (Html.Attribute msg)
frame job =
    let
        across =
            String.fromInt (Dimension.toInt job.width)

        down =
            String.fromInt (Dimension.toInt job.height)
    in
    [ style "aspect-ratio" (across ++ " / " ++ down)
    , style "width" ("min(100%, " ++ across ++ "px, 70svh * " ++ across ++ " / " ++ down ++ ")")
    ]


viewPicture : { pipeline : Msg -> msg, library : Library.Msg -> msg } -> Library.Model -> Model -> Pipeline.Picture -> Html msg
viewPicture toMsg library model picture =
    let
        job =
            picture.job

        prompt =
            Prompt.toString job.prompt

        detailed =
            model.detailed == Just picture.url
    in
    figure (class "group/picture relative overflow-hidden rounded-xl shadow-sm ring-1 ring-foreground/10" :: frame job)
        [ button
            [ type_ "button"
            , onClick (toMsg.pipeline (DetailsToggled picture.url))
            , attribute "aria-label" "Show details"
            , attribute "aria-pressed"
                (if detailed then
                    "true"

                 else
                    "false"
                )
            , class "block size-full outline-none"
            ]
            [ img
                [ src picture.url
                , alt prompt
                , width (Dimension.toInt job.width)
                , height (Dimension.toInt job.height)
                , class "block size-full"
                ]
                []
            ]
        , figcaption
            [ class "pointer-events-none absolute inset-0 flex flex-col justify-end gap-2 overflow-hidden bg-linear-to-t from-black/85 via-black/30 to-transparent p-4 text-white transition-opacity duration-200 group-hover/picture:opacity-100 group-has-[:focus-visible]/picture:opacity-100"
            , class
                (if detailed then
                    "opacity-100"

                 else
                    "opacity-0"
                )
            ]
            [ p [ class "line-clamp-3 text-sm" ] [ text prompt ]
            , p [ class "text-xs text-white/70 tabular-nums" ]
                [ text
                    (String.join " · "
                        [ size job.width job.height
                        , String.fromInt (Steps.toInt job.steps) ++ " steps"
                        , "seed " ++ Seed.toString job.seed
                        , Pipeline.weightsName picture.weights
                        ]
                    )
                ]
            , div [ class "pointer-events-auto flex flex-wrap items-center gap-1.5" ]
                [ viewSave (toMsg.library (Library.SaveClicked picture)) (Library.saveOf picture.url library)
                , Button.new
                    |> Button.icon (Phosphor.downloadSimple Regular |> Phosphor.toHtml [ attribute "aria-hidden" "true" ])
                    |> Button.label "Download"
                    |> Button.small
                    |> Button.toAnchor [ href picture.url, download (Library.fileName job), glass ]
                , Button.new
                    |> Button.icon (Phosphor.diceFive Regular |> Phosphor.toHtml [ attribute "aria-hidden" "true" ])
                    |> Button.label "Use this seed"
                    |> Button.small
                    |> Button.toHtml [ onClick (toMsg.pipeline (SeedReused job.seed)), glass ]
                ]
            ]
        ]


viewSave : msg -> Library.Save -> Html msg
viewSave onSave save =
    case save of
        Library.Unsaved ->
            Button.new
                |> Button.text "Save to library"
                |> Button.small
                |> Button.startIcon (Phosphor.bookmarkSimple Regular |> Phosphor.toHtml [ attribute "aria-hidden" "true" ])
                |> Button.toHtml [ onClick onSave, solid ]

        Library.Saving ->
            Button.new
                |> Button.text "Saving…"
                |> Button.small
                |> Button.toHtml [ disabled True, solid ]

        Library.Saved ->
            Button.new
                |> Button.text "In the library"
                |> Button.small
                |> Button.startIcon (Phosphor.check Regular |> Phosphor.toHtml [ attribute "aria-hidden" "true" ])
                |> Button.toAnchor [ href (Path.toString Path.Library), glass ]
