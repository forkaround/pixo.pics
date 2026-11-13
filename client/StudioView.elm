module StudioView exposing (canvas, controls, lightbox, loading, message)

import Data.Dimension as Dimension exposing (Dimension)
import Data.Prompt as Prompt
import Data.Seed as Seed
import Data.Steps as Steps
import Html exposing (Html, button, div, figcaption, figure, form, img, p, span, text)
import Html.Attributes exposing (alt, attribute, checked, class, disabled, download, for, height, href, id, name, placeholder, readonly, src, style, title, type_, value, width)
import Html.Events exposing (onCheck, onClick, onInput, onSubmit)
import Library
import Lightbox
import Overlay exposing (glass, solid)
import Path
import Phosphor exposing (IconWeight(..))
import Pipeline exposing (Component(..), Engine(..), Message(..), Model, Msg(..), Seeding(..), Stage(..))
import Stylist
import UI.Button as Button
import UI.DropdownMenu as DropdownMenu
import UI.Empty as Empty
import UI.Field as Field
import UI.Input as Input
import UI.Progress as Progress
import UI.Select as Select
import UI.Skeleton as Skeleton
import UI.Slider as Slider
import UI.Switch as Switch
import UI.Textarea as Textarea



-- CONTROLS


controls : { pipeline : Msg -> msg, stylist : Stylist.Msg -> msg } -> Stylist.Model -> Model -> Html msg
controls toMsg stylist model =
    let
        settings =
            model.settings
    in
    form [ onSubmit (toMsg.pipeline GenerateClicked), class "flex min-h-full flex-col" ]
        [ Html.fieldset [ disabled (not (Pipeline.hasWebGpu model.engine)), class "flex flex-1 flex-col" ]
            [ Field.group [ class "flex-1 p-4" ]
                (Html.map toMsg.pipeline (viewWeights model)
                    :: viewPrompt toMsg stylist settings.prompt
                    :: List.map (Html.map toMsg.pipeline) (settingsFields settings)
                )
            ]
        , Html.map toMsg.pipeline
            (div [ class "sticky bottom-0 z-10 flex flex-col gap-3 border-t bg-background px-4 py-5" ]
                [ message model.message
                , viewGenerate model
                ]
            )
        ]


settingsFields : Pipeline.Settings -> List (Html Msg)
settingsFields settings =
    [ viewPresets settings
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


viewPrompt : { pipeline : Msg -> msg, stylist : Stylist.Msg -> msg } -> Stylist.Model -> String -> Html msg
viewPrompt toMsg stylist prompt =
    let
        busy =
            Stylist.isBusy stylist
    in
    Field.view []
        [ Field.label [ for "prompt" ] [ text "Prompt" ]
        , Textarea.view
            [ id "prompt"
            , name "prompt"
            , placeholder "A bonsai tree on a misty mountain top"
            , value prompt
            , onInput (PromptChanged >> toMsg.pipeline)
            , readonly busy
            , attribute "aria-busy"
                (if busy then
                    "true"

                 else
                    "false"
                )
            , class "min-h-28"
            ]
        , Html.map toMsg.stylist (viewStyle stylist prompt)
        , message stylist.message
        ]


viewStyle : Stylist.Model -> String -> Html Stylist.Msg
viewStyle stylist prompt =
    case stylist.state of
        Stylist.Loading progress (Just _) ->
            viewWriting
                [ Progress.new (100 * progress)
                    |> Progress.label [ text "Loading the prompt model" ]
                    |> Progress.formatValue (\done -> String.fromInt (floor done) ++ "%")
                    |> Progress.toHtml [ class "min-w-0 flex-1" ]
                ]

        Stylist.Writing job _ ->
            viewWriting [ p [ class "flex-1 text-sm text-muted-foreground", attribute "role" "status" ] [ text ("Writing a " ++ Stylist.styleName job.style ++ " prompt…") ] ]

        Stylist.Stopping _ ->
            viewWriting [ p [ class "flex-1 text-sm text-muted-foreground", attribute "role" "status" ] [ text "Stopping…" ] ]

        state ->
            div [ class "flex flex-wrap items-center gap-2" ]
                (viewStyleMenu (state == Stylist.Removing) prompt stylist
                    :: (if Stylist.canUndo prompt stylist then
                            [ Button.new
                                |> Button.text "Undo"
                                |> Button.ghost
                                |> Button.small
                                |> Button.startIcon (Phosphor.arrowCounterClockwise Regular |> Phosphor.toHtml [ attribute "aria-hidden" "true" ])
                                |> Button.toHtml [ onClick Stylist.UndoClicked ]
                            ]

                        else
                            []
                       )
                )


viewWriting : List (Html Stylist.Msg) -> Html Stylist.Msg
viewWriting shown =
    div [ class "flex min-h-8 items-center gap-3" ]
        (shown
            ++ [ Button.new
                    |> Button.text "Stop"
                    |> Button.outline
                    |> Button.small
                    |> Button.toHtml [ onClick Stylist.StopClicked, class "shrink-0" ]
               ]
        )


viewStyleMenu : Bool -> String -> Stylist.Model -> Html Stylist.Msg
viewStyleMenu removing prompt stylist =
    DropdownMenu.new
        |> DropdownMenu.id "ai-style"
        |> DropdownMenu.trigger
            ((Button.new |> Button.text "AI Style" |> Button.outline |> Button.small |> Button.toAttributes)
                ++ [ disabled removing ]
            )
            [ Phosphor.magicWand Regular |> Phosphor.toHtml [ attribute "aria-hidden" "true" ]
            , text "AI Style"
            , Phosphor.caretDown Regular |> Phosphor.withClass "opacity-60" |> Phosphor.toHtml [ attribute "aria-hidden" "true" ]
            ]
        |> DropdownMenu.content
            (DropdownMenu.label [ text "Rewrite the prompt as" ]
                :: List.map
                    (\style -> DropdownMenu.item [ onClick (Stylist.StyleChosen style prompt) ] [ text (Stylist.styleName style) ])
                    Stylist.styles
                ++ (if stylist.downloaded then
                        []

                    else
                        [ DropdownMenu.separator
                        , DropdownMenu.label [ text ("Downloads " ++ Stylist.modelSize ++ " once") ]
                        ]
                   )
            )
        |> DropdownMenu.toHtml [ class "w-56" ]


viewWeights : Model -> Html Msg
viewWeights model =
    Field.view []
        [ Field.label [ for "weights" ] [ text "Model" ]
        , Select.new
            |> Select.id "weights"
            |> Select.onSelect WeightsChosen
            |> Select.value (Just model.settings.weights)
            |> Select.items (List.map (\known -> Select.item known [ text (Pipeline.weightsName known) ]) Pipeline.weights)
            |> Select.toHtml [ class "w-full" ]
        , Field.description [] [ Html.a [ href (Path.toString Path.Models) ] [ text "Manage models" ] ]
        ]


viewPresets : Pipeline.Settings -> Html Msg
viewPresets settings =
    Field.view []
        [ Field.title [] [ text "Size" ]
        , div []
            [ div
                [ class "-mx-4 flex snap-x scroll-px-4 gap-2 overflow-x-auto px-4 [scrollbar-width:none] [&::-webkit-scrollbar]:hidden"
                , attribute "role" "group"
                , attribute "aria-label" "Size"
                ]
                (List.map (viewPreset settings) Pipeline.presets)
            ]
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
        , class "flex shrink-0 snap-start items-center gap-2 rounded-md border border-border bg-muted px-2 py-1.5 text-start text-xs whitespace-nowrap outline-none transition-colors hover:bg-muted/60 disabled:pointer-events-none disabled:opacity-50 focus-visible:ring-2 focus-visible:ring-ring/50 focus-visible:ring-inset aria-pressed:border-ring aria-pressed:bg-background aria-pressed:hover:bg-background dark:bg-input/30 dark:hover:bg-input/50 dark:aria-pressed:bg-input dark:aria-pressed:hover:bg-input"
        ]
        [ span [ class "flex size-5 shrink-0 items-center justify-center", attribute "aria-hidden" "true" ]
            [ span
                [ class "rounded-[3px] border-[1.5px] border-current"
                , style "width" (side (across / down))
                , style "height" (side (down / across))
                ]
                []
            ]
        , span [ class "flex flex-col leading-tight" ]
            [ span [ class "font-medium" ] [ text preset.name ]
            , span [ class "text-[0.6875rem] text-muted-foreground tabular-nums" ] [ text preset.ratio ]
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
                , Field.description [] [ text "Same seed, same picture." ]
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
            div [ class "flex flex-col gap-4" ]
                [ loading chosen parts
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
            generateButton "Generate" [ disabled True ]

        Releasing (Just _) ->
            viewWaiting "Switching models…"

        Releasing Nothing ->
            viewWaiting "Releasing…"

        Clearing ->
            viewWaiting "Removing…"

        Unchecked ->
            generateButton "Generate" [ disabled True ]

        Checking ->
            generateButton "Generate" [ disabled True ]


viewStart : Model -> Html Msg
viewStart model =
    generateButton
        (if Pipeline.isDownloaded model.settings.weights model || Pipeline.inMemory model.engine == Just model.settings.weights then
            "Generate"

         else
            "Download and generate"
        )
        []


generateButton : String -> List (Html.Attribute Msg) -> Html Msg
generateButton label attributes =
    Button.new
        |> Button.text label
        |> Button.startIcon (Phosphor.sparkle Regular |> Phosphor.toHtml [ attribute "aria-hidden" "true" ])
        |> Button.toHtml (type_ "submit" :: attributes)


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
        "Finishing…"


message : Maybe Message -> Html msg
message shown =
    case shown of
        Just (Problem reason) ->
            Field.error [] [ text reason ]

        Just (Info note) ->
            p [ class "text-sm text-muted-foreground", attribute "role" "status" ] [ text note ]

        Nothing ->
            text ""


loading : Pipeline.Weights -> List Pipeline.Download -> Html msg
loading chosen parts =
    let
        part =
            Pipeline.current parts
    in
    Progress.new (Maybe.withDefault 0 (Maybe.map percentage part))
        |> Progress.label
            [ text
                (case part of
                    Just found ->
                        "Loading the " ++ String.toLower (componentName found.component)

                    Nothing ->
                        "Loading " ++ Pipeline.weightsName chosen ++ "…"
                )
            ]
        |> Progress.formatValue (\done -> String.fromInt (floor done) ++ "%")
        |> Progress.toHtml []


percentage : Pipeline.Download -> Float
percentage { loaded, total } =
    if total > 0 then
        100 * toFloat loaded / toFloat total

    else
        0


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
            List.map (viewPicture toMsg library) model.pictures
    in
    div [ class "mx-auto flex w-full max-w-3xl flex-1 flex-col items-center gap-10 p-4 md:p-8" ]
        (case ( model.engine, pictures ) of
            ( Unsupported, _ ) ->
                [ viewEmpty CannotGenerate ]

            ( Unchecked, _ ) ->
                [ viewEmpty Unknown ]

            ( Checking, _ ) ->
                [ viewEmpty Unknown ]

            ( Generating _ job stage, _ ) ->
                viewPending job stage :: pictures

            ( _, [] ) ->
                [ viewEmpty CanGenerate ]

            _ ->
                pictures
        )


type Capability
    = Unknown
    | CanGenerate
    | CannotGenerate


viewEmpty : Capability -> Html msg
viewEmpty capability =
    let
        layer shown =
            class
                (if shown == capability then
                    "col-start-1 row-start-1 self-start"

                 else
                    "invisible col-start-1 row-start-1 self-start"
                )
    in
    div [ class "grid w-full flex-1 content-center" ]
        [ Empty.new
            |> Empty.icon (Skeleton.view [ class "size-full rounded-lg" ])
            |> Empty.title [ Skeleton.view [ class "my-0.5 h-4 w-40" ] ]
            |> Empty.description [ Skeleton.view [ class "my-1 h-4 w-64" ] ]
            |> Empty.toHtml [ layer Unknown, attribute "aria-hidden" "true" ]
        , Empty.new
            |> Empty.icon (Phosphor.paintBrush Bold |> Phosphor.toHtml [ attribute "aria-hidden" "true" ])
            |> Empty.title [ text "Nothing here yet" ]
            |> Empty.description
                [ text "Describe a picture and press Generate. Save the ones you want to keep." ]
            |> Empty.toHtml [ layer CanGenerate ]
        , Empty.new
            |> Empty.icon (Phosphor.warning Bold |> Phosphor.toHtml [ attribute "aria-hidden" "true" ])
            |> Empty.title [ text "Cannot generate images on this device" ]
            |> Empty.description [ text "Pixo needs a WebGPU device" ]
            |> Empty.toHtml [ layer CannotGenerate ]
        ]


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


viewPicture : { pipeline : Msg -> msg, library : Library.Msg -> msg } -> Library.Model -> Pipeline.Picture -> Html msg
viewPicture toMsg library picture =
    let
        job =
            picture.job

        prompt =
            Prompt.toString job.prompt
    in
    figure (class "group/picture relative overflow-hidden rounded-xl shadow-sm ring-1 ring-foreground/10" :: frame job)
        [ button
            [ type_ "button"
            , onClick (toMsg.pipeline (Opened picture.url))
            , attribute "aria-label" ("Open " ++ prompt)
            , class "block size-full cursor-zoom-in outline-none"
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
            [ class "pointer-events-none absolute inset-0 flex flex-col justify-end gap-2 overflow-hidden bg-linear-to-t from-black/85 via-black/30 to-transparent p-4 text-white opacity-0 transition-opacity duration-200 group-focus-within/picture:opacity-100 group-hover/picture:opacity-100"
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
            , div [ class "pointer-events-auto flex flex-wrap items-center gap-1.5" ] (actions toMsg library picture)
            ]
        ]


actions : { pipeline : Msg -> msg, library : Library.Msg -> msg } -> Library.Model -> Pipeline.Picture -> List (Html msg)
actions toMsg library picture =
    [ viewSave (toMsg.library (Library.SaveClicked picture)) (Library.saveOf picture.url library)
    , Button.new
        |> Button.icon (Phosphor.downloadSimple Regular |> Phosphor.toHtml [ attribute "aria-hidden" "true" ])
        |> Button.label "Download"
        |> Button.small
        |> Button.toAnchor [ href picture.url, download (Library.fileName picture.job), glass ]
    , Button.new
        |> Button.icon (Phosphor.diceFive Regular |> Phosphor.toHtml [ attribute "aria-hidden" "true" ])
        |> Button.label "Use this seed"
        |> Button.small
        |> Button.toHtml [ onClick (toMsg.pipeline (SeedReused picture.job.seed)), glass ]
    ]


lightbox : { pipeline : Msg -> msg, library : Library.Msg -> msg } -> Library.Model -> Model -> Html msg
lightbox toMsg library model =
    Lightbox.view
        { id = "studio-picture"
        , open = model.zoomed
        , detailed = model.detailed
        , onClose = toMsg.pipeline Closed
        , onToggle = toMsg.pipeline DetailsToggled
        , onStep = toMsg.pipeline << Stepped
        }
        (model.viewing
            |> Maybe.andThen (\url -> List.head (List.filter (\picture -> picture.url == url) model.pictures))
            |> Maybe.map
                (\picture ->
                    { url = picture.url
                    , job = picture.job
                    , model = Pipeline.weightsName picture.weights
                    , pinned = False
                    , notice = text ""
                    , actions = actions toMsg library picture
                    }
                )
        )


viewSave : msg -> Library.Save -> Html msg
viewSave onSave save =
    case save of
        Library.Unsaved ->
            Button.new
                |> Button.text "Save"
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
                |> Button.text "Saved"
                |> Button.small
                |> Button.startIcon (Phosphor.check Regular |> Phosphor.toHtml [ attribute "aria-hidden" "true" ])
                |> Button.toAnchor [ href (Path.toString Path.Library), glass ]
