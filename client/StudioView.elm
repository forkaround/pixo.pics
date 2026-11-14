module StudioView exposing (canvas, controls, lightbox, loading, message)

import Data.Dimension as Dimension exposing (Dimension)
import Data.Prompt as Prompt
import Data.Seed as Seed
import Data.Steps as Steps
import Html exposing (Html, button, div, figcaption, figure, form, img, label, p, span, text)
import Html.Attributes exposing (alt, attribute, class, disabled, download, for, height, href, id, name, novalidate, placeholder, readonly, src, style, title, type_, value, width)
import Html.Events exposing (onClick, onInput, onSubmit)
import Html.Keyed as Keyed
import Library
import Lightbox
import Overlay exposing (glass, solid)
import Path
import Phosphor exposing (IconWeight(..))
import Pipeline exposing (Component(..), Device(..), Draft(..), Engine(..), Message(..), Model, Msg(..), Seeding(..), Stage(..))
import Stylist
import SupportView
import UI.Button as Button
import UI.Card as Card
import UI.DropdownMenu as DropdownMenu
import UI.Empty as Empty
import UI.Field as Field
import UI.InputGroup as InputGroup
import UI.Progress as Progress
import UI.Select as Select
import UI.Textarea as Textarea



-- CONTROLS


controls : { pipeline : Msg -> msg, stylist : Stylist.Msg -> msg } -> Stylist.Model -> Model -> Html msg
controls toMsg stylist model =
    let
        checks =
            Stylist.checks model.settings.prompt stylist ++ Pipeline.checks model
    in
    div [ class "sticky bottom-0 z-10 px-1.5 pb-[max(--spacing(1.5),env(safe-area-inset-bottom))] md:px-8" ]
        [ form [ onSubmit (toMsg.pipeline GenerateClicked), novalidate True, class "mx-auto w-full md:max-w-2xl" ]
            [ Html.fieldset [ disabled (not (Pipeline.isSupported model.device)), class "min-w-0" ]
                [ Card.new
                    |> Card.small
                    |> Card.content [ class "flex flex-col gap-2" ]
                        (viewPrompt toMsg checks stylist model
                            ++ [ notices (List.map fromCheck checks ++ fromMessage stylist ++ fromMessage model) ]
                        )
                    |> Card.footer [ class "min-h-13 flex-wrap gap-2" ] (viewFooter toMsg checks stylist model)
                    |> Card.toHtml [ class "shadow-lg [&_:not([popover],[popover]_*)]:text-[length:inherit]! [&_:not([popover],[popover]_*)]:leading-[inherit]! has-[#prompt:focus-visible]:ring-ring/50 has-[#prompt:focus-visible]:ring-2" ]
                ]
            ]
        ]


invalid : List Pipeline.Validation -> String -> Html.Attribute msg
invalid checks field =
    attribute "aria-invalid"
        (if List.any (\check -> check.field == field && check.failing) checks then
            "true"

         else
            "false"
        )


viewPrompt : { pipeline : Msg -> msg, stylist : Stylist.Msg -> msg } -> List Pipeline.Validation -> Stylist.Model -> Model -> List (Html msg)
viewPrompt toMsg checks stylist model =
    let
        settings =
            model.settings

        ( shown, styling ) =
            case settings.prompt of
                Editing prompt ->
                    ( prompt, False )

                Styling ->
                    ( Maybe.withDefault "" (Stylist.shown stylist), True )
    in
    [ Field.label [ for "prompt", class "sr-only" ] [ text "Prompt" ]
    , Textarea.view
        [ id "prompt"
        , name "prompt"
        , placeholder "A bonsai tree on a misty mountain top"
        , value shown
        , onInput (PromptChanged >> toMsg.pipeline)
        , disabled (isLocked model)
        , invalid checks "prompt"
        , attribute "aria-busy"
            (if styling then
                "true"

             else
                "false"
            )
        , class "max-h-[calc(2lh+--spacing(2))] min-h-[calc(2lh+--spacing(2))]! resize-none overflow-y-auto rounded-none! border-0! bg-transparent! px-0! py-1! transition-[max-height]! duration-300 ease-out focus:max-h-[30svh] focus-visible:ring-0! aria-invalid:ring-0! aria-busy:max-h-[30svh] motion-reduce:transition-none! dark:bg-transparent!"
        ]
    , div [ class "flex min-h-8 flex-wrap items-center gap-2" ]
        (List.map (Html.map toMsg.stylist) (viewStyle (isLocked model) model.device stylist settings)
            ++ [ Html.map toMsg.pipeline (viewWeights model) ]
        )
    ]


isLocked : Model -> Bool
isLocked model =
    model.settings.prompt == Styling || isMakingPicture model.device


isMakingPicture : Device -> Bool
isMakingPicture device =
    case Pipeline.engineOf device of
        Just (Loading _ _ waiting) ->
            waiting /= Nothing

        Just (Generating _ _ _) ->
            True

        Just (Releasing (Just pending)) ->
            pending.job /= Nothing

        _ ->
            False


viewStyle : Bool -> Pipeline.Device -> Stylist.Model -> Pipeline.Settings -> List (Html Stylist.Msg)
viewStyle locked device stylist settings =
    let
        prompt =
            case settings.prompt of
                Editing typed ->
                    typed

                Styling ->
                    ""

        picture =
            { width = Dimension.toInt settings.width, height = Dimension.toInt settings.height }
    in
    case stylist.state of
        Stylist.Loading _ (Just _) ->
            [ viewStyleMenu True picture prompt stylist ]

        Stylist.Writing _ _ ->
            [ stop "Stop generating" ]

        Stylist.Stopping _ ->
            [ Button.new
                |> Button.text "Stopping…"
                |> Button.outline
                |> Button.small
                |> Button.toHtml [ disabled True, class "shrink-0" ]
            ]

        state ->
            viewStyleMenu (locked || state == Stylist.Removing || not (Pipeline.canPrompt device)) picture prompt stylist
                :: (if Pipeline.prompting device == Just Pipeline.NoShaderF16 then
                        [ p [ class "min-w-0 flex-1 basis-0 truncate text-xs text-muted-foreground", title SupportView.needsShaderF16 ]
                            [ text SupportView.needsShaderF16 ]
                        ]

                    else
                        []
                   )
                ++ (if Stylist.canUndo prompt stylist then
                        [ Button.new
                            |> Button.icon (Phosphor.arrowCounterClockwise Bold |> Phosphor.withClass "size-4" |> Phosphor.toHtml [ attribute "aria-hidden" "true" ])
                            |> Button.label "Undo"
                            |> Button.ghost
                            |> Button.small
                            |> Button.toHtml [ onClick Stylist.UndoClicked, title "Undo", disabled locked ]
                        ]

                    else
                        []
                   )


stop : String -> Html Stylist.Msg
stop label =
    Button.new
        |> Button.text label
        |> Button.outline
        |> Button.small
        |> Button.toHtml [ onClick Stylist.StopClicked, class "shrink-0" ]


viewStyleMenu : Bool -> Stylist.Frame -> String -> Stylist.Model -> Html Stylist.Msg
viewStyleMenu off picture prompt stylist =
    DropdownMenu.new
        |> DropdownMenu.id "ai-style"
        |> DropdownMenu.top
        |> DropdownMenu.trigger
            ((Button.new |> Button.text "AI Style" |> Button.outline |> Button.small |> Button.toAttributes)
                ++ [ disabled off ]
            )
            [ Phosphor.magicWand Bold |> Phosphor.withClass "size-4" |> Phosphor.toHtml [ attribute "aria-hidden" "true" ]
            , text "AI Style"
            , Phosphor.caretDown Regular |> Phosphor.withClass "opacity-60" |> Phosphor.toHtml [ attribute "aria-hidden" "true" ]
            ]
        |> DropdownMenu.content
            (DropdownMenu.label [ text "Rewrite the prompt as" ]
                :: List.map
                    (\style -> DropdownMenu.item [ onClick (Stylist.StyleChosen style picture prompt) ] [ text (Stylist.styleName style) ])
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
    span [ class "ms-auto flex" ]
        [ Field.label [ for "weights", class "sr-only" ] [ text "Model" ]
        , Select.new
            |> Select.id "weights"
            |> Select.onSelect WeightsChosen
            |> Select.value (Just model.settings.weights)
            |> Select.group "Bonsai" (List.map (\known -> Select.item known [ text (Pipeline.weightsVariant known) ]) Pipeline.weights)
            |> Select.small
            |> Select.toHtml [ disabled (isLocked model) ]
        ]


viewFooter : { pipeline : Msg -> msg, stylist : Stylist.Msg -> msg } -> List Pipeline.Validation -> Stylist.Model -> Model -> List (Html msg)
viewFooter toMsg checks stylist model =
    case ( viewWork model, viewPromptModel stylist ) of
        ( Nothing, Nothing ) ->
            List.map (Html.map toMsg.pipeline) (viewSettings (isLocked model) checks model.settings (viewGenerate model))

        ( work, promptModel ) ->
            List.filterMap identity
                [ Maybe.map (viewRow >> Html.map toMsg.pipeline) work
                , Maybe.map (viewRow >> Html.map toMsg.stylist) promptModel
                ]


viewRow : List (Html msg) -> Html msg
viewRow =
    div [ class "flex w-full min-w-0 flex-wrap items-center gap-2" ]


viewPromptModel : Stylist.Model -> Maybe (List (Html Stylist.Msg))
viewPromptModel stylist =
    case stylist.state of
        Stylist.Loading progress (Just _) ->
            Just
                [ Progress.new (100 * progress)
                    |> Progress.label
                        [ text
                            (if stylist.downloaded then
                                "Loading the prompt model"

                             else
                                "Downloading the prompt model"
                            )
                        ]
                    |> Progress.formatValue (\done -> String.fromInt (floor done) ++ "%")
                    |> Progress.toHtml [ class "min-w-0 flex-1 [&_[data-slot=progress-value]]:text-xs" ]
                , Button.new
                    |> Button.text "Cancel"
                    |> Button.outline
                    |> Button.small
                    |> Button.toHtml [ onClick Stylist.StopClicked, class "ms-auto" ]
                ]

        _ ->
            Nothing


viewGenerate : Model -> Html Msg
viewGenerate model =
    case Pipeline.engineOf model.device of
        Just Empty ->
            viewStart model

        Just (Ready _) ->
            viewStart model

        Just (Generating _ _ (Denoising _)) ->
            Button.new
                |> Button.text "Cancel"
                |> Button.outline
                |> Button.small
                |> Button.toHtml [ onClick CancelClicked, class "ms-auto max-md:w-full" ]

        Just (Generating _ _ Stopping) ->
            Button.new
                |> Button.text "Cancelling…"
                |> Button.outline
                |> Button.small
                |> Button.toHtml [ disabled True, class "ms-auto max-md:w-full" ]

        _ ->
            generateButton "Generate" [ disabled True ]


viewWork : Model -> Maybe (List (Html Msg))
viewWork model =
    case Pipeline.engineOf model.device of
        Just (Loading chosen parts _) ->
            Just
                [ loading chosen parts
                    |> List.singleton
                    |> div [ class "min-w-0 flex-1" ]
                , cancel
                ]

        Just (Cancelling chosen parts) ->
            Just
                [ loading chosen parts
                    |> List.singleton
                    |> div [ class "min-w-0 flex-1" ]
                , Button.new
                    |> Button.text "Cancelling…"
                    |> Button.outline
                    |> Button.small
                    |> Button.toHtml [ disabled True, class "ms-auto" ]
                ]

        Just (Generating _ _ _) ->
            Nothing

        Just (Releasing (Just _)) ->
            Just (viewWaiting "Switching models…")

        Just (Releasing Nothing) ->
            Just (viewWaiting "Releasing…")

        Just Clearing ->
            Just (viewWaiting "Removing…")

        Just Empty ->
            Nothing

        Just (Ready _) ->
            Nothing

        Nothing ->
            Nothing


viewSettings : Bool -> List Pipeline.Validation -> Pipeline.Settings -> Html Msg -> List (Html Msg)
viewSettings locked checks settings generate =
    [ div [ class "grid w-full grid-cols-[auto_minmax(0,1fr)] gap-2 md:flex md:flex-wrap md:items-center" ]
        [ viewSize locked settings
        , viewSeed locked (invalid checks "seed") settings.seeding
        , viewSteps locked (invalid checks "steps") settings
        , generate
        ]
    ]


viewSteps : Bool -> Html.Attribute Msg -> Pipeline.Settings -> Html Msg
viewSteps locked validity settings =
    InputGroup.view [ InputGroup.small, class "w-auto!" ]
        [ InputGroup.start [] [ label [ for "steps" ] [ text "Steps" ] ]
        , InputGroup.input
            [ id "steps"
            , name "steps"
            , attribute "inputmode" "numeric"
            , validity
            , value settings.steps
            , onInput StepsChanged
            , disabled locked
            , class "w-14! flex-none! text-center tabular-nums"
            ]
        ]


viewSize : Bool -> Pipeline.Settings -> Html Msg
viewSize locked settings =
    let
        chosen =
            List.filter (\preset -> preset.width == settings.width && preset.height == settings.height) Pipeline.presets
                |> List.head
    in
    DropdownMenu.new
        |> DropdownMenu.id "size"
        |> DropdownMenu.top
        |> DropdownMenu.trigger
            ((Button.new |> Button.text "Size" |> Button.outline |> Button.small |> Button.toAttributes)
                ++ [ attribute "aria-label"
                        (case chosen of
                            Just preset ->
                                "Size: " ++ preset.name ++ ", " ++ preset.ratio

                            Nothing ->
                                "Size: " ++ size settings.width settings.height
                        )
                   , class "tabular-nums"
                   , disabled locked
                   ]
            )
            [ shapeIcon settings.width settings.height
            , text (Maybe.withDefault (size settings.width settings.height) (Maybe.map .ratio chosen))
            , Phosphor.caretDown Regular |> Phosphor.withClass "opacity-60" |> Phosphor.toHtml [ attribute "aria-hidden" "true" ]
            ]
        |> DropdownMenu.content
            (DropdownMenu.label [ text "Size" ]
                :: List.map
                    (\preset ->
                        DropdownMenu.radioItem (chosen == Just preset)
                            [ onClick (PresetChosen preset)
                            , title (preset.use ++ ", " ++ size preset.width preset.height)
                            , attribute "popovertarget" "size-menu"
                            , attribute "popovertargetaction" "hide"
                            ]
                            [ shapeIcon preset.width preset.height
                            , text preset.name
                            , DropdownMenu.shortcut [ span [ class "tabular-nums" ] [ text preset.ratio ] ]
                            ]
                    )
                    Pipeline.presets
            )
        |> DropdownMenu.toHtml [ class "w-56" ]


shapeIcon : Dimension -> Dimension -> Html msg
shapeIcon across down =
    let
        wide =
            toFloat (Dimension.toInt across)

        tall =
            toFloat (Dimension.toInt down)

        side length =
            String.fromFloat (14 * min 1 length) ++ "px"
    in
    span [ class "flex size-4 shrink-0 items-center justify-center", attribute "aria-hidden" "true" ]
        [ span
            [ class "rounded-[2px] border-[1.5px] border-current"
            , style "width" (side (wide / tall))
            , style "height" (side (tall / wide))
            ]
            []
        ]


size : Dimension -> Dimension -> String
size across down =
    String.fromInt (Dimension.toInt across) ++ " × " ++ String.fromInt (Dimension.toInt down)


viewSeed : Bool -> Html.Attribute Msg -> Seeding -> Html Msg
viewSeed locked validity seeding =
    let
        ( typed, random ) =
            case seeding of
                RandomSeed ->
                    ( "", True )

                FixedSeed seed ->
                    ( seed, False )
    in
    InputGroup.view [ InputGroup.small, class "w-auto!" ]
        [ InputGroup.start [] [ label [ for "seed" ] [ text "Seed" ] ]
        , InputGroup.input
            [ id "seed"
            , name "seed"
            , attribute "inputmode" "numeric"
            , validity
            , placeholder "Random"
            , value typed
            , onInput SeedChanged
            , disabled locked
            , class "tabular-nums md:w-24! md:flex-none!"
            ]
        , InputGroup.end []
            [ InputGroup.button
                |> Button.icon (Phosphor.diceFive Bold |> Phosphor.withClass "size-4" |> Phosphor.toHtml [ attribute "aria-hidden" "true" ])
                |> Button.label "Random seed"
                |> Button.toHtml
                    [ attribute "aria-pressed"
                        (if random then
                            "true"

                         else
                            "false"
                        )
                    , title "Random seed"
                    , onClick (RandomSeedToggled (not random))
                    , disabled locked
                    , class "aria-pressed:bg-muted! aria-pressed:text-foreground! dark:aria-pressed:bg-input!"
                    ]
            ]
        ]


viewStart : Model -> Html Msg
viewStart model =
    generateButton
        (if Pipeline.isDownloaded model.settings.weights model || Pipeline.inMemory model.device == Just model.settings.weights then
            "Generate"

         else
            "Download and generate"
        )
        [ disabled (model.settings.prompt == Styling) ]


generateButton : String -> List (Html.Attribute Msg) -> Html Msg
generateButton label attributes =
    Button.new
        |> Button.text label
        |> Button.startIcon (Phosphor.sparkle Bold |> Phosphor.withClass "size-4" |> Phosphor.toHtml [ attribute "aria-hidden" "true" ])
        |> Button.small
        |> Button.toHtml (type_ "submit" :: class "ms-auto max-md:w-full" :: attributes)


cancel : Html Msg
cancel =
    Button.new
        |> Button.text "Cancel"
        |> Button.outline
        |> Button.small
        |> Button.toHtml [ onClick CancelClicked, class "ms-auto" ]


viewWaiting : String -> List (Html Msg)
viewWaiting reason =
    [ p [ class "min-w-0 flex-1 text-sm text-muted-foreground", attribute "role" "status" ] [ text reason ]
    , generateButton "Generate" [ disabled True ]
    ]


stepLabel : Int -> Int -> String
stepLabel done steps =
    if done < steps then
        "Generating · Step " ++ String.fromInt (done + 1) ++ " of " ++ String.fromInt steps

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


type alias Notice =
    { open : Bool
    , message : Message
    }


fromCheck : Pipeline.Validation -> Notice
fromCheck check =
    { open = check.failing, message = Problem check.problem }


fromMessage : { a | message : Maybe Message, fading : Maybe Message } -> List Notice
fromMessage source =
    case ( source.message, source.fading ) of
        ( Just current, _ ) ->
            [ { open = True, message = current } ]

        ( Nothing, Just faded ) ->
            [ { open = False, message = faded } ]

        ( Nothing, Nothing ) ->
            []


notices : List Notice -> Html msg
notices lines =
    let
        open =
            List.any .open lines
    in
    collapsible open
        [ class "-mx-(--card-spacing) -mt-2 transition-[grid-template-rows,margin-bottom,opacity]"
        , if open then
            class "-mb-(--card-spacing)"

          else
            class ""
        ]
        ( "band"
        , div [ class "mt-3 border-t px-(--card-spacing) py-1.75" ]
            (List.map viewNotice lines)
        )


viewNotice : Notice -> Html msg
viewNotice line =
    collapsible line.open
        [ class "transition-[grid-template-rows,opacity]" ]
        ( if line.open then
            "shown"

          else
            "hidden"
        , div [ class "py-0.75" ] [ notice line.open line.message ]
        )


collapsible : Bool -> List (Html.Attribute msg) -> ( String, Html msg ) -> Html msg
collapsible open attributes ( key, child ) =
    Keyed.node "div"
        ([ class "grid duration-200 ease-out motion-reduce:transition-none"
         , if open then
            class "grid-rows-[1fr] opacity-100"

           else
            class "grid-rows-[0fr] opacity-0"
         ]
            ++ attributes
            ++ (if open then
                    []

                else
                    [ attribute "aria-hidden" "true", attribute "inert" "" ]
               )
        )
        [ ( key, div [ class "min-h-0 overflow-hidden" ] [ child ] ) ]


notice : Bool -> Message -> Html msg
notice live shown =
    let
        role name =
            if live then
                [ attribute "role" name ]

            else
                []
    in
    case shown of
        Problem reason ->
            div (class "flex items-start gap-2 text-sm text-destructive" :: role "alert")
                [ Phosphor.warning Bold |> Phosphor.withClass "mt-0.5 size-4 shrink-0" |> Phosphor.toHtml [ attribute "aria-hidden" "true" ]
                , span [] [ text reason ]
                ]

        Info note ->
            p (class "flex items-start gap-2 text-sm text-muted-foreground" :: role "status")
                [ Phosphor.info Bold |> Phosphor.withClass "mt-0.5 size-4 shrink-0" |> Phosphor.toHtml [ attribute "aria-hidden" "true" ]
                , span [] [ text note ]
                ]


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
                        (if found.cached then
                            "Loading the "

                         else
                            "Downloading the "
                        )
                            ++ String.toLower (componentName found.component)

                    Nothing ->
                        "Loading " ++ Pipeline.weightsName chosen ++ "…"
                )
            ]
        |> Progress.formatValue (\done -> String.fromInt (floor done) ++ "%")
        |> Progress.toHtml [ class "[&_[data-slot=progress-value]]:text-xs" ]


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
        (case ( model.device, pictures ) of
            ( Supported _ (Generating _ job stage), _ ) ->
                viewPending job stage :: pictures

            ( Supported _ _, _ :: _ ) ->
                pictures

            _ ->
                [ viewEmpty model.device ]
        )


viewEmpty : Pipeline.Device -> Html msg
viewEmpty device =
    SupportView.layers [ class "w-full flex-1 content-center" ]
        device
        { checking =
            Empty.new
                |> Empty.icon
                    (Phosphor.circleNotch Bold
                        |> Phosphor.withClass "animate-spin motion-reduce:animate-none"
                        |> Phosphor.toHtml [ attribute "aria-hidden" "true" ]
                    )
                |> Empty.title [ text "Checking GPU support" ]
                |> Empty.description [ text "Making sure this device can generate images." ]
                |> Empty.toHtml [ attribute "role" "status" ]
        , supported =
            Empty.new
                |> Empty.icon (Phosphor.paintBrush Bold |> Phosphor.toHtml [ attribute "aria-hidden" "true" ])
                |> Empty.title [ text "Nothing here yet" ]
                |> Empty.description
                    [ text "Describe a picture and press Generate. Save the ones you want to keep." ]
                |> Empty.toHtml []
        , unsupported =
            Empty.new
                |> Empty.icon (Phosphor.warning Bold |> Phosphor.toHtml [ attribute "aria-hidden" "true" ])
                |> Empty.title [ text SupportView.unsupportedTitle ]
                |> Empty.description [ text SupportView.unsupportedReason ]
                |> Empty.toHtml []
        }


viewPending : Pipeline.Job -> Stage -> Html msg
viewPending job stage =
    div (class "relative flex items-center justify-center overflow-hidden rounded-xl" :: frame job)
        [ div [ class "absolute inset-0 animate-pulse bg-muted motion-reduce:animate-none" ] []
        , case stage of
            Denoising done ->
                let
                    steps =
                        Steps.toInt job.steps
                in
                Progress.new (100 * toFloat done / toFloat steps)
                    |> Progress.label [ text (stepLabel done steps) ]
                    |> Progress.formatValue (\finished -> String.fromInt (floor finished) ++ "%")
                    |> Progress.toHtml [ class "relative w-2/3 max-w-64 tabular-nums [&_[data-slot=progress-track]]:bg-foreground/10!" ]

            Stopping ->
                p [ class "relative text-sm text-muted-foreground", attribute "role" "status" ] [ text "Cancelling…" ]
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
