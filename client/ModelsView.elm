module ModelsView exposing (view)

import Html exposing (Html, div, p, text)
import Html.Attributes exposing (attribute, class, disabled)
import Html.Events exposing (onClick)
import Phosphor exposing (IconWeight(..))
import Pipeline exposing (Device(..), Engine(..), Model, Msg(..), Storage(..), Weights)
import StudioView
import Stylist
import SupportView
import UI.Badge as Badge
import UI.Button as Button
import UI.Card as Card
import UI.Skeleton as Skeleton


type Row
    = Downloading (List Pipeline.Download)
    | InMemory
    | Downloaded
    | NotDownloaded


view : { pipeline : Msg -> msg, stylist : Stylist.Msg -> msg } -> Stylist.Model -> Model -> Html msg
view toMsg writer model =
    div [ class "flex flex-col gap-6" ]
        (List.map (viewWeights model >> Html.map toMsg.pipeline) Pipeline.weights
            ++ [ Html.map toMsg.stylist (stylist model.device writer)
               , Html.map toMsg.pipeline (storage model)
               ]
        )


storageHint : Device -> String
storageHint device =
    case device of
        Unchecked ->
            inMemoryHint device

        Checking ->
            inMemoryHint device

        Unsupported Kept ->
            inMemoryHint device

        Unsupported Emptying ->
            inMemoryHint device

        Unsupported Unreachable ->
            "Reload the page to remove them."

        Supported _ _ ->
            inMemoryHint device


inMemoryHint : Device -> String
inMemoryHint device =
    if Pipeline.inMemory device == Nothing then
        "Remove local copies to free up space."

    else
        "Release the model first."


storage : Model -> Html Msg
storage model =
    div [ class "flex flex-col gap-6" ]
        [ StudioView.message model.message
        , Card.new
            |> Card.title [ Html.h3 [] [ text "Model storage" ] ]
            |> Card.description [ text "Downloaded models are kept in this browser." ]
            |> Card.footer [ class "flex-col items-start gap-3 sm:flex-row sm:items-center sm:justify-between" ]
                [ p [ class "text-xs text-muted-foreground" ]
                    [ text (storageHint model.device) ]
                , Button.new
                    |> Button.text
                        (if Pipeline.engineOf model.device == Just Clearing || model.device == Unsupported Emptying then
                            "Removing…"

                         else
                            "Remove downloaded models"
                        )
                    |> Button.outline
                    |> Button.small
                    |> Button.startIcon (Phosphor.trash Regular |> Phosphor.toHtml [ attribute "aria-hidden" "true" ])
                    |> Button.toHtml [ onClick ClearClicked, disabled (Pipeline.engineOf model.device /= Just Empty && model.device /= Unsupported Kept), class "max-sm:w-full" ]
                ]
            |> Card.toHtml [ class "w-full [--card-spacing:--spacing(5)] sm:[--card-spacing:--spacing(6)]" ]
        ]


row : Model -> Weights -> Row
row model weights =
    case model.device of
        Supported _ (Loading chosen parts _) ->
            if chosen == weights then
                Downloading parts

            else
                stored model weights

        device ->
            if Pipeline.inMemory device == Just weights then
                InMemory

            else
                stored model weights


stored : Model -> Weights -> Row
stored model weights =
    if Pipeline.isDownloaded weights model then
        Downloaded

    else
        NotDownloaded


viewWeights : Model -> Weights -> Html Msg
viewWeights model weights =
    let
        name =
            Pipeline.weightsName weights

        state =
            row model weights

        idle =
            Pipeline.engineOf model.device == Just Empty || Pipeline.inMemory model.device /= Nothing && not (isGenerating model.device)
    in
    Card.new
        |> Card.content []
            [ div [ class "flex items-center gap-4 py-2" ]
                [ div [ class "flex size-10 shrink-0 items-center justify-center rounded-lg bg-muted text-muted-foreground" ]
                    [ Phosphor.cube Regular
                        |> Phosphor.withClass "size-5"
                        |> Phosphor.toHtml [ attribute "aria-hidden" "true" ]
                    ]
                , div [ class "min-w-0 flex-1 space-y-1" ]
                    [ div [ class "flex flex-wrap items-center gap-2" ]
                        (Html.h3 [ class "text-base font-semibold tracking-tight" ] [ text name ]
                            :: (case state of
                                    InMemory ->
                                        [ badge "In memory" ]

                                    Downloaded ->
                                        [ badge "Downloaded" ]

                                    _ ->
                                        []
                               )
                        )
                    , p [ class "text-sm text-muted-foreground" ]
                        [ text
                            (case state of
                                InMemory ->
                                    Pipeline.weightsSize weights

                                Downloaded ->
                                    Pipeline.weightsSize weights

                                _ ->
                                    Pipeline.weightsSize weights ++ " download"
                            )
                        ]
                    ]
                ]
            ]
        |> Card.footer [ class "min-h-16 gap-4" ]
            (case state of
                Downloading parts ->
                    [ div [ class "min-w-0 flex-1" ] [ StudioView.loading weights parts ]
                    , Button.new
                        |> Button.text "Cancel"
                        |> Button.outline
                        |> Button.small
                        |> Button.startIcon (Phosphor.x Regular |> Phosphor.toHtml [ attribute "aria-hidden" "true" ])
                        |> Button.toHtml [ onClick CancelClicked, class "shrink-0" ]
                    ]

                InMemory ->
                    [ p [ class "flex-1 text-xs text-muted-foreground" ] [ text "Ready to generate on this device." ]
                    , Button.new
                        |> Button.text "Release"
                        |> Button.outline
                        |> Button.small
                        |> Button.startIcon (Phosphor.ejectSimple Regular |> Phosphor.toHtml [ attribute "aria-hidden" "true" ])
                        |> Button.toHtml [ onClick ReleaseClicked, disabled (not idle), class "shrink-0" ]
                    ]

                Downloaded ->
                    let
                        checkCircle =
                            Phosphor.checkCircle Regular |> Phosphor.toHtml [ attribute "aria-hidden" "true" ]
                    in
                    [ hint []
                        model.device
                        [ checkCircle, text "Saved in this browser. Ready to use in the studio." ]
                        [ checkCircle, text ("Saved in this browser. " ++ SupportView.needsWebGpu) ]
                    ]

                NotDownloaded ->
                    [ hint [ class "flex-1" ] model.device [ text "Download once, use in the studio." ] [ text SupportView.needsWebGpu ]
                    , Button.new
                        |> Button.text "Download"
                        |> Button.outline
                        |> Button.small
                        |> Button.startIcon (Phosphor.downloadSimple Regular |> Phosphor.toHtml [ attribute "aria-hidden" "true" ])
                        |> Button.toHtml
                            [ onClick (DownloadClicked weights)
                            , disabled (not idle)
                            , attribute "aria-label" ("Download " ++ name)
                            , class "shrink-0"
                            ]
                    ]
            )
        |> Card.toHtml [ class "w-full [--card-spacing:--spacing(5)] sm:[--card-spacing:--spacing(6)]" ]


stylist : Pipeline.Device -> Stylist.Model -> Html Stylist.Msg
stylist device model =
    let
        idle =
            model.state == Stylist.Asleep || model.state == Stylist.Awake
    in
    Card.new
        |> Card.content []
            [ div [ class "flex items-center gap-4 py-2" ]
                [ div [ class "flex size-10 shrink-0 items-center justify-center rounded-lg bg-muted text-muted-foreground" ]
                    [ Phosphor.magicWand Regular
                        |> Phosphor.withClass "size-5"
                        |> Phosphor.toHtml [ attribute "aria-hidden" "true" ]
                    ]
                , div [ class "min-w-0 flex-1 space-y-1" ]
                    [ div [ class "flex flex-wrap items-center gap-2" ]
                        (Html.h3 [ class "text-base font-semibold tracking-tight" ] [ text "Prompt writer" ]
                            :: (if model.state /= Stylist.Asleep && model.state /= Stylist.Removing && model.downloaded then
                                    [ badge "In memory" ]

                                else if model.downloaded then
                                    [ badge "Downloaded" ]

                                else
                                    []
                               )
                        )
                    , p [ class "text-sm text-muted-foreground" ]
                        [ text
                            (if model.downloaded then
                                "Qwen3 1.7B, " ++ Stylist.modelSize

                             else
                                "Qwen3 1.7B, " ++ Stylist.modelSize ++ " download"
                            )
                        ]
                    ]
                ]
            ]
        |> Card.footer [ class "min-h-16 flex-col items-start gap-3 sm:flex-row sm:items-center sm:justify-between" ]
            [ hint []
                device
                [ text
                    (if model.downloaded then
                        "Writes the AI Style prompts in the studio."

                     else
                        "Downloads the first time you use AI Style in the studio."
                    )
                ]
                [ text SupportView.needsWebGpu ]
            , if model.downloaded then
                Button.new
                    |> Button.text
                        (if model.state == Stylist.Removing then
                            "Removing…"

                         else
                            "Remove"
                        )
                    |> Button.outline
                    |> Button.small
                    |> Button.startIcon (Phosphor.trash Regular |> Phosphor.toHtml [ attribute "aria-hidden" "true" ])
                    |> Button.toHtml
                        [ onClick Stylist.RemoveClicked
                        , disabled (not idle)
                        , attribute "aria-label" "Remove the prompt writer"
                        , class "max-sm:w-full"
                        ]

              else
                text ""
            , StudioView.message model.message
            ]
        |> Card.toHtml [ class "w-full [--card-spacing:--spacing(5)] sm:[--card-spacing:--spacing(6)]" ]


hint : List (Html.Attribute msg) -> Pipeline.Device -> List (Html msg) -> List (Html msg) -> Html msg
hint attributes device supported unsupported =
    SupportView.layers attributes
        device
        { checking = div [ attribute "aria-hidden" "true" ] [ Skeleton.view [ class "my-0.5 h-3 w-48 max-w-full" ] ]
        , supported = p [ class "flex items-center gap-2 text-xs text-muted-foreground" ] supported
        , unsupported = p [ class "flex items-center gap-2 text-xs text-muted-foreground" ] unsupported
        }


isGenerating : Device -> Bool
isGenerating device =
    case device of
        Supported _ (Generating _ _ _) ->
            True

        _ ->
            False


badge : String -> Html msg
badge label =
    Badge.new
        |> Badge.text label
        |> Badge.secondary
        |> Badge.toHtml []
