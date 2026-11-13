module ModelsView exposing (view)

import Html exposing (Html, div, p, text)
import Html.Attributes exposing (attribute, class, disabled)
import Html.Events exposing (onClick)
import Phosphor exposing (IconWeight(..))
import Pipeline exposing (Engine(..), Model, Msg(..), Weights)
import StudioView
import Stylist
import UI.Badge as Badge
import UI.Button as Button
import UI.Card as Card


type Row
    = Downloading (List Pipeline.Download)
    | InMemory
    | Downloaded
    | NotDownloaded


view : { pipeline : Msg -> msg, stylist : Stylist.Msg -> msg } -> Stylist.Model -> Model -> Html msg
view toMsg writer model =
    div [ class "flex flex-col gap-6" ]
        (List.map (viewWeights model >> Html.map toMsg.pipeline) Pipeline.weights
            ++ [ Html.map toMsg.stylist (stylist writer)
               , Html.map toMsg.pipeline (storage model)
               ]
        )


storage : Model -> Html Msg
storage model =
    div [ class "flex flex-col gap-6" ]
        [ StudioView.message model.message
        , Card.new
            |> Card.title [ Html.h3 [] [ text "Model storage" ] ]
            |> Card.description [ text "Downloaded models are kept in this browser." ]
            |> Card.footer [ class "flex-col items-start gap-3 sm:flex-row sm:items-center sm:justify-between" ]
                [ p [ class "text-xs text-muted-foreground" ]
                    [ text
                        (if Pipeline.inMemory model.engine == Nothing then
                            "Remove local copies to free up space."

                         else
                            "Release the model first."
                        )
                    ]
                , Button.new
                    |> Button.text
                        (if model.engine == Clearing then
                            "Removing…"

                         else
                            "Remove downloaded models"
                        )
                    |> Button.outline
                    |> Button.small
                    |> Button.startIcon (Phosphor.trash Regular |> Phosphor.toHtml [ attribute "aria-hidden" "true" ])
                    |> Button.toHtml [ onClick ClearClicked, disabled (model.engine /= Empty), class "max-sm:w-full" ]
                ]
            |> Card.toHtml [ class "w-full [--card-spacing:--spacing(5)] sm:[--card-spacing:--spacing(6)]" ]
        ]


row : Model -> Weights -> Row
row model weights =
    case model.engine of
        Loading chosen parts _ ->
            if chosen == weights then
                Downloading parts

            else
                stored model weights

        engine ->
            if Pipeline.inMemory engine == Just weights then
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
            model.engine == Empty || Pipeline.inMemory model.engine /= Nothing && not (isGenerating model.engine)
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
                        |> Button.toHtml [ onClick CancelClicked, class "shrink-0" ]
                    ]

                InMemory ->
                    [ p [ class "flex-1 text-xs text-muted-foreground" ] [ text "Ready to generate on this device." ]
                    , Button.new
                        |> Button.text "Release"
                        |> Button.outline
                        |> Button.small
                        |> Button.toHtml [ onClick ReleaseClicked, disabled (not idle), class "shrink-0" ]
                    ]

                Downloaded ->
                    [ p [ class "flex items-center gap-2 text-xs text-muted-foreground" ]
                        [ Phosphor.checkCircle Regular |> Phosphor.toHtml [ attribute "aria-hidden" "true" ]
                        , text "Saved in this browser. Ready to use in the studio."
                        ]
                    ]

                NotDownloaded ->
                    [ p [ class "flex-1 text-xs text-muted-foreground" ] [ text "Download once, use in the studio." ]
                    , Button.new
                        |> Button.text "Download"
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


stylist : Stylist.Model -> Html Stylist.Msg
stylist model =
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
            [ p [ class "text-xs text-muted-foreground" ]
                [ text
                    (if model.downloaded then
                        "Writes the AI Style prompts in the studio."

                     else
                        "Downloads the first time you use AI Style in the studio."
                    )
                ]
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


isGenerating : Engine -> Bool
isGenerating engine =
    case engine of
        Generating _ _ _ ->
            True

        _ ->
            False


badge : String -> Html msg
badge label =
    Badge.new
        |> Badge.text label
        |> Badge.secondary
        |> Badge.toHtml []
