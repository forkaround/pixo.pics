module ModelsView exposing (view)

import Html exposing (Html, div, h2, p, text)
import Html.Attributes exposing (attribute, class, disabled)
import Html.Events exposing (onClick)
import Phosphor exposing (IconWeight(..))
import Pipeline exposing (Engine(..), Model, Msg(..), Weights)
import StudioView
import UI.Badge as Badge
import UI.Button as Button
import UI.Item as Item


type Row
    = Downloading (List Pipeline.Download)
    | InMemory
    | Downloaded
    | NotDownloaded


view : Model -> Html Msg
view model =
    div [ class "mx-auto flex w-full max-w-2xl flex-col gap-6 p-4 md:p-8" ]
        [ div [ class "flex flex-col gap-1" ]
            [ h2 [ class "text-lg font-semibold" ] [ text "Models" ]
            , p [ class "text-sm text-muted-foreground" ]
                [ text
                    (if model.engine == Unsupported then
                        "They need WebGPU. Try a recent Chrome, Edge or Safari."

                     else
                        "They run in this browser: your prompts stay here."
                    )
                ]
            ]
        , Item.group [] (List.map (viewWeights model) Pipeline.weights)
        , StudioView.message model.message
        , div [ class "flex flex-wrap items-center gap-3" ]
            [ Button.new
                |> Button.text
                    (if model.engine == Clearing then
                        "Removing…"

                     else
                        "Remove downloaded models"
                    )
                |> Button.outline
                |> Button.small
                |> Button.startIcon (Phosphor.trash Regular |> Phosphor.toHtml [ attribute "aria-hidden" "true" ])
                |> Button.toHtml [ onClick ClearClicked, disabled (model.engine /= Empty) ]
            , p [ class "text-sm text-muted-foreground" ]
                [ text
                    (if Pipeline.inMemory model.engine == Nothing then
                        ""

                     else
                        "Release the model first."
                    )
                ]
            ]
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

        again =
            Button.new
                |> Button.icon (Phosphor.arrowsClockwise Regular |> Phosphor.toHtml [ attribute "aria-hidden" "true" ])
                |> Button.label ("Download " ++ name ++ " again")
                |> Button.ghost
                |> Button.small
                |> Button.toHtml [ onClick (DownloadAgainClicked weights), disabled (not idle) ]
    in
    Item.new
        |> Item.outline
        |> Item.icon (Phosphor.cube Regular |> Phosphor.toHtml [ attribute "aria-hidden" "true" ])
        |> Item.title
            (text name
                :: (case state of
                        InMemory ->
                            [ badge "In memory" ]

                        Downloaded ->
                            [ badge "Downloaded" ]

                        _ ->
                            []
                   )
            )
        |> Item.description
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
        |> Item.actions
            (case state of
                Downloading _ ->
                    [ Button.new
                        |> Button.text "Cancel"
                        |> Button.outline
                        |> Button.small
                        |> Button.toHtml [ onClick CancelClicked ]
                    ]

                InMemory ->
                    [ Button.new
                        |> Button.text "Release"
                        |> Button.outline
                        |> Button.small
                        |> Button.toHtml [ onClick ReleaseClicked, disabled (not idle) ]
                    , again
                    ]

                Downloaded ->
                    [ again ]

                NotDownloaded ->
                    [ Button.new
                        |> Button.text "Download"
                        |> Button.small
                        |> Button.startIcon (Phosphor.downloadSimple Regular |> Phosphor.toHtml [ attribute "aria-hidden" "true" ])
                        |> Button.toHtml
                            [ onClick (DownloadClicked weights)
                            , disabled (not idle)
                            , attribute "aria-label" ("Download " ++ name)
                            ]
                    ]
            )
        |> Item.footer
            (case state of
                Downloading parts ->
                    [ div [ class "w-full pt-2" ] [ StudioView.loading weights parts ] ]

                _ ->
                    []
            )
        |> Item.toHtml []


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
