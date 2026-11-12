module LibraryView exposing (search, view)

import Data.Dimension as Dimension
import Data.Prompt as Prompt
import Data.Seed as Seed
import Data.Steps as Steps
import Html exposing (Attribute, Html, button, div, img, p, span, text)
import Html.Attributes exposing (alt, attribute, class, download, height, href, id, placeholder, src, style, type_, value, width)
import Html.Events exposing (onClick, onInput)
import Library exposing (Model, Msg(..), Picture, Pictures(..))
import Overlay exposing (glass, red)
import Path
import Phosphor exposing (IconWeight(..))
import Share
import UI.Button as Button
import UI.Dialog as Dialog
import UI.DropdownMenu as DropdownMenu
import UI.Empty as Empty
import UI.Input as Input
import UI.Skeleton as Skeleton


search : Model -> Html Msg
search model =
    div [ class "relative ms-auto w-full max-w-xs" ]
        [ span [ class "pointer-events-none absolute inset-y-0 start-2.5 flex items-center text-muted-foreground" ]
            [ Phosphor.magnifyingGlass Regular |> Phosphor.toHtml [ attribute "aria-hidden" "true" ] ]
        , Input.view
            [ id "search"
            , type_ "search"
            , attribute "aria-label" "Search prompts"
            , placeholder "Search prompts"
            , value model.query
            , onInput QueryChanged
            , class "ps-8"
            ]
        ]


view : Model -> Html Msg
view model =
    div [ class "flex flex-1 flex-col" ]
        [ case model.pictures of
            NotLoaded ->
                viewLoading

            Loading ->
                viewLoading

            Unavailable reason ->
                Empty.new
                    |> Empty.icon (icon Phosphor.warning)
                    |> Empty.title [ text "Can't open the library" ]
                    |> Empty.description [ text reason ]
                    |> Empty.toHtml []

            Loaded [] ->
                Empty.new
                    |> Empty.icon (icon Phosphor.images)
                    |> Empty.title [ text "No pictures yet" ]
                    |> Empty.description [ text "Save a picture from the studio and it is kept here, in this browser, with everything that made it." ]
                    |> Empty.content
                        [ Button.new
                            |> Button.text "Open the studio"
                            |> Button.outline
                            |> Button.small
                            |> Button.toAnchor [ href (Path.toString Path.Studio) ]
                        ]
                    |> Empty.toHtml []

            Loaded pictures ->
                case Library.visible model of
                    [] ->
                        Empty.new
                            |> Empty.icon (icon Phosphor.magnifyingGlass)
                            |> Empty.title [ text "Nothing matches" ]
                            |> Empty.description [ text ("No prompt matches “" ++ String.trim model.query ++ "”.") ]
                            |> Empty.toHtml []

                    found ->
                        div [ class "flex flex-col gap-3 p-4" ]
                            [ p [ class "text-xs text-muted-foreground tabular-nums", attribute "role" "status" ]
                                [ text (count (List.length found) (List.length pictures)) ]
                            , div [ class "columns-2 gap-4 md:columns-3 xl:columns-4 2xl:columns-5" ]
                                (List.map (viewTile model) found)
                            ]
        , viewDialog model
        , if model.zoomed then
            text ""

          else
            div [ class "pointer-events-none fixed inset-x-0 bottom-4 z-40 flex justify-center px-4" ]
                [ viewNotice model.notice ]
        ]


count : Int -> Int -> String
count found total =
    if found == total then
        if total == 1 then
            "1 picture"

        else
            String.fromInt total ++ " pictures"

    else
        String.fromInt found ++ " of " ++ String.fromInt total ++ " pictures"


icon : Phosphor.Icon -> Html msg
icon shape =
    shape Regular |> Phosphor.toHtml [ attribute "aria-hidden" "true" ]


viewLoading : Html Msg
viewLoading =
    div [ class "columns-2 gap-4 p-4 md:columns-3 xl:columns-4 2xl:columns-5", attribute "aria-hidden" "true" ]
        (List.map
            (\shape -> Skeleton.view [ class "mb-4 w-full break-inside-avoid rounded-xl", style "aspect-ratio" shape ])
            [ "1 / 1", "4 / 5", "16 / 9", "9 / 16", "1 / 1", "2 / 3", "16 / 9", "4 / 5" ]
        )


viewTile : Model -> Picture -> Html Msg
viewTile model picture =
    let
        prompt =
            Prompt.toString picture.job.prompt

        confirming =
            model.confirming == Just picture.id && not model.zoomed
    in
    div
        [ class "group relative mb-4 break-inside-avoid overflow-hidden rounded-xl bg-muted shadow-sm ring-1 ring-foreground/10"
        , style "aspect-ratio" (ratio picture)
        ]
        [ button
            [ type_ "button"
            , onClick (Opened picture.id)
            , attribute "aria-label" ("Open " ++ prompt)
            , class "block size-full cursor-zoom-in outline-none"
            ]
            [ img
                [ src picture.url
                , alt ""
                , width (Dimension.toInt picture.job.width)
                , height (Dimension.toInt picture.job.height)
                , attribute "loading" "lazy"
                , class "size-full object-cover transition-transform duration-300 ease-out group-hover:scale-105"
                ]
                []
            ]
        , div
            [ class "pointer-events-none absolute inset-0 flex flex-col justify-end gap-2 bg-linear-to-t from-black/80 via-black/25 to-transparent p-3 transition-opacity duration-200 group-focus-within:opacity-100 group-hover:opacity-100 has-[:popover-open]:opacity-100"
            , class
                (if confirming then
                    "opacity-100"

                 else
                    "opacity-0"
                )
            ]
            (if confirming then
                [ p [ class "text-sm font-medium text-white" ] [ text "Delete this picture?" ]
                , div [ class "pointer-events-auto flex items-center gap-2" ]
                    [ Button.new
                        |> Button.text "Cancel"
                        |> Button.small
                        |> Button.toHtml [ onClick DeleteCancelled, glass ]
                    , Button.new
                        |> Button.text "Delete"
                        |> Button.small
                        |> Button.toHtml [ onClick (DeleteConfirmed picture.id), red ]
                    ]
                ]

             else
                [ p [ class "line-clamp-3 text-sm text-white" ] [ text prompt ]
                , div [ class "pointer-events-auto flex items-center gap-1.5" ]
                    [ Button.new
                        |> Button.icon (icon Phosphor.downloadSimple)
                        |> Button.label "Download"
                        |> Button.small
                        |> Button.toAnchor [ href picture.url, download (Library.fileName picture.job), glass ]
                    , viewShare "share-"
                        model
                        picture
                        ((Button.new |> Button.icon (text "") |> Button.small |> Button.toAttributes)
                            ++ [ glass, attribute "aria-label" "Share" ]
                        )
                        [ icon Phosphor.shareNetwork ]
                    , Button.new
                        |> Button.icon (icon Phosphor.trash)
                        |> Button.label "Delete"
                        |> Button.small
                        |> Button.toHtml [ onClick (DeleteRequested picture.id), red, class "ms-auto" ]
                    ]
                ]
            )
        ]


ratio : Picture -> String
ratio picture =
    String.fromInt (Dimension.toInt picture.job.width) ++ " / " ++ String.fromInt (Dimension.toInt picture.job.height)


viewShare : String -> Model -> Picture -> List (Attribute Msg) -> List (Html Msg) -> Html Msg
viewShare prefix model picture attrs children =
    DropdownMenu.new
        |> DropdownMenu.id (prefix ++ String.fromInt picture.id)
        |> DropdownMenu.trigger attrs children
        |> DropdownMenu.content
            ((if model.canShare then
                [ DropdownMenu.item [ onClick (ShareClicked picture.id) ] [ icon Phosphor.export, text "Share…" ] ]

              else
                []
             )
                ++ [ DropdownMenu.item [ onClick (CopyClicked picture.id) ] [ icon Phosphor.copy, text "Copy picture" ]
                   , DropdownMenu.separator
                   , DropdownMenu.label [ text "Copy it, then post on" ]
                   ]
                ++ List.map
                    (\network ->
                        DropdownMenu.item [ onClick (PostClicked network picture.id) ]
                            [ icon (logo network), text (Share.name network) ]
                    )
                    Share.networks
            )
        |> DropdownMenu.toHtml [ class "w-48" ]


logo : Share.Network -> Phosphor.Icon
logo network =
    case network of
        Share.X ->
            Phosphor.xLogo

        Share.Threads ->
            Phosphor.threadsLogo

        Share.Bluesky ->
            Phosphor.butterfly

        Share.LinkedIn ->
            Phosphor.linkedinLogo

        Share.Reddit ->
            Phosphor.redditLogo


viewDialog : Model -> Html Msg
viewDialog model =
    let
        shown =
            Maybe.andThen (\viewed -> Library.find viewed model) model.viewing
    in
    Dialog.new
        |> Dialog.id "picture"
        |> Dialog.open (model.zoomed && shown /= Nothing) Closed
        |> Dialog.label "Picture"
        |> Dialog.hideCloseButton
        |> Dialog.content (Maybe.withDefault [] (Maybe.map (viewZoomed model) shown))
        |> Dialog.toHtml [ class "w-fit! max-w-none! gap-0! overflow-hidden bg-black! p-0!" ]


viewZoomed : Model -> Picture -> List (Html Msg)
viewZoomed model picture =
    let
        job =
            picture.job

        across =
            Dimension.toInt job.width

        down =
            Dimension.toInt job.height

        confirming =
            model.confirming == Just picture.id

        pinned =
            model.detailed || confirming || model.notice /= Nothing
    in
    [ div [ class "group/picture relative" ]
        [ button
            [ type_ "button"
            , onClick DetailsToggled
            , attribute "aria-label" "Show details"
            , attribute "aria-pressed"
                (if model.detailed then
                    "true"

                 else
                    "false"
                )
            , class "block outline-none"
            ]
            [ img
                [ src picture.url
                , alt (Prompt.toString job.prompt)
                , width across
                , height down
                , class "block h-auto max-w-none"
                , style "aspect-ratio" (ratio picture)
                , style "width"
                    ("min(100vw - 2rem, (100svh - 2rem) * "
                        ++ String.fromInt across
                        ++ " / "
                        ++ String.fromInt down
                        ++ ", "
                        ++ String.fromInt (2 * across)
                        ++ "px)"
                    )
                ]
                []
            ]
        , div
            [ class "pointer-events-none absolute inset-0 flex flex-col justify-between overflow-hidden text-white transition-opacity duration-200 group-hover/picture:opacity-100 group-has-[:focus-visible]/picture:opacity-100 group-has-[:popover-open]/picture:opacity-100"
            , class
                (if pinned then
                    "opacity-100"

                 else
                    "opacity-0"
                )
            ]
            [ div [ class "flex justify-end bg-linear-to-b from-black/60 to-transparent p-3 pb-8" ]
                [ Button.new
                    |> Button.icon (icon Phosphor.x)
                    |> Button.label "Close"
                    |> Button.small
                    |> Button.toHtml [ onClick Closed, glass, class "pointer-events-auto" ]
                ]
            , div [ class "flex flex-col gap-2 bg-linear-to-t from-black/85 via-black/55 to-transparent p-4 pt-16" ]
                [ p [ class "line-clamp-4 text-sm" ] [ text (Prompt.toString job.prompt) ]
                , p [ class "text-xs text-white/70 tabular-nums" ]
                    [ text
                        (String.join " · "
                            [ String.fromInt across ++ " × " ++ String.fromInt down
                            , String.fromInt (Steps.toInt job.steps) ++ " steps"
                            , "seed " ++ Seed.toString job.seed
                            , Library.modelName picture
                            ]
                        )
                    ]
                , div [ class "pointer-events-auto empty:hidden" ] [ viewNotice model.notice ]
                , div [ class "pointer-events-auto flex flex-wrap items-center gap-1.5" ]
                    (if confirming then
                        [ span [ class "basis-full text-sm font-medium" ] [ text "Delete this picture?" ]
                        , Button.new
                            |> Button.text "Cancel"
                            |> Button.small
                            |> Button.toHtml [ onClick DeleteCancelled, glass ]
                        , Button.new
                            |> Button.text "Delete"
                            |> Button.small
                            |> Button.toHtml [ onClick (DeleteConfirmed picture.id), red ]
                        ]

                     else
                        [ Button.new
                            |> Button.text "Download"
                            |> Button.small
                            |> Button.startIcon (icon Phosphor.downloadSimple)
                            |> Button.toAnchor [ href picture.url, download (Library.fileName job), glass ]
                        , viewShare "share-open-"
                            model
                            picture
                            ((Button.new |> Button.text "Share" |> Button.small |> Button.toAttributes) ++ [ glass ])
                            [ icon Phosphor.shareNetwork, text "Share" ]
                        , Button.new
                            |> Button.icon (icon Phosphor.trash)
                            |> Button.label "Delete"
                            |> Button.small
                            |> Button.toHtml [ onClick (DeleteRequested picture.id), red, class "ms-auto" ]
                        ]
                    )
                ]
            ]
        ]
    ]


viewNotice : Maybe Library.Notice -> Html Msg
viewNotice notice =
    case notice of
        Nothing ->
            text ""

        Just shown ->
            div
                [ class "pointer-events-auto flex items-center gap-2 rounded-lg bg-popover px-3 py-2 text-sm text-popover-foreground shadow-md ring-1 ring-foreground/10"
                , attribute "role" "status"
                ]
                [ case shown of
                    Library.Info note ->
                        span [] [ text note ]

                    Library.Problem reason ->
                        span [ class "text-destructive" ] [ text reason ]
                , Button.new
                    |> Button.icon (icon Phosphor.x)
                    |> Button.label "Dismiss"
                    |> Button.ghost
                    |> Button.extraSmall
                    |> Button.toHtml [ onClick NoticeDismissed ]
                ]
