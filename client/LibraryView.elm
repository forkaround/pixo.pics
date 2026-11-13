module LibraryView exposing (filters, search, view)

import Data.Dimension as Dimension
import Data.Prompt as Prompt
import Data.Tag as Tag exposing (Tag)
import Html exposing (Attribute, Html, button, div, img, p, span, text)
import Html.Attributes exposing (alt, attribute, class, download, height, href, id, placeholder, src, style, type_, value, width)
import Html.Events exposing (onClick, onInput)
import Library exposing (Model, Msg(..), Picture, Pictures(..))
import Lightbox
import Overlay exposing (glass, red)
import Path
import Phosphor exposing (IconWeight(..))
import Share
import UI.Button as Button
import UI.DropdownMenu as DropdownMenu
import UI.Empty as Empty
import UI.Input as Input
import UI.Sidebar as Sidebar
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


filters : Model -> Html Msg
filters model =
    let
        chosen =
            Library.chosen model
    in
    Sidebar.group []
        [ Sidebar.groupLabel [ class "max-md:sr-only" ] [ text "Tags" ]
        , Sidebar.menu [ class "max-md:flex-row max-md:overflow-x-auto max-md:pb-1" ]
            (case model.pictures of
                Loaded pictures ->
                    viewFilter (chosen == Nothing) Nothing (Library.cover Nothing model) "All pictures" (Just (List.length pictures))
                        :: List.map
                            (\( tag, n ) -> viewFilter (chosen == Just tag) (Just tag) (Library.cover (Just tag) model) (Tag.toString tag) (Just n))
                            (Library.tags model)

                Unavailable _ ->
                    [ viewFilter (chosen == Nothing) Nothing Nothing "All pictures" Nothing ]

                _ ->
                    viewFilter True Nothing Nothing "All pictures" Nothing
                        :: List.repeat 4 viewFilterSkeleton
            )
        , viewHint model
        ]


viewFilter : Bool -> Maybe Tag -> Maybe Picture -> String -> Maybe Int -> Html Msg
viewFilter current tag cover label total =
    Sidebar.menuItem [ class "max-md:shrink-0" ]
        (Sidebar.menuButton
            ([ onClick (TagChosen tag)
             , attribute "aria-pressed"
                (if current then
                    "true"

                 else
                    "false"
                )
             , class "h-10! gap-2.5! ps-1.5! pe-8"
             ]
                ++ (if current then
                        [ Sidebar.active ]

                    else
                        []
                   )
            )
            [ viewCover cover
            , span [] [ text label ]
            ]
            :: (case total of
                    Just n ->
                        [ Sidebar.menuBadge
                            [ class "top-2.5! font-normal! text-muted-foreground! peer-hover/menu-button:text-sidebar-accent-foreground! peer-data-active/menu-button:text-sidebar-accent-foreground!" ]
                            [ text (String.fromInt n) ]
                        ]

                    Nothing ->
                        []
               )
        )


viewCover : Maybe Picture -> Html Msg
viewCover cover =
    span [ class "flex size-7 shrink-0 items-center justify-center overflow-hidden rounded-md bg-muted ring-1 ring-foreground/10" ]
        [ case cover of
            Just picture ->
                img
                    [ src picture.url
                    , alt ""
                    , attribute "loading" "lazy"
                    , attribute "decoding" "async"
                    , class "size-full object-cover"
                    ]
                    []

            Nothing ->
                Phosphor.images Bold |> Phosphor.toHtml [ attribute "aria-hidden" "true" ]
        ]


viewFilterSkeleton : Html Msg
viewFilterSkeleton =
    Sidebar.menuItem [ class "max-md:shrink-0" ]
        [ div [ class "flex h-10 items-center gap-2.5 rounded-lg ps-1.5 pe-8 max-md:w-32", attribute "aria-hidden" "true" ]
            [ Skeleton.view [ class "size-7 shrink-0" ]
            , Skeleton.view [ class "h-4 flex-1" ]
            ]
        ]


viewHint : Model -> Html Msg
viewHint model =
    case model.pictures of
        Loaded (_ :: _) ->
            if List.isEmpty (Library.tags model) then
                p [ class "px-2 text-xs text-muted-foreground max-md:hidden" ]
                    [ text "No tags yet. The prompt writer tags pictures as you save them." ]

            else
                text ""

        _ ->
            text ""


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
                    |> Empty.icon (Phosphor.warning Bold |> Phosphor.toHtml [ attribute "aria-hidden" "true" ])
                    |> Empty.title [ text "Can't open the library" ]
                    |> Empty.description [ text reason ]
                    |> Empty.toHtml []

            Loaded [] ->
                Empty.new
                    |> Empty.icon (Phosphor.images Bold |> Phosphor.toHtml [ attribute "aria-hidden" "true" ])
                    |> Empty.title [ text "No pictures yet" ]
                    |> Empty.description [ text "Pictures you save in the studio are kept in this browser." ]
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
                            |> Empty.icon (Phosphor.magnifyingGlass Bold |> Phosphor.toHtml [ attribute "aria-hidden" "true" ])
                            |> Empty.title [ text ("Nothing matches “" ++ String.trim model.query ++ "”") ]
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
                   , DropdownMenu.label [ text "Copy and post on" ]
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
    Lightbox.view
        { id = "picture"
        , open = model.zoomed
        , detailed = model.detailed
        , onClose = Closed
        , onToggle = DetailsToggled
        , onStep = Stepped
        }
        (Maybe.map (viewZoomed model) (Maybe.andThen (\viewed -> Library.find viewed model) model.viewing))


viewZoomed : Model -> Picture -> Lightbox.Shown Msg
viewZoomed model picture =
    let
        confirming =
            model.confirming == Just picture.id
    in
    { url = picture.url
    , job = picture.job
    , model = Library.modelName picture
    , pinned = confirming || model.notice /= Nothing
    , notice = viewNotice model.notice
    , actions =
        if confirming then
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
                |> Button.toAnchor [ href picture.url, download (Library.fileName picture.job), glass ]
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
    }


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
