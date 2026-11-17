module FeedView exposing (grid, job, lightbox, neighbour, skeleton, tile)

import Api
import Data.Dimension as Dimension
import Data.PictureId exposing (PictureId)
import Data.Prompt as Prompt
import Data.Seed as Seed
import Data.Steps as Steps
import Data.Username as Username
import Html exposing (Html, a, button, div, img, text)
import Html.Attributes exposing (alt, attribute, class, height, href, src, style, type_, width)
import Html.Events exposing (onClick)
import Lightbox
import Path
import Pipeline
import UI.Skeleton as Skeleton


job : Api.FeedPicture -> Maybe Pipeline.Job
job picture =
    Maybe.map5 Pipeline.Job
        (Prompt.parse picture.prompt)
        (Dimension.fromInt picture.width)
        (Dimension.fromInt picture.height)
        (Steps.fromInt picture.steps)
        (Seed.fromInt picture.seed)


tile : { open : PictureId -> msg, author : Bool } -> Api.FeedPicture -> Html msg
tile config picture =
    case job picture of
        Nothing ->
            text ""

        Just _ ->
            div [ class "mb-4 break-inside-avoid" ]
                [ button
                    [ type_ "button"
                    , onClick (config.open picture.id)
                    , attribute "aria-label" ("Open " ++ picture.prompt)
                    , class "block w-full overflow-hidden rounded-xl bg-muted shadow-sm ring-1 ring-foreground/10 outline-none focus-visible:ring-2 focus-visible:ring-ring"
                    , style "aspect-ratio" (String.fromInt picture.width ++ " / " ++ String.fromInt picture.height)
                    ]
                    [ img
                        [ src picture.url
                        , alt picture.prompt
                        , width picture.width
                        , height picture.height
                        , attribute "loading" "lazy"
                        , class "size-full object-cover"
                        ]
                        []
                    ]
                , if config.author then
                    a
                        [ href (Path.toString (Path.Profile picture.username))
                        , class "mt-1.5 block truncate px-1 text-xs text-muted-foreground hover:text-foreground"
                        ]
                        [ text (Username.toString picture.username) ]

                  else
                    text ""
                ]


grid : List (Html msg) -> Html msg
grid children =
    div [ class columns ] children


columns : String
columns =
    "columns-2 gap-4 md:columns-3 xl:columns-4 2xl:columns-5"


skeleton : Html msg
skeleton =
    div [ class columns, attribute "aria-hidden" "true" ]
        (List.indexedMap
            (\i _ ->
                div [ class "mb-4 break-inside-avoid" ]
                    [ Skeleton.view
                        [ class "w-full rounded-xl"
                        , style "aspect-ratio"
                            (if modBy 2 i == 0 then
                                "4 / 5"

                             else
                                "1 / 1"
                            )
                        ]
                    ]
            )
            (List.repeat 8 ())
        )


lightbox :
    { id : String
    , onClose : msg
    , onToggle : msg
    , onStep : Int -> msg
    , toasts : Html msg
    , detailed : Bool
    }
    -> Maybe ( Api.FeedPicture, List (Html msg) )
    -> Html msg
lightbox config shown =
    let
        prepared =
            shown
                |> Maybe.andThen
                    (\( picture, actions ) ->
                        job picture
                            |> Maybe.map
                                (\parsed ->
                                    { url = picture.url
                                    , job = parsed
                                    , model =
                                        Pipeline.weightsFromId picture.model
                                            |> Maybe.map Pipeline.weightsName
                                            |> Maybe.withDefault picture.model
                                    , pinned = False
                                    , actions = actions
                                    }
                                )
                    )
    in
    Lightbox.view
        { id = config.id
        , open =
            case prepared of
                Just _ ->
                    True

                Nothing ->
                    False
        , detailed = config.detailed
        , onClose = config.onClose
        , onToggle = config.onToggle
        , onStep = config.onStep
        , toasts = config.toasts
        }
        prepared


neighbour : Int -> PictureId -> List Api.FeedPicture -> Maybe PictureId
neighbour offset viewed pictures =
    let
        ids =
            pictures
                |> List.filter (\picture -> job picture /= Nothing)
                |> List.map .id
    in
    ids
        |> List.indexedMap Tuple.pair
        |> List.filter (\( _, known ) -> known == viewed)
        |> List.head
        |> Maybe.map (\( index, _ ) -> index + offset)
        |> Maybe.andThen
            (\index ->
                if index < 0 then
                    Nothing

                else
                    List.head (List.drop index ids)
            )
