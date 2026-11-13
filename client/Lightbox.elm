module Lightbox exposing (Config, Shown, view)

import Data.Dimension as Dimension
import Data.Prompt as Prompt
import Data.Seed as Seed
import Data.Steps as Steps
import Html exposing (Attribute, Html, button, div, img, p, text)
import Html.Attributes exposing (alt, attribute, class, height, src, style, type_, width)
import Html.Events exposing (onClick)
import Json.Decode as Decode exposing (Decoder)
import Overlay exposing (glass)
import Phosphor exposing (IconWeight(..))
import Pipeline
import UI.Button as Button
import UI.Dialog as Dialog


type alias Config msg =
    { id : String
    , open : Bool
    , detailed : Bool
    , onClose : msg
    , onToggle : msg
    , onStep : Int -> msg
    }


type alias Shown msg =
    { url : String
    , job : Pipeline.Job
    , model : String
    , pinned : Bool
    , notice : Html msg
    , actions : List (Html msg)
    }


view : Config msg -> Maybe (Shown msg) -> Html msg
view config shown =
    div [ attribute "data-slot" "lightbox", class "contents", onArrows config.onStep ]
        [ viewDialog config shown ]


onArrows : (Int -> msg) -> Attribute msg
onArrows onStep =
    Html.Events.custom "keydown"
        (Decode.map3 (\key role modified -> ( key, String.startsWith "menuitem" role || modified ))
            (Decode.field "key" Decode.string)
            (Decode.oneOf [ Decode.at [ "target", "role" ] Decode.string, Decode.succeed "" ])
            (Decode.map3 (\alt control meta -> alt || control || meta)
                (flag "altKey")
                (flag "ctrlKey")
                (flag "metaKey")
            )
            |> Decode.andThen
                (\pressed ->
                    case pressed of
                        ( "ArrowLeft", False ) ->
                            Decode.succeed { message = onStep -1, stopPropagation = True, preventDefault = True }

                        ( "ArrowRight", False ) ->
                            Decode.succeed { message = onStep 1, stopPropagation = True, preventDefault = True }

                        _ ->
                            Decode.fail "not an arrow"
                )
        )


flag : String -> Decoder Bool
flag name =
    Decode.oneOf [ Decode.field name Decode.bool, Decode.succeed False ]


viewDialog : Config msg -> Maybe (Shown msg) -> Html msg
viewDialog config shown =
    Dialog.new
        |> Dialog.id config.id
        |> Dialog.open (config.open && shown /= Nothing) config.onClose
        |> Dialog.label "Picture"
        |> Dialog.hideCloseButton
        |> Dialog.content (Maybe.withDefault [] (Maybe.map (viewPicture config) shown))
        |> Dialog.toHtml [ class "w-fit! max-w-none! gap-0! overflow-hidden bg-black! p-0!" ]


viewPicture : Config msg -> Shown msg -> List (Html msg)
viewPicture config shown =
    let
        job =
            shown.job

        across =
            Dimension.toInt job.width

        down =
            Dimension.toInt job.height
    in
    [ div [ class "group/picture relative" ]
        [ button
            [ type_ "button"
            , onClick config.onToggle
            , attribute "aria-label" "Show details"
            , attribute "aria-pressed"
                (if config.detailed then
                    "true"

                 else
                    "false"
                )
            , class "block outline-none"
            ]
            [ img
                [ src shown.url
                , alt (Prompt.toString job.prompt)
                , width across
                , height down
                , class "block h-auto max-w-none"
                , style "aspect-ratio" (String.fromInt across ++ " / " ++ String.fromInt down)
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
                (if config.detailed || shown.pinned then
                    "opacity-100"

                 else
                    "opacity-0"
                )
            ]
            [ div [ class "flex shrink-0 justify-end bg-linear-to-b from-black/60 to-transparent p-3 pb-8" ]
                [ Button.new
                    |> Button.icon (Phosphor.x Regular |> Phosphor.toHtml [ attribute "aria-hidden" "true" ])
                    |> Button.label "Close"
                    |> Button.small
                    |> Button.toHtml [ onClick config.onClose, glass, class "pointer-events-auto" ]
                ]
            , div [ class "flex min-h-0 flex-col gap-2 bg-linear-to-t from-black/85 via-black/55 to-transparent p-4 pt-16" ]
                [ p
                    [ onClick config.onToggle
                    , class "pointer-events-auto min-h-[2lh] overflow-y-auto overscroll-contain text-sm"
                    ]
                    [ text (Prompt.toString job.prompt) ]
                , p [ class "shrink-0 text-xs text-white/70 tabular-nums" ]
                    [ text
                        (String.join " · "
                            [ String.fromInt across ++ " × " ++ String.fromInt down
                            , String.fromInt (Steps.toInt job.steps) ++ " steps"
                            , "seed " ++ Seed.toString job.seed
                            , shown.model
                            ]
                        )
                    ]
                , div [ class "pointer-events-auto shrink-0 empty:hidden" ] [ shown.notice ]
                , div [ class "pointer-events-auto flex shrink-0 flex-wrap items-center gap-1.5" ] shown.actions
                ]
            ]
        ]
    ]
