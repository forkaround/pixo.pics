module UI.AlertDialog exposing
    ( AlertDialog
    , NeedsId
    , NeedsOpen
    , Ready
    , action
    , busy
    , cancel
    , description
    , id
    , media
    , new
    , open
    , small
    , title
    , toHtml
    )

import Html exposing (Attribute, Html)
import Html.Attributes exposing (attribute, class, classList, tabindex)
import Html.Events
import Json.Decode as Decode
import UI.Button as Button exposing (Button)


type AlertDialog id state msg
    = AlertDialog
        { id : String
        , open : Bool
        , onClose : Maybe msg
        , media : Maybe ( List (Attribute msg), List (Html msg) )
        , title : List (Html msg)
        , description : List (Html msg)
        , cancel : Maybe (List (Attribute msg) -> Html msg)
        , action : Maybe (List (Attribute msg) -> Html msg)
        , size : Size
        , busy : Bool
        }


type Size
    = Default
    | Small


type Ready
    = Ready


type NeedsId
    = NeedsId


type NeedsOpen
    = NeedsOpen


new : AlertDialog NeedsId NeedsOpen msg
new =
    AlertDialog
        { id = ""
        , open = False
        , onClose = Nothing
        , media = Nothing
        , title = []
        , description = []
        , cancel = Nothing
        , action = Nothing
        , size = Default
        , busy = False
        }


id : String -> AlertDialog NeedsId state msg -> AlertDialog Ready state msg
id string (AlertDialog config) =
    AlertDialog { config | id = string }


open : Bool -> msg -> AlertDialog id NeedsOpen msg -> AlertDialog id Ready msg
open isOpen onClose (AlertDialog config) =
    AlertDialog { config | open = isOpen, onClose = Just onClose }


media : List (Attribute msg) -> List (Html msg) -> AlertDialog id state msg -> AlertDialog id state msg
media attrs children (AlertDialog config) =
    AlertDialog { config | media = Just ( attrs, children ) }


title : List (Html msg) -> AlertDialog id state msg -> AlertDialog id state msg
title children (AlertDialog config) =
    AlertDialog { config | title = children }


description : List (Html msg) -> AlertDialog id state msg -> AlertDialog id state msg
description children (AlertDialog config) =
    AlertDialog { config | description = children }


cancel : Button shape Button.Ready msg -> AlertDialog id state msg -> AlertDialog id state msg
cancel button (AlertDialog config) =
    AlertDialog { config | cancel = Just (\attrs -> Button.toHtml attrs button) }


action : msg -> Button shape Button.Ready msg -> AlertDialog id state msg -> AlertDialog id state msg
action msg button (AlertDialog config) =
    AlertDialog { config | action = Just (\attrs -> Button.toHtml (attrs ++ [ Html.Events.onClick msg ]) button) }


small : AlertDialog id state msg -> AlertDialog id state msg
small (AlertDialog config) =
    AlertDialog { config | size = Small }


busy : Bool -> AlertDialog id state msg -> AlertDialog id state msg
busy isBusy (AlertDialog config) =
    AlertDialog { config | busy = isBusy }


toHtml : List (Attribute msg) -> AlertDialog Ready Ready msg -> Html msg
toHtml attrs (AlertDialog config) =
    let
        titleId =
            config.id ++ "-title"

        descriptionId =
            config.id ++ "-description"

        hasMedia =
            isJust config.media

        hasHeader =
            hasMedia || not (List.isEmpty config.title && List.isEmpty config.description)

        hasFooter =
            isJust config.cancel || isJust config.action

        isDefault =
            config.size == Default

        onClose =
            List.filterMap identity [ config.onClose ]
    in
    Html.node "elmcn-dialog"
        ([ Html.Attributes.id config.id
         , attribute "data-slot" "alert-dialog"
         , class "contents"
         ]
            ++ (if config.open then
                    [ attribute "open" "" ]

                else
                    []
               )
        )
        [ Html.node "dialog"
            ([ attribute "role" "alertdialog"
             , attribute "aria-busy"
                (if config.busy then
                    "true"

                 else
                    "false"
                )
             , class "fixed inset-0 isolate z-50 m-0 hidden h-full max-h-none w-full max-w-none place-items-center overflow-hidden bg-transparent p-4 outline-none open:grid backdrop:bg-transparent"
             , class "[html:has(&:modal)]:overflow-hidden [html:has(&:modal)]:[scrollbar-gutter:stable]"
             , class "supports-[-webkit-touch-callout:none]:[html:has(&:modal)]:transition-[filter] supports-[-webkit-touch-callout:none]:[html:has(&:modal)]:duration-300 supports-[-webkit-touch-callout:none]:[html:has(&:modal)]:ease-[ease] supports-[-webkit-touch-callout:none]:[html:has(elmcn-dialog[open]>&:modal)]:[filter:blur(var(--blur-xs))_brightness(0.9)]"
             ]
                ++ (if List.isEmpty config.title then
                        []

                    else
                        [ attribute "aria-labelledby" titleId ]
                   )
                ++ (if List.isEmpty config.description then
                        []

                    else
                        [ attribute "aria-describedby" descriptionId ]
                   )
                ++ List.concatMap closeRequests onClose
            )
            [ Html.div
                [ attribute "data-slot" "alert-dialog-overlay"
                , attribute "aria-hidden" "true"
                , class "absolute inset-0 select-none not-supports-[-webkit-touch-callout:none]:bg-black/10"
                , class "opacity-0 transition-[opacity,backdrop-filter] duration-300 ease-[ease] [elmcn-dialog[open]>dialog>&]:opacity-100 starting:[elmcn-dialog[open]>dialog>&]:opacity-0 not-supports-[-webkit-touch-callout:none]:[backdrop-filter:blur(0)] not-supports-[-webkit-touch-callout:none]:[elmcn-dialog[open]>dialog>&]:[backdrop-filter:blur(var(--blur-xs))] not-supports-[-webkit-touch-callout:none]:starting:[elmcn-dialog[open]>dialog>&]:[backdrop-filter:blur(0)]"
                , class "[elmcn-dialog_elmcn-dialog>dialog>&]:bg-transparent [elmcn-dialog_elmcn-dialog[open]>dialog>&]:[backdrop-filter:none]"
                ]
                []
            , Html.div
                ([ attribute "data-slot" "alert-dialog-content"
                 , attribute "data-size"
                    (if isDefault then
                        "default"

                     else
                        "sm"
                    )
                 , class "relative grid w-full max-w-xs max-h-full gap-4 rounded-xl bg-popover p-4 text-popover-foreground ring-1 ring-foreground/10 outline-none"
                 , classList [ ( "sm:max-w-(--alert-dialog-width) [--alert-dialog-width:var(--container-sm)]", isDefault ) ]
                 , class "opacity-0 [scale:0.95] transition-[opacity,scale] duration-300 ease-[ease] [elmcn-dialog[open]>dialog>&]:opacity-100 [elmcn-dialog[open]>dialog>&]:[scale:1] starting:[elmcn-dialog[open]>dialog>&]:opacity-0 starting:[elmcn-dialog[open]>dialog>&]:[scale:0.95]"
                 ]
                    ++ attrs
                )
                (List.filterMap identity
                    [ if hasHeader then
                        Just
                            (Html.div
                                [ attribute "data-slot" "alert-dialog-header"
                                , class "grid place-items-center gap-1.5 text-center"
                                , class
                                    (case ( hasMedia, isDefault ) of
                                        ( False, _ ) ->
                                            "grid-rows-[auto_1fr]"

                                        ( True, False ) ->
                                            "grid-rows-[auto_auto_1fr] gap-x-4"

                                        ( True, True ) ->
                                            "grid-rows-[auto_auto_1fr] gap-x-4 sm:grid-rows-[auto_1fr]"
                                    )
                                , classList [ ( "sm:place-items-start sm:text-left", isDefault ) ]
                                ]
                                (List.filterMap identity
                                    [ Maybe.map (viewMedia isDefault) config.media
                                    , if List.isEmpty config.title then
                                        Nothing

                                      else
                                        Just
                                            (Html.h2
                                                [ Html.Attributes.id titleId
                                                , attribute "data-slot" "alert-dialog-title"
                                                , class "text-base font-medium"
                                                , classList [ ( "sm:col-start-2", hasMedia && isDefault ) ]
                                                ]
                                                config.title
                                            )
                                    , if List.isEmpty config.description then
                                        Nothing

                                      else
                                        Just
                                            (Html.p
                                                [ Html.Attributes.id descriptionId
                                                , attribute "data-slot" "alert-dialog-description"
                                                , class "text-sm text-balance text-muted-foreground md:text-pretty *:[a]:underline *:[a]:underline-offset-3 *:[a]:hover:text-foreground"
                                                ]
                                                config.description
                                            )
                                    ]
                                )
                            )

                      else
                        Nothing
                    , if hasFooter then
                        Just
                            (Html.div
                                [ attribute "data-slot" "alert-dialog-footer"
                                , class "-mx-4 -mb-4 gap-2 rounded-b-xl border-t bg-muted/50 p-4"
                                , class
                                    (if isDefault then
                                        "flex flex-col-reverse sm:flex-row sm:justify-end"

                                     else
                                        "grid grid-cols-2"
                                    )
                                ]
                                (List.filterMap identity
                                    [ Maybe.map (\toButton -> toButton (attribute "data-slot" "alert-dialog-cancel" :: tabindex 0 :: Html.Attributes.disabled config.busy :: List.map Html.Events.onClick onClose)) config.cancel
                                    , Maybe.map (\toButton -> toButton [ attribute "data-slot" "alert-dialog-action", tabindex 0, Html.Attributes.disabled config.busy ]) config.action
                                    ]
                                )
                            )

                      else
                        Nothing
                    ]
                )
            ]
        ]


closeRequests : msg -> List (Attribute msg)
closeRequests msg =
    [ Html.Events.custom "keydown"
        (Decode.field "key" Decode.string
            |> Decode.andThen
                (\key ->
                    if key == "Escape" then
                        Decode.succeed { message = msg, stopPropagation = True, preventDefault = True }

                    else
                        Decode.fail key
                )
        )
    , Html.Events.preventDefaultOn "cancel" (Decode.succeed ( msg, True ))
    , Html.Events.on "close"
        (Decode.at [ "target", "parentElement", "attributes", "open" ] Decode.value
            |> Decode.map (\_ -> msg)
        )
    ]


viewMedia : Bool -> ( List (Attribute msg), List (Html msg) ) -> Html msg
viewMedia isDefault ( attrs, children ) =
    Html.div
        ([ attribute "data-slot" "alert-dialog-media"
         , class "mb-2 inline-flex size-10 items-center justify-center rounded-md bg-(--alert-dialog-media-bg) [--alert-dialog-media-bg:var(--muted)] *:[svg:not([class*='size-'])]:size-6"
         , classList [ ( "sm:row-span-2", isDefault ) ]
         ]
            ++ attrs
        )
        children


isJust : Maybe a -> Bool
isJust maybe =
    case maybe of
        Just _ ->
            True

        Nothing ->
            False
