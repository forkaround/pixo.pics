module UI.Dialog exposing
    ( Dialog
    , NeedsId
    , NeedsOpen
    , Ready
    , content
    , description
    , footer
    , hideCloseButton
    , id
    , label
    , modal
    , new
    , open
    , title
    , toHtml
    )

import Html exposing (Attribute, Html)
import Html.Attributes exposing (attribute, class, tabindex)
import Html.Events
import Json.Decode as Decode
import UI.Button as Button


type Dialog id state msg
    = Dialog
        { id : String
        , open : Bool
        , onClose : Maybe msg
        , title : List (Html msg)
        , description : List (Html msg)
        , content : List (Html msg)
        , footer : Maybe ( List (Attribute msg), List (Html msg) )
        , closeButton : Bool
        , modal : Bool
        , label : Maybe String
        }


type Ready
    = Ready


type NeedsId
    = NeedsId


type NeedsOpen
    = NeedsOpen



-- CONSTRUCTOR


new : Dialog NeedsId NeedsOpen msg
new =
    Dialog
        { id = ""
        , open = False
        , onClose = Nothing
        , title = []
        , description = []
        , content = []
        , footer = Nothing
        , closeButton = True
        , modal = False
        , label = Nothing
        }



-- REQUIRED


id : String -> Dialog NeedsId state msg -> Dialog Ready state msg
id string (Dialog config) =
    Dialog { config | id = string }


open : Bool -> msg -> Dialog id NeedsOpen msg -> Dialog id Ready msg
open isOpen onClose (Dialog config) =
    Dialog { config | open = isOpen, onClose = Just onClose }



-- PARTS


title : List (Html msg) -> Dialog id state msg -> Dialog id state msg
title children (Dialog config) =
    Dialog { config | title = children }


description : List (Html msg) -> Dialog id state msg -> Dialog id state msg
description children (Dialog config) =
    Dialog { config | description = children }


content : List (Html msg) -> Dialog id state msg -> Dialog id state msg
content children (Dialog config) =
    Dialog { config | content = children }


footer : List (Attribute msg) -> List (Html msg) -> Dialog id state msg -> Dialog id state msg
footer attrs children (Dialog config) =
    Dialog { config | footer = Just ( attrs, children ) }



-- SETTERS


modal : Dialog id state msg -> Dialog id state msg
modal (Dialog config) =
    Dialog { config | modal = True }


label : String -> Dialog id state msg -> Dialog id state msg
label name (Dialog config) =
    Dialog { config | label = Just name }


hideCloseButton : Dialog id state msg -> Dialog id state msg
hideCloseButton (Dialog config) =
    Dialog { config | closeButton = False }



-- RENDERING


toHtml : List (Attribute msg) -> Dialog Ready Ready msg -> Html msg
toHtml attrs (Dialog config) =
    let
        titleId =
            config.id ++ "-title"

        descriptionId =
            config.id ++ "-description"

        hasHeader =
            not (List.isEmpty config.title && List.isEmpty config.description)

        onClose =
            List.filterMap identity [ config.onClose ]
    in
    Html.node "elmcn-dialog"
        ([ Html.Attributes.id config.id
         , attribute "data-slot" "dialog"
         , class "contents"
         ]
            ++ (if config.open then
                    [ attribute "open" "" ]

                else
                    []
               )
        )
        [ Html.node "dialog"
            ([ -- The dialog fills the viewport and centres the panel over the overlay.
               class "fixed inset-0 isolate z-50 m-0 hidden h-full max-h-none w-full max-w-none place-items-center overflow-hidden bg-transparent p-4 outline-none open:grid backdrop:bg-transparent"
             , class "[html:has(&:modal)]:overflow-hidden [html:has(&:modal)]:[scrollbar-gutter:stable]"
             , class "supports-[-webkit-touch-callout:none]:[html:has(&:modal)]:transition-[filter] supports-[-webkit-touch-callout:none]:[html:has(&:modal)]:duration-300 supports-[-webkit-touch-callout:none]:[html:has(&:modal)]:ease-[ease] supports-[-webkit-touch-callout:none]:[html:has(elmcn-dialog[open]>&:modal)]:[filter:blur(var(--blur-xs))_brightness(0.9)]"
             ]
                ++ (case ( config.title, config.label ) of
                        ( [], Just name ) ->
                            [ attribute "aria-label" name ]

                        ( [], Nothing ) ->
                            []

                        _ ->
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
                ([ attribute "data-slot" "dialog-overlay"
                 , attribute "aria-hidden" "true"
                 , class "absolute inset-0 select-none not-supports-[-webkit-touch-callout:none]:bg-black/10"
                 , class "opacity-0 transition-[opacity,backdrop-filter] duration-300 ease-[ease] [elmcn-dialog[open]>dialog>&]:opacity-100 starting:[elmcn-dialog[open]>dialog>&]:opacity-0 not-supports-[-webkit-touch-callout:none]:[backdrop-filter:blur(0)] not-supports-[-webkit-touch-callout:none]:[elmcn-dialog[open]>dialog>&]:[backdrop-filter:blur(var(--blur-xs))] not-supports-[-webkit-touch-callout:none]:starting:[elmcn-dialog[open]>dialog>&]:[backdrop-filter:blur(0)]"
                 , class "[elmcn-dialog_elmcn-dialog>dialog>&]:bg-transparent [elmcn-dialog_elmcn-dialog[open]>dialog>&]:[backdrop-filter:none]"
                 ]
                    ++ (if config.modal then
                            []

                        else
                            List.map Html.Events.onClick onClose
                       )
                )
                []
            , Html.div
                ([ attribute "data-slot" "dialog-content"
                 , class "relative grid w-full max-h-full gap-4 rounded-xl bg-popover p-4 text-sm text-popover-foreground ring-1 ring-foreground/10 outline-none sm:max-w-(--dialog-width) [--dialog-width:var(--container-sm)]"
                 , class "opacity-0 [scale:0.95] transition-[opacity,scale] duration-300 ease-[ease] [elmcn-dialog[open]>dialog>&]:opacity-100 [elmcn-dialog[open]>dialog>&]:[scale:1] starting:[elmcn-dialog[open]>dialog>&]:opacity-0 starting:[elmcn-dialog[open]>dialog>&]:[scale:0.95]"
                 ]
                    ++ attrs
                )
                (List.filterMap identity
                    [ if hasHeader then
                        Just
                            (Html.div
                                [ attribute "data-slot" "dialog-header"
                                , class "flex flex-col gap-2"
                                ]
                                (List.filterMap identity
                                    [ if List.isEmpty config.title then
                                        Nothing

                                      else
                                        Just
                                            (Html.h2
                                                [ Html.Attributes.id titleId
                                                , attribute "data-slot" "dialog-title"
                                                , class "text-base leading-none font-medium"
                                                ]
                                                config.title
                                            )
                                    , if List.isEmpty config.description then
                                        Nothing

                                      else
                                        Just
                                            (Html.p
                                                [ Html.Attributes.id descriptionId
                                                , attribute "data-slot" "dialog-description"
                                                , class "text-sm text-muted-foreground *:[a]:underline *:[a]:underline-offset-3 *:[a]:hover:text-foreground"
                                                ]
                                                config.description
                                            )
                                    ]
                                )
                            )

                      else
                        Nothing
                    ]
                    ++ config.content
                    ++ List.filterMap identity
                        [ Maybe.map viewFooter config.footer
                        , if config.closeButton then
                            Just (viewCloseButton onClose)

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


viewFooter : ( List (Attribute msg), List (Html msg) ) -> Html msg
viewFooter ( attrs, children ) =
    Html.div
        ([ attribute "data-slot" "dialog-footer"
         , class "-mx-4 -mb-4 flex flex-col-reverse gap-2 rounded-b-xl border-t bg-muted/50 p-4 sm:flex-row sm:justify-end"
         ]
            ++ attrs
        )
        children


viewCloseButton : List msg -> Html msg
viewCloseButton onClose =
    Button.new
        |> Button.icon
            (Html.span
                [ attribute "aria-hidden" "true"
                , class "relative flex size-4 items-center justify-center"
                ]
                [ Html.span [ class "absolute h-[1.333px] w-[12.667px] rotate-45 rounded-full bg-current" ] []
                , Html.span [ class "absolute h-[1.333px] w-[12.667px] -rotate-45 rounded-full bg-current" ] []
                ]
            )
        |> Button.label "Close"
        |> Button.ghost
        |> Button.small
        |> Button.toHtml
            (attribute "data-slot" "dialog-close"
                :: class "absolute top-2 end-2"
                :: tabindex 0
                :: List.map Html.Events.onClick onClose
            )
