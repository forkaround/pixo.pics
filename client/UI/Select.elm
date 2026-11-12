module UI.Select exposing
    ( Item
    , NeedsHandler
    , NeedsId
    , Ready
    , Select
    , alignToEdge
    , disabled
    , group
    , id
    , item
    , items
    , new
    , onSelect
    , placeholder
    , separator
    , small
    , toHtml
    , value
    )

import Html exposing (Attribute, Html)
import Html.Attributes exposing (attribute, autofocus, class, type_)
import Html.Events exposing (onClick)


type Select a id handler msg
    = Select
        { id : String
        , onSelect : Maybe (a -> msg)
        , placeholder : String
        , value : Maybe a
        , size : Size
        , align : Align
        , sections : List (Section a msg)
        }


type Ready
    = Ready


type NeedsId
    = NeedsId


type NeedsHandler
    = NeedsHandler


type Item a msg
    = Item
        { value : a
        , label : List (Html msg)
        , disabled : Bool
        }


type Section a msg
    = Group (Maybe String) (List (Item a msg))
    | Separator


type Size
    = Medium
    | Small


type Align
    = SelectedItem
    | Edge



-- CONSTRUCTOR


new : Select a NeedsId NeedsHandler msg
new =
    Select
        { id = ""
        , onSelect = Nothing
        , placeholder = ""
        , value = Nothing
        , size = Medium
        , align = SelectedItem
        , sections = []
        }



-- REQUIRED


id : String -> Select a NeedsId handler msg -> Select a Ready handler msg
id string (Select config) =
    Select { config | id = string }


onSelect : (a -> msg) -> Select a id NeedsHandler msg -> Select a id Ready msg
onSelect toMsg (Select config) =
    Select { config | onSelect = Just toMsg }



-- ITEMS


item : a -> List (Html msg) -> Item a msg
item v label =
    Item { value = v, label = label, disabled = False }


disabled : Item a msg -> Item a msg
disabled (Item config) =
    Item { config | disabled = True }


items : List (Item a msg) -> Select a id handler msg -> Select a id handler msg
items list (Select config) =
    Select { config | sections = config.sections ++ [ Group Nothing list ] }


group : String -> List (Item a msg) -> Select a id handler msg -> Select a id handler msg
group label list (Select config) =
    Select { config | sections = config.sections ++ [ Group (Just label) list ] }


separator : Select a id handler msg -> Select a id handler msg
separator (Select config) =
    Select { config | sections = config.sections ++ [ Separator ] }



-- VALUE


placeholder : String -> Select a id handler msg -> Select a id handler msg
placeholder string (Select config) =
    Select { config | placeholder = string }


value : Maybe a -> Select a id handler msg -> Select a id handler msg
value v (Select config) =
    Select { config | value = v }



-- SETTERS


small : Select a id handler msg -> Select a id handler msg
small (Select config) =
    Select { config | size = Small }


alignToEdge : Select a id handler msg -> Select a id handler msg
alignToEdge (Select config) =
    Select { config | align = Edge }



-- RENDERING


toHtml : List (Attribute msg) -> Select a Ready Ready msg -> Html msg
toHtml attrs (Select config) =
    let
        popoverId =
            config.id ++ "-popover"

        anchor =
            "--select-" ++ config.id

        selected =
            config.sections
                |> List.concatMap sectionItems
                |> List.filter (\(Item i) -> Just i.value == config.value)
                |> List.head

        focused =
            case selected of
                Just (Item i) ->
                    Just i.value

                Nothing ->
                    config.sections
                        |> List.concatMap sectionItems
                        |> List.filter (\(Item i) -> not i.disabled)
                        |> List.head
                        |> Maybe.map (\(Item i) -> i.value)
    in
    Html.span
        [ attribute "data-slot" "select", class "contents" ]
        [ Html.button
            ([ type_ "button"
             , Html.Attributes.id config.id
             , attribute "popovertarget" popoverId
             , attribute "data-slot" "select-trigger"
             , attribute "data-size" (sizeName config.size)
             , attribute "style" ("anchor-name: " ++ anchor)
             , class "inline-flex items-center justify-between gap-1.5 rounded-lg border border-input bg-transparent py-2 ps-2.5 pe-2 text-sm whitespace-nowrap transition-colors outline-none select-none disabled:cursor-not-allowed disabled:opacity-50 data-[size=default]:h-8 data-[size=sm]:h-7 data-[size=sm]:rounded-[min(var(--radius-md),10px)] data-placeholder:text-muted-foreground dark:bg-input/30 dark:hover:bg-input/50 *:data-[slot=select-value]:line-clamp-1 *:data-[slot=select-value]:flex *:data-[slot=select-value]:items-center *:data-[slot=select-value]:gap-1.5 [&_svg]:pointer-events-none [&_svg]:shrink-0 [&_svg:not([class*='size-'])]:size-4"
             , class "focus-visible:border-ring focus-visible:ring-3 focus-visible:ring-ring/50"
             , class "aria-invalid:border-destructive aria-invalid:ring-3 aria-invalid:ring-destructive/20 dark:aria-invalid:border-destructive/50 dark:aria-invalid:ring-destructive/40"
             ]
                ++ (case selected of
                        Nothing ->
                            [ attribute "data-placeholder" "" ]

                        Just _ ->
                            []
                   )
                ++ attrs
            )
            [ Html.span [ attribute "data-slot" "select-value", class "flex flex-1 text-start" ]
                (case selected of
                    Just (Item i) ->
                        i.label

                    Nothing ->
                        [ Html.text config.placeholder ]
                )
            , Html.span
                [ attribute "data-slot" "select-trigger-icon"
                , attribute "aria-hidden" "true"
                , class "pointer-events-none flex size-4 items-center justify-center text-muted-foreground"
                ]
                [ Html.span [ class "mb-1 size-2 rotate-45 border-r-[1.5px] border-b-[1.5px] border-current" ] [] ]
            ]
        , Html.div
            [ Html.Attributes.id popoverId
            , attribute "popover" "auto"
            , attribute "role" "listbox"
            , attribute "aria-labelledby" config.id
            , attribute "focusgroup" "listbox block"
            , attribute "data-slot" "select-content"
            , attribute "style" ("position-anchor: " ++ anchor ++ "; " ++ placement config selected)
            , class "group/list fixed isolate z-50 m-0 inset-auto w-[anchor-size(width)] max-h-[calc(100%-0.5rem)] overflow-x-hidden overflow-y-auto rounded-lg border-0 p-0 text-popover-foreground shadow-md ring-1 ring-foreground/10"
            , class "origin-top scale-95 opacity-0 ease-out open:scale-100 open:opacity-100 starting:open:scale-95 starting:open:opacity-0"
            , class "[transition:opacity_100ms,scale_100ms,display_100ms_allow-discrete,overlay_100ms_allow-discrete,top_0s_var(--placement-delay)_allow-discrete,bottom_0s_var(--placement-delay)_allow-discrete,height_0s_var(--placement-delay)_allow-discrete,max-height_0s_var(--placement-delay)_allow-discrete,min-height_0s_var(--placement-delay)_allow-discrete,margin_0s_var(--placement-delay)_allow-discrete,position-area_0s_var(--placement-delay)_allow-discrete,position-try-fallbacks_0s_var(--placement-delay)_allow-discrete,scroll-snap-type_0s_var(--snap-delay)_allow-discrete]"
            , class "[--placement-delay:100ms] open:[--placement-delay:0s]"
            , class "snap-y snap-mandatory [--snap-delay:90ms] open:snap-none open:[--snap-delay:0s]"
            , class "bg-popover/70 backdrop-blur-2xl backdrop-saturate-150"
            , class "backdrop:hidden"
            ]
            (List.indexedMap (viewSection popoverId config.onSelect config.value focused) config.sections)
        ]


sectionItems : Section a msg -> List (Item a msg)
sectionItems section =
    case section of
        Group _ list ->
            list

        Separator ->
            []


viewSection : String -> Maybe (a -> msg) -> Maybe a -> Maybe a -> Int -> Section a msg -> Html msg
viewSection popoverId handler current focused index section =
    let
        scrollReset =
            if index == 0 then
                [ class "snap-start" ]

            else
                []
    in
    case section of
        Group label list ->
            Html.div
                ([ attribute "data-slot" "select-group"
                 , attribute "role" "group"
                 , class "scroll-my-1 p-1"
                 ]
                    ++ scrollReset
                )
                ((case label of
                    Just string ->
                        [ Html.div
                            [ attribute "data-slot" "select-label"
                            , class "px-1.5 py-1 text-xs text-muted-foreground"
                            ]
                            [ Html.text string ]
                        ]

                    Nothing ->
                        []
                 )
                    ++ List.map (viewItem popoverId handler current focused) list
                )

        Separator ->
            Html.div
                ([ attribute "data-slot" "select-separator"
                 , attribute "role" "separator"
                 , class "pointer-events-none my-1 h-px bg-border"
                 ]
                    ++ scrollReset
                )
                []


viewItem : String -> Maybe (a -> msg) -> Maybe a -> Maybe a -> Item a msg -> Html msg
viewItem popoverId handler current focused (Item config) =
    let
        isSelected =
            Just config.value == current
    in
    Html.button
        ([ type_ "button"
         , attribute "role" "option"
         , attribute "aria-selected" (boolean isSelected)
         , attribute "data-slot" "select-item"
         , attribute "popovertarget" popoverId
         , attribute "popovertargetaction" "hide"
         , Html.Attributes.disabled config.disabled
         , autofocus (focused == Just config.value)
         , class "relative flex w-full cursor-default items-center gap-1.5 rounded-md py-1 ps-1.5 pe-8 text-start text-sm outline-hidden select-none hover:bg-foreground/10 focus:bg-foreground/10 group-hover/list:not-hover:focus:bg-transparent disabled:pointer-events-none disabled:opacity-50 [&_svg]:pointer-events-none [&_svg]:shrink-0 [&_svg:not([class*='size-'])]:size-4"
         ]
            ++ (case handler of
                    Just toMsg ->
                        [ onClick (toMsg config.value) ]

                    Nothing ->
                        []
               )
        )
        [ Html.span [ attribute "data-slot" "select-item-text", class "flex flex-1 gap-2 whitespace-nowrap" ] config.label
        , Html.span
            [ attribute "data-slot" "select-item-indicator"
            , attribute "aria-hidden" "true"
            , class "pointer-events-none absolute end-2 flex size-4 items-center justify-center"
            , class
                (if isSelected then
                    "visible"

                 else
                    "invisible"
                )
            ]
            [ Html.span [ class "mb-0.5 h-2.5 w-1.5 rotate-45 border-r-[1.5px] border-b-[1.5px] border-current" ] [] ]
        ]



-- PLACEMENT


type alias Length =
    { rem : Float, px : Float }


add : Length -> Length -> Length
add a b =
    { rem = a.rem + b.rem, px = a.px + b.px }


zero : Length
zero =
    { rem = 0, px = 0 }


times : Float -> Length -> Length
times n l =
    { rem = n * l.rem, px = n * l.px }


css : Length -> String
css l =
    "calc(" ++ String.fromFloat l.rem ++ "rem + " ++ String.fromFloat l.px ++ "px)"


groupPadding : Length
groupPadding =
    { rem = 0.25, px = 0 }


labelHeight : Length
labelHeight =
    { rem = 1.5, px = 0 }


itemHeight : Length
itemHeight =
    { rem = 1.75, px = 0 }


separatorHeight : Length
separatorHeight =
    { rem = 0.5, px = 1 }


triggerHeight : Size -> Length
triggerHeight size =
    case size of
        Medium ->
            { rem = 2, px = 0 }

        Small ->
            { rem = 1.75, px = 0 }


sectionHeight : Section a msg -> Length
sectionHeight section =
    case section of
        Group label list ->
            times 2 groupPadding
                |> add (Maybe.map (\_ -> labelHeight) label |> Maybe.withDefault zero)
                |> add (times (toFloat (List.length list)) itemHeight)

        Separator ->
            separatorHeight


offsetOf : Maybe a -> List (Section a msg) -> Maybe Length
offsetOf current sections =
    case sections of
        [] ->
            Nothing

        (Group label list) :: rest ->
            let
                index =
                    list
                        |> List.indexedMap Tuple.pair
                        |> List.filter (\( _, Item i ) -> Just i.value == current)
                        |> List.head
                        |> Maybe.map Tuple.first
            in
            case index of
                Just n ->
                    Just
                        (groupPadding
                            |> add (Maybe.map (\_ -> labelHeight) label |> Maybe.withDefault zero)
                            |> add (times (toFloat n) itemHeight)
                        )

                Nothing ->
                    Maybe.map (add (sectionHeight (Group label list))) (offsetOf current rest)

        Separator :: rest ->
            Maybe.map (add separatorHeight) (offsetOf current rest)


placement :
    { config | sections : List (Section a msg), value : Maybe a, size : Size, align : Align }
    -> Maybe (Item a msg)
    -> String
placement config selected =
    let
        height =
            List.foldl (\section total -> add total (sectionHeight section)) zero config.sections

        offset =
            case ( config.align, selected ) of
                ( SelectedItem, Just _ ) ->
                    offsetOf config.value config.sections

                _ ->
                    Nothing
    in
    case offset of
        Just itemTop ->
            let
                shift =
                    times 0.5 (add (triggerHeight config.size) (times -1 itemHeight))
            in
            "top: clamp(0.25rem, anchor(top) - "
                ++ css itemTop
                ++ " + "
                ++ css shift
                ++ ", max(0.25rem, 100% - 0.25rem - min("
                ++ css height
                ++ ", 20rem))); bottom: 0.25rem; left: anchor(left); height: auto; max-height: "
                ++ css height

        Nothing ->
            "position-area: bottom; position-try-fallbacks: top; margin: 0.25rem 0; min-height: min(" ++ css height ++ ", 10rem)"


boolean : Bool -> String
boolean b =
    if b then
        "true"

    else
        "false"


sizeName : Size -> String
sizeName size =
    case size of
        Medium ->
            "default"

        Small ->
            "sm"
