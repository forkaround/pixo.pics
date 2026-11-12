module UI.DropdownMenu exposing
    ( DropdownMenu
    , Item
    , NeedsId
    , Ready
    , alignCenter
    , alignEnd
    , checkboxItem
    , content
    , destructive
    , disabled
    , group
    , id
    , inset
    , item
    , label
    , left
    , new
    , radioItem
    , right
    , separator
    , shortcut
    , submenu
    , toHtml
    , top
    , trigger
    )

import Html exposing (Attribute, Html)
import Html.Attributes exposing (attribute, autofocus, class, type_)


type DropdownMenu id msg
    = DropdownMenu
        { id : String
        , trigger : Maybe ( List (Attribute msg), List (Html msg) )
        , content : List (Item msg)
        , side : Side
        , align : Align
        }


type Ready
    = Ready


type NeedsId
    = NeedsId


type Side
    = Top
    | Bottom
    | Left
    | Right


type Align
    = Start
    | Center
    | End


type Item msg
    = Item
        { kind : Kind
        , attrs : List (Attribute msg)
        , children : List (Html msg)
        , inset : Bool
        , disabled : Bool
        , destructive : Bool
        }
    | Submenu
        { children : List (Html msg)
        , items : List (Item msg)
        , inset : Bool
        , disabled : Bool
        }
    | Label Bool (List (Html msg))
    | Separator
    | Group (List (Item msg))


type Kind
    = Action
    | Checkbox Bool
    | Radio Bool



-- CONSTRUCTOR


new : DropdownMenu NeedsId msg
new =
    DropdownMenu
        { id = ""
        , trigger = Nothing
        , content = []
        , side = Bottom
        , align = Start
        }



-- REQUIRED


id : String -> DropdownMenu NeedsId msg -> DropdownMenu Ready msg
id string (DropdownMenu config) =
    DropdownMenu { config | id = string }



-- TRIGGER


trigger : List (Attribute msg) -> List (Html msg) -> DropdownMenu id msg -> DropdownMenu id msg
trigger attrs children (DropdownMenu config) =
    DropdownMenu { config | trigger = Just ( attrs, children ) }



-- CONTENT


content : List (Item msg) -> DropdownMenu id msg -> DropdownMenu id msg
content items (DropdownMenu config) =
    DropdownMenu { config | content = items }


item : List (Attribute msg) -> List (Html msg) -> Item msg
item attrs children =
    Item
        { kind = Action
        , attrs = attrs
        , children = children
        , inset = False
        , disabled = False
        , destructive = False
        }


checkboxItem : Bool -> List (Attribute msg) -> List (Html msg) -> Item msg
checkboxItem checked attrs children =
    Item
        { kind = Checkbox checked
        , attrs = attrs
        , children = children
        , inset = False
        , disabled = False
        , destructive = False
        }


radioItem : Bool -> List (Attribute msg) -> List (Html msg) -> Item msg
radioItem checked attrs children =
    Item
        { kind = Radio checked
        , attrs = attrs
        , children = children
        , inset = False
        , disabled = False
        , destructive = False
        }


submenu : List (Html msg) -> List (Item msg) -> Item msg
submenu children items =
    Submenu
        { children = children
        , items = items
        , inset = False
        , disabled = False
        }


destructive : Item msg -> Item msg
destructive entry =
    case entry of
        Item config ->
            Item { config | destructive = True }

        _ ->
            entry


inset : Item msg -> Item msg
inset entry =
    case entry of
        Item config ->
            Item { config | inset = True }

        Submenu config ->
            Submenu { config | inset = True }

        Label _ children ->
            Label True children

        _ ->
            entry


disabled : Item msg -> Item msg
disabled entry =
    case entry of
        Item config ->
            Item { config | disabled = True }

        Submenu config ->
            Submenu { config | disabled = True }

        _ ->
            entry


group : List (Item msg) -> Item msg
group =
    Group


label : List (Html msg) -> Item msg
label =
    Label False


separator : Item msg
separator =
    Separator


shortcut : List (Html msg) -> Html msg
shortcut children =
    Html.span
        [ attribute "data-slot" "dropdown-menu-shortcut"
        , class "ms-auto text-xs tracking-widest text-muted-foreground"
        ]
        children



-- PLACEMENT


top : DropdownMenu id msg -> DropdownMenu id msg
top =
    side Top


left : DropdownMenu id msg -> DropdownMenu id msg
left =
    side Left


right : DropdownMenu id msg -> DropdownMenu id msg
right =
    side Right


side : Side -> DropdownMenu id msg -> DropdownMenu id msg
side s (DropdownMenu config) =
    DropdownMenu { config | side = s }


alignCenter : DropdownMenu id msg -> DropdownMenu id msg
alignCenter (DropdownMenu config) =
    DropdownMenu { config | align = Center }


alignEnd : DropdownMenu id msg -> DropdownMenu id msg
alignEnd (DropdownMenu config) =
    DropdownMenu { config | align = End }



-- RENDERING


toHtml : List (Attribute msg) -> DropdownMenu Ready msg -> Html msg
toHtml attrs (DropdownMenu config) =
    let
        menuId =
            config.id ++ "-menu"

        anchor =
            "--dropdown-menu-" ++ config.id
    in
    Html.span
        [ attribute "data-slot" "dropdown-menu", class "contents" ]
        [ case config.trigger of
            Just ( triggerAttrs, children ) ->
                Html.button
                    ([ type_ "button"
                     , Html.Attributes.id config.id
                     , attribute "popovertarget" menuId
                     , attribute "aria-haspopup" "menu"
                     , attribute "data-slot" "dropdown-menu-trigger"
                     , attribute "style" ("anchor-name: " ++ anchor)
                     ]
                        ++ triggerAttrs
                    )
                    children

            Nothing ->
                Html.text ""
        , Html.div
            ([ Html.Attributes.id menuId
             , attribute "popover" "auto"
             , attribute "role" "menu"
             , attribute "aria-labelledby" config.id
             , attribute "focusgroup" "menu block wrap"
             , attribute "data-slot" "dropdown-menu-content"
             , attribute "data-side" (sideName config.side)
             , attribute "data-align" (alignName config.align)
             , attribute "style" ("position-anchor: " ++ anchor ++ "; " ++ placement config.side config.align ++ "; " ++ minHeight config.content)
             , class "min-w-[max(--spacing(32),anchor-size(width))]"
             , class (menuClass config.side)
             ]
                ++ attrs
            )
            (viewItems menuId menuId config.content)
        ]


menuClass : Side -> String
menuClass s =
    String.join " "
        [ "group/menu fixed isolate z-50 m-0 inset-auto max-h-[calc(100%-0.5rem)] overflow-x-hidden overflow-y-auto rounded-[0.875rem] border-0 p-1.5 text-sm text-popover-foreground shadow-lg ring-1 ring-foreground/10 outline-hidden"
        , "scale-95 opacity-0 ease-out open:scale-100 open:translate-0 open:opacity-100 starting:open:scale-95 starting:open:opacity-0"
        , slideClass s
        , "[transition:opacity_100ms,scale_100ms,translate_100ms,display_100ms_allow-discrete,overlay_100ms_allow-discrete]"
        , "bg-popover/80 backdrop-blur-xl backdrop-saturate-150"
        , "backdrop:hidden"
        ]


viewItems : String -> String -> List (Item msg) -> List (Html msg)
viewItems rootId prefix items =
    let
        first =
            items
                |> List.concatMap focusable
                |> List.indexedMap Tuple.pair
                |> List.filter Tuple.second
                |> List.head
                |> Maybe.map Tuple.first
    in
    viewList rootId prefix first 0 items
        |> Tuple.first


focusable : Item msg -> List Bool
focusable entry =
    case entry of
        Item config ->
            [ not config.disabled ]

        Submenu config ->
            [ not config.disabled ]

        Group items ->
            List.concatMap focusable items

        _ ->
            []


viewList : String -> String -> Maybe Int -> Int -> List (Item msg) -> ( List (Html msg), Int )
viewList rootId prefix first index items =
    case items of
        [] ->
            ( [], index )

        entry :: rest ->
            let
                ( html, next ) =
                    viewItem rootId prefix first index entry

                ( htmls, final ) =
                    viewList rootId prefix first next rest
            in
            ( html :: htmls, final )


viewItem : String -> String -> Maybe Int -> Int -> Item msg -> ( Html msg, Int )
viewItem rootId prefix first index entry =
    let
        isFirst =
            first == Just index

        insetClass isInset =
            class
                (if isInset then
                    "ps-8"

                 else
                    "ps-2"
                )
    in
    case entry of
        Item config ->
            ( Html.button
                ([ type_ "button"
                 , attribute "role" (roleName config.kind)
                 , attribute "data-slot" (slotName config.kind)
                 , Html.Attributes.disabled config.disabled
                 , autofocus isFirst
                 , class itemClass
                 , insetClass config.inset
                 ]
                    ++ (case config.kind of
                            Action ->
                                [ attribute "popovertarget" rootId
                                , attribute "popovertargetaction" "hide"
                                , class "pe-2"
                                ]

                            Checkbox checked ->
                                [ attribute "aria-checked" (boolean checked), class "pe-8" ]

                            Radio checked ->
                                [ attribute "aria-checked" (boolean checked), class "pe-8" ]
                       )
                    ++ (if config.destructive then
                            [ attribute "data-variant" "destructive"
                            , class "data-[variant=destructive]:text-destructive data-[variant=destructive]:hover:bg-destructive/10 data-[variant=destructive]:focus:bg-destructive/10 dark:data-[variant=destructive]:hover:bg-destructive/20 dark:data-[variant=destructive]:focus:bg-destructive/20 data-[variant=destructive]:*:[svg]:text-destructive"
                            ]

                        else
                            [ attribute "data-variant" "default" ]
                       )
                    ++ config.attrs
                )
                (config.children
                    ++ (case config.kind of
                            Action ->
                                []

                            Checkbox checked ->
                                [ indicator (slotName config.kind ++ "-indicator") checked ]

                            Radio checked ->
                                [ indicator (slotName config.kind ++ "-indicator") checked ]
                       )
                )
            , index + 1
            )

        Submenu config ->
            let
                subId =
                    prefix ++ "-sub-" ++ String.fromInt index

                subAnchor =
                    "--" ++ subId
            in
            ( Html.div
                [ attribute "data-slot" "dropdown-menu-sub", class "contents" ]
                [ Html.button
                    [ type_ "button"
                    , attribute "role" "menuitem"
                    , attribute "aria-haspopup" "menu"
                    , attribute "popovertarget" subId
                    , attribute "data-slot" "dropdown-menu-sub-trigger"
                    , attribute "style" ("anchor-name: " ++ subAnchor)
                    , Html.Attributes.disabled config.disabled
                    , autofocus isFirst
                    , class itemClass
                    , class "pe-2 has-[+:popover-open]:bg-foreground/10"
                    , insetClass config.inset
                    ]
                    (config.children
                        ++ [ Html.span
                                [ attribute "aria-hidden" "true"
                                , class "pointer-events-none ms-auto flex size-4 items-center justify-center rtl:-scale-x-100"
                                ]
                                [ Html.span [ class "me-1 size-2 rotate-45 border-t-[1.5px] border-r-[1.5px] border-current" ] [] ]
                           ]
                    )
                , Html.div
                    [ Html.Attributes.id subId
                    , attribute "popover" "auto"
                    , attribute "role" "menu"
                    , attribute "focusgroup" "menu block wrap"
                    , attribute "data-slot" "dropdown-menu-sub-content"
                    , attribute "data-side" "right"
                    , attribute "style" ("position-anchor: " ++ subAnchor ++ "; " ++ submenuPlacement ++ "; " ++ minHeight config.items)
                    , class "min-w-24 shadow-lg [position-area:right_span-bottom] rtl:[position-area:left_span-bottom]"
                    , class (menuClass Right)
                    ]
                    (viewItems rootId subId config.items)
                ]
            , index + 1
            )

        Label isInset children ->
            ( Html.div
                [ attribute "data-slot" "dropdown-menu-label"
                , class "py-1 pe-2 text-xs font-medium text-muted-foreground"
                , insetClass isInset
                ]
                children
            , index
            )

        Separator ->
            ( Html.div
                [ attribute "data-slot" "dropdown-menu-separator"
                , attribute "role" "separator"
                , class "pointer-events-none -mx-1.5 my-1.5 h-px bg-border"
                ]
                []
            , index
            )

        Group items ->
            let
                ( htmls, next ) =
                    viewList rootId prefix first index items
            in
            ( Html.div
                [ attribute "data-slot" "dropdown-menu-group"
                , attribute "role" "group"
                ]
                htmls
            , next
            )


itemClass : String
itemClass =
    "relative flex w-full cursor-default items-center gap-2 rounded-lg py-2 text-start text-sm font-medium text-muted-foreground outline-hidden select-none hover:bg-foreground/10 hover:text-foreground focus:bg-foreground/10 focus:text-foreground aria-checked:text-foreground group-hover/menu:not-hover:focus:bg-transparent group-hover/menu:not-hover:focus:not-aria-checked:text-muted-foreground disabled:pointer-events-none disabled:opacity-50 [&_svg]:pointer-events-none [&_svg]:shrink-0 [&_svg:not([class*='size-'])]:size-4"


indicator : String -> Bool -> Html msg
indicator slot checked =
    Html.span
        [ attribute "data-slot" slot
        , attribute "aria-hidden" "true"
        , class "pointer-events-none absolute end-2 flex size-4 items-center justify-center"
        , class
            (if checked then
                "visible"

             else
                "invisible"
            )
        ]
        [ Html.span [ class "mb-0.5 h-2.5 w-1.5 rotate-45 border-r-[1.5px] border-b-[1.5px] border-current" ] [] ]


roleName : Kind -> String
roleName kind =
    case kind of
        Action ->
            "menuitem"

        Checkbox _ ->
            "menuitemcheckbox"

        Radio _ ->
            "menuitemradio"


slotName : Kind -> String
slotName kind =
    case kind of
        Action ->
            "dropdown-menu-item"

        Checkbox _ ->
            "dropdown-menu-checkbox-item"

        Radio _ ->
            "dropdown-menu-radio-item"


boolean : Bool -> String
boolean b =
    if b then
        "true"

    else
        "false"



-- HEIGHT


type alias Length =
    { rem : Float, px : Float }


add : Length -> Length -> Length
add a b =
    { rem = a.rem + b.rem, px = a.px + b.px }


css : Length -> String
css l =
    "calc(" ++ String.fromFloat l.rem ++ "rem + " ++ String.fromFloat l.px ++ "px)"


menuPadding : Length
menuPadding =
    { rem = 0.75, px = 0 }


labelHeight : Length
labelHeight =
    { rem = 1.5, px = 0 }


itemHeight : Length
itemHeight =
    { rem = 2.25, px = 0 }


separatorHeight : Length
separatorHeight =
    { rem = 0.75, px = 1 }


minHeight : List (Item msg) -> String
minHeight items =
    "min-height: min(" ++ css (List.foldl (\entry total -> add total (height entry)) menuPadding items) ++ ", 10rem)"


height : Item msg -> Length
height entry =
    case entry of
        Item _ ->
            itemHeight

        Submenu _ ->
            itemHeight

        Label _ _ ->
            labelHeight

        Separator ->
            separatorHeight

        Group items ->
            List.foldl (\e total -> add total (height e)) { rem = 0, px = 0 } items



-- PLACEMENT


placement : Side -> Align -> String
placement s a =
    let
        area =
            case ( s, a ) of
                ( Top, Center ) ->
                    "top"

                ( Top, Start ) ->
                    "top span-right"

                ( Top, End ) ->
                    "top span-left"

                ( Bottom, Center ) ->
                    "bottom"

                ( Bottom, Start ) ->
                    "bottom span-right"

                ( Bottom, End ) ->
                    "bottom span-left"

                ( Left, Center ) ->
                    "left"

                ( Left, Start ) ->
                    "left span-bottom"

                ( Left, End ) ->
                    "left span-top"

                ( Right, Center ) ->
                    "right"

                ( Right, Start ) ->
                    "right span-bottom"

                ( Right, End ) ->
                    "right span-top"

        ( fallbacks, margin ) =
            case s of
                Top ->
                    ( "flip-block, flip-inline, flip-block flip-inline", "0.25rem 0" )

                Bottom ->
                    ( "flip-block, flip-inline, flip-block flip-inline", "0.25rem 0" )

                Left ->
                    ( "flip-inline, flip-block, flip-inline flip-block", "0 0.25rem" )

                Right ->
                    ( "flip-inline, flip-block, flip-inline flip-block", "0 0.25rem" )
    in
    "position-area: " ++ area ++ "; position-try-fallbacks: " ++ fallbacks ++ "; margin: " ++ margin


submenuPlacement : String
submenuPlacement =
    "position-try-fallbacks: flip-inline, flip-block, flip-inline flip-block; margin: -5px 0 0 0"


slideClass : Side -> String
slideClass s =
    case s of
        Top ->
            "origin-bottom translate-y-2 starting:open:translate-y-2"

        Bottom ->
            "origin-top -translate-y-2 starting:open:-translate-y-2"

        Left ->
            "origin-right translate-x-2 starting:open:translate-x-2"

        Right ->
            "origin-left -translate-x-2 starting:open:-translate-x-2"


sideName : Side -> String
sideName s =
    case s of
        Top ->
            "top"

        Bottom ->
            "bottom"

        Left ->
            "left"

        Right ->
            "right"


alignName : Align -> String
alignName a =
    case a of
        Start ->
            "start"

        Center ->
            "center"

        End ->
            "end"
