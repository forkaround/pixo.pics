module UI.Sidebar exposing
    ( NeedsState
    , Ready
    , Sidebar
    , State
    , Trigger
    , active
    , collapsesToIcons
    , collapsible
    , content
    , defaultOpen
    , floating
    , footer
    , group
    , groupAction
    , groupContent
    , groupLabel
    , groupTrigger
    , header
    , init
    , inset
    , large
    , menu
    , menuAction
    , menuActionAttributes
    , menuBadge
    , menuButton
    , menuButtonAttributes
    , menuItem
    , menuLink
    , menuSkeleton
    , menuSub
    , menuSubButton
    , menuSubItem
    , menuSubLink
    , menuTrigger
    , mobileWidth
    , neverCollapses
    , new
    , offcanvas
    , outline
    , page
    , rail
    , right
    , separator
    , showIcon
    , showOnHover
    , small
    , state
    , toHtml
    , tooltip
    , trigger
    , width
    )

import Html exposing (Attribute, Html)
import Html.Attributes exposing (attribute, class, property, tabindex, type_)
import Html.Events
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Phosphor exposing (IconWeight(..))
import UI.Button as Button
import UI.Skeleton as Skeleton


type Sidebar state msg
    = Sidebar
        { state : State
        , onChange : Maybe (State -> msg)
        , header : List (Html msg)
        , content : List (Html msg)
        , footer : List (Html msg)
        , rail : Bool
        , page : Maybe ( List (Attribute msg), List (Html msg) )
        , side : Side
        , variant : Variant
        , collapsible : Collapsible
        , width : Maybe String
        , mobileWidth : Maybe String
        }


type alias State =
    { open : Bool
    , openMobile : Bool
    }


init : State
init =
    { open = True, openMobile = False }


type Side
    = Left
    | Right


type Variant
    = Default
    | Floating
    | Inset


type Collapsible
    = Offcanvas
    | Icon
    | None


type Ready
    = Ready


type NeedsState
    = NeedsState



-- CONSTRUCTOR


new : Sidebar NeedsState msg
new =
    Sidebar
        { state = init
        , onChange = Nothing
        , header = []
        , content = []
        , footer = []
        , rail = False
        , page = Nothing
        , side = Left
        , variant = Default
        , collapsible = Offcanvas
        , width = Nothing
        , mobileWidth = Nothing
        }



-- REQUIRED


state : State -> (State -> msg) -> Sidebar NeedsState msg -> Sidebar Ready msg
state current onChange (Sidebar config) =
    Sidebar { config | state = current, onChange = Just onChange }



-- PARTS


header : List (Html msg) -> Sidebar state msg -> Sidebar state msg
header children (Sidebar config) =
    Sidebar { config | header = children }


content : List (Html msg) -> Sidebar state msg -> Sidebar state msg
content children (Sidebar config) =
    Sidebar { config | content = children }


footer : List (Html msg) -> Sidebar state msg -> Sidebar state msg
footer children (Sidebar config) =
    Sidebar { config | footer = children }


rail : Sidebar state msg -> Sidebar state msg
rail (Sidebar config) =
    Sidebar { config | rail = True }


page : List (Attribute msg) -> List (Html msg) -> Sidebar state msg -> Sidebar state msg
page attrs children (Sidebar config) =
    Sidebar { config | page = Just ( attrs, children ) }



-- SIDE AND VARIANTS


right : Sidebar state msg -> Sidebar state msg
right (Sidebar config) =
    Sidebar { config | side = Right }


floating : Sidebar state msg -> Sidebar state msg
floating (Sidebar config) =
    Sidebar { config | variant = Floating }


inset : Sidebar state msg -> Sidebar state msg
inset (Sidebar config) =
    Sidebar { config | variant = Inset }



-- COLLAPSING


offcanvas : Sidebar state msg -> Sidebar state msg
offcanvas (Sidebar config) =
    Sidebar { config | collapsible = Offcanvas }


collapsesToIcons : Sidebar state msg -> Sidebar state msg
collapsesToIcons (Sidebar config) =
    Sidebar { config | collapsible = Icon }


neverCollapses : Sidebar state msg -> Sidebar state msg
neverCollapses (Sidebar config) =
    Sidebar { config | collapsible = None }



-- SETTERS


width : String -> Sidebar state msg -> Sidebar state msg
width value (Sidebar config) =
    Sidebar { config | width = Just value }


mobileWidth : String -> Sidebar state msg -> Sidebar state msg
mobileWidth value (Sidebar config) =
    Sidebar { config | mobileWidth = Just value }



-- RENDERING


toHtml : List (Attribute msg) -> Sidebar Ready msg -> Html msg
toHtml attrs (Sidebar config) =
    Html.div
        ([ attribute "data-slot" "sidebar-wrapper"
         , attribute "style"
            ("--sidebar-width: "
                ++ Maybe.withDefault "16rem" config.width
                ++ "; --sidebar-width-mobile: "
                ++ Maybe.withDefault "18rem" config.mobileWidth
                ++ "; --sidebar-width-icon: 3.5rem"
            )
         , class "group/sidebar-wrapper flex min-h-svh w-full overflow-x-clip"
         , class
            (case config.variant of
                Inset ->
                    "md:bg-sidebar"

                _ ->
                    ""
            )
         ]
            ++ attrs
        )
        (case config.side of
            Left ->
                [ viewSidebar config, viewMain config ]

            Right ->
                [ viewMain config, viewSidebar config ]
        )


type alias Config msg =
    { state : State
    , onChange : Maybe (State -> msg)
    , header : List (Html msg)
    , content : List (Html msg)
    , footer : List (Html msg)
    , rail : Bool
    , page : Maybe ( List (Attribute msg), List (Html msg) )
    , side : Side
    , variant : Variant
    , collapsible : Collapsible
    , width : Maybe String
    , mobileWidth : Maybe String
    }


viewMain : Config msg -> Html msg
viewMain config =
    case config.page of
        Nothing ->
            Html.text ""

        Just ( attrs, children ) ->
            Html.main_
                ([ attribute "data-slot" "sidebar-inset"
                 , class "relative flex w-full flex-1 flex-col bg-background"
                 , class
                    (case config.variant of
                        Inset ->
                            "md:m-2 md:rounded-xl md:shadow-sm "
                                ++ (case config.side of
                                        Left ->
                                            if config.state.open || config.collapsible == None then
                                                "md:ms-0"

                                            else
                                                "md:ms-2"

                                        Right ->
                                            if config.state.open || config.collapsible == None then
                                                "md:me-0"

                                            else
                                                "md:me-2"
                                   )

                        _ ->
                            ""
                    )
                 ]
                    ++ attrs
                )
                children


viewSidebar : Config msg -> Html msg
viewSidebar config =
    let
        parts =
            List.filterMap identity
                [ if List.isEmpty config.header then
                    Nothing

                  else
                    Just (Html.div [ attribute "data-slot" "sidebar-header", attribute "data-sidebar" "header", class "flex flex-col gap-2 px-3 py-2" ] config.header)
                , Just
                    (Html.div
                        [ attribute "data-slot" "sidebar-content"
                        , attribute "data-sidebar" "content"
                        , class "scrollbar-none flex min-h-0 flex-1 flex-col gap-0 overflow-auto md:group-data-[collapsible=icon]:overflow-hidden"
                        ]
                        config.content
                    )
                , if List.isEmpty config.footer then
                    Nothing

                  else
                    Just (Html.div [ attribute "data-slot" "sidebar-footer", attribute "data-sidebar" "footer", class "flex flex-col gap-2 p-3" ] config.footer)
                ]
    in
    case config.collapsible of
        None ->
            Html.div
                [ attribute "data-slot" "sidebar"
                , attribute "data-side" (sideName config.side)
                , attribute "data-variant" (variantName config.variant)
                , attribute "data-state" "expanded"
                , attribute "data-collapsible" ""
                , class "group peer flex h-full w-(--sidebar-width) shrink-0 flex-col bg-sidebar text-sidebar-foreground"
                ]
                parts

        _ ->
            viewCollapsible config parts


viewCollapsible : Config msg -> List (Html msg) -> Html msg
viewCollapsible config parts =
    let
        collapsed =
            not config.state.open

        padded =
            config.variant /= Default

        closeMobile =
            List.filterMap identity
                [ Maybe.map (\onChange -> onChange { open = config.state.open, openMobile = False }) config.onChange ]

        gapClass =
            String.join " "
                [ "relative flex h-full w-0 bg-transparent transition-[width] duration-200 ease-linear md:w-(--sidebar-width)"
                , case config.side of
                    Left ->
                        "justify-start"

                    Right ->
                        "justify-end"
                , if collapsed then
                    case config.collapsible of
                        Icon ->
                            if padded then
                                "md:w-[calc(var(--sidebar-width-icon)+(--spacing(4)))]"

                            else
                                "md:w-(--sidebar-width-icon)"

                        _ ->
                            "md:w-0"

                  else
                    ""
                ]

        containerClass =
            String.join " "
                [ "fixed inset-0 z-50 m-0 hidden h-full max-h-none w-full max-w-none border-0 bg-transparent p-0 text-inherit outline-none open:block backdrop:bg-transparent"
                , "md:sticky md:inset-auto md:top-0 md:z-20 md:flex md:max-h-svh md:w-(--sidebar-width) md:shrink-0 md:transition-[translate,width] md:duration-200 md:ease-linear"
                , "[html:has(&:modal)]:overflow-hidden [html:has(&:modal)]:[scrollbar-gutter:stable]"
                , "supports-[-webkit-touch-callout:none]:[html:has(&:modal)]:transition-[filter] supports-[-webkit-touch-callout:none]:[html:has(&:modal)]:duration-200 supports-[-webkit-touch-callout:none]:[html:has(&:modal)]:ease-in-out supports-[-webkit-touch-callout:none]:[html:has(elmcn-dialog[open]>&:modal)]:[filter:blur(var(--blur-xs))_brightness(0.9)]"
                , if padded then
                    "md:p-2"

                  else
                    case config.side of
                        Left ->
                            "md:border-e"

                        Right ->
                            "md:border-s"
                , if collapsed then
                    case config.collapsible of
                        Icon ->
                            if padded then
                                "md:w-[calc(var(--sidebar-width-icon)+(--spacing(4))+2px)] md:[translate:0_0]"

                            else
                                "md:w-(--sidebar-width-icon) md:[translate:0_0]"

                        _ ->
                            case config.side of
                                Left ->
                                    "md:[translate:-100%_0] md:rtl:[translate:100%_0]"

                                Right ->
                                    "md:[translate:100%_0] md:rtl:[translate:-100%_0]"

                  else
                    "md:[translate:0_0]"
                ]

        innerClass =
            String.join " "
                [ "absolute inset-y-0 start-0 flex w-(--sidebar-width-mobile) flex-col border-e bg-sidebar text-sidebar-foreground shadow-lg md:static md:inset-auto md:size-full md:border-0 md:shadow-none"
                , "opacity-0 transition-[opacity,translate] duration-200 ease-in-out md:opacity-100 md:transition-none [elmcn-dialog[open]>dialog>&]:opacity-100 starting:[elmcn-dialog[open]>dialog>&]:opacity-0"
                , case config.side of
                    Left ->
                        "max-md:[translate:-2.5rem_0] max-md:rtl:[translate:2.5rem_0] [elmcn-dialog[open]>dialog>&]:[translate:0_0] starting:[elmcn-dialog[open]>dialog>&]:[translate:-2.5rem_0] rtl:starting:[elmcn-dialog[open]>dialog>&]:[translate:2.5rem_0]"

                    Right ->
                        "start-auto end-0 border-e-0 border-s max-md:[translate:2.5rem_0] max-md:rtl:[translate:-2.5rem_0] [elmcn-dialog[open]>dialog>&]:[translate:0_0] starting:[elmcn-dialog[open]>dialog>&]:[translate:2.5rem_0] rtl:starting:[elmcn-dialog[open]>dialog>&]:[translate:-2.5rem_0]"
                , case config.variant of
                    Floating ->
                        "md:rounded-lg md:shadow-sm md:ring-1 md:ring-sidebar-border"

                    _ ->
                        ""
                ]
    in
    Html.div
        [ attribute "data-slot" "sidebar"
        , attribute "data-side" (sideName config.side)
        , attribute "data-variant" (variantName config.variant)
        , attribute "data-state"
            (if collapsed then
                "collapsed"

             else
                "expanded"
            )
        , attribute "data-collapsible"
            (if collapsed then
                collapsibleName config.collapsible

             else
                ""
            )
        , class "group peer text-sidebar-foreground"
        ]
        [ Html.div
            [ attribute "data-slot" "sidebar-gap", class gapClass ]
            [ Html.node "elmcn-dialog"
                (class "contents"
                    :: (if config.state.openMobile then
                            [ attribute "open" "" ]

                        else
                            []
                       )
                )
                [ Html.node "dialog"
                    ([ attribute "data-slot" "sidebar-container"
                     , attribute "data-sidebar" "sidebar"
                     , attribute "aria-label" "Sidebar"
                     , class containerClass
                     ]
                        ++ List.concatMap closeRequests closeMobile
                    )
                    [ Html.div
                        ([ attribute "data-slot" "sidebar-overlay"
                         , attribute "aria-hidden" "true"
                         , class "absolute inset-0 select-none md:hidden not-supports-[-webkit-touch-callout:none]:bg-black/10"
                         , class "opacity-0 transition-[opacity,backdrop-filter] duration-200 ease-in-out [elmcn-dialog[open]>dialog>&]:opacity-100 starting:[elmcn-dialog[open]>dialog>&]:opacity-0 not-supports-[-webkit-touch-callout:none]:[backdrop-filter:blur(0)] not-supports-[-webkit-touch-callout:none]:[elmcn-dialog[open]>dialog>&]:[backdrop-filter:blur(var(--blur-xs))] not-supports-[-webkit-touch-callout:none]:starting:[elmcn-dialog[open]>dialog>&]:[backdrop-filter:blur(0)]"
                         ]
                            ++ List.map Html.Events.onClick closeMobile
                        )
                        []
                    , Html.div
                        [ attribute "data-slot" "sidebar-inner"
                        , attribute "data-sidebar" "sidebar"
                        , class innerClass
                        ]
                        parts
                    , if config.rail then
                        viewRail config

                      else
                        Html.text ""
                    ]
                ]
            ]
        ]


viewRail : Config msg -> Html msg
viewRail config =
    let
        collapsed =
            not config.state.open

        offcanvas_ =
            collapsed && config.collapsible == Offcanvas
    in
    Html.button
        ([ type_ "button"
         , attribute "data-slot" "sidebar-rail"
         , attribute "data-sidebar" "rail"
         , attribute "aria-label" "Toggle Sidebar"
         , Html.Attributes.title "Toggle Sidebar"
         , tabindex -1
         , class "absolute inset-y-0 z-20 hidden w-4 transition-all ease-linear after:absolute after:inset-y-0 after:start-1/2 after:w-[2px] hover:after:bg-sidebar-border md:flex"
         , class
            (case config.side of
                Left ->
                    if offcanvas_ then
                        "-end-2 translate-x-0 after:start-full hover:bg-sidebar"

                    else
                        "-end-4 -translate-x-1/2 rtl:translate-x-1/2"

                Right ->
                    if offcanvas_ then
                        "-start-2 translate-x-0 after:start-full hover:bg-sidebar"

                    else
                        "start-0 -translate-x-1/2 rtl:translate-x-1/2"
            )
         , class
            (case ( config.side, collapsed ) of
                ( Left, False ) ->
                    "cursor-w-resize rtl:cursor-e-resize"

                ( Left, True ) ->
                    "cursor-e-resize rtl:cursor-w-resize"

                ( Right, False ) ->
                    "cursor-e-resize rtl:cursor-w-resize"

                ( Right, True ) ->
                    "cursor-w-resize rtl:cursor-e-resize"
            )
         ]
            ++ toggleEvents config
        )
        []


toggleEvents : Config msg -> List (Attribute msg)
toggleEvents config =
    List.filterMap identity
        [ Maybe.map (\onChange -> Html.Events.on "click" (toggle config.state onChange)) config.onChange ]


toggle : State -> (State -> msg) -> Decoder msg
toggle current onChange =
    Decode.maybe (Decode.at [ "view", "innerWidth" ] Decode.float)
        |> Decode.map
            (\innerWidth ->
                if Maybe.withDefault 1024 innerWidth < 768 then
                    onChange { current | openMobile = not current.openMobile }

                else
                    onChange { current | open = not current.open }
            )


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


sideName : Side -> String
sideName side =
    case side of
        Left ->
            "left"

        Right ->
            "right"


variantName : Variant -> String
variantName variant =
    case variant of
        Default ->
            "sidebar"

        Floating ->
            "floating"

        Inset ->
            "inset"


collapsibleName : Collapsible -> String
collapsibleName collapsible_ =
    case collapsible_ of
        Offcanvas ->
            "offcanvas"

        Icon ->
            "icon"

        None ->
            "none"



-- TRIGGER


trigger : List (Attribute msg) -> State -> (State -> msg) -> Html msg
trigger attrs current onChange =
    Button.new
        |> Button.icon (Phosphor.sidebarSimple Bold |> Phosphor.toHtml [ attribute "aria-hidden" "true" ])
        |> Button.label "Toggle Sidebar"
        |> Button.ghost
        |> Button.small
        |> Button.toHtml
            ([ attribute "data-slot" "sidebar-trigger"
             , attribute "data-sidebar" "trigger"
             , Html.Events.on "click" (toggle current onChange)
             ]
                ++ attrs
            )



-- GROUPS


group : List (Attribute msg) -> List (Html msg) -> Html msg
group attrs children =
    Html.div
        ([ attribute "data-slot" "sidebar-group"
         , attribute "data-sidebar" "group"
         , class "relative flex w-full min-w-0 flex-col px-3 py-2"
         ]
            ++ attrs
        )
        children


groupLabel : List (Attribute msg) -> List (Html msg) -> Html msg
groupLabel attrs children =
    Html.div
        ([ attribute "data-slot" "sidebar-group-label"
         , attribute "data-sidebar" "group-label"
         , class groupLabelClass
         ]
            ++ attrs
        )
        children


groupLabelClass : String
groupLabelClass =
    "flex h-8 shrink-0 items-center rounded-md px-2 text-xs font-medium text-sidebar-foreground/70 ring-sidebar-ring outline-hidden transition-[margin,opacity] duration-200 ease-linear focus-visible:ring-2 md:group-data-[collapsible=icon]:-mt-8 md:group-data-[collapsible=icon]:opacity-0 [&>svg]:size-4 [&>svg]:shrink-0"


groupAction : List (Attribute msg) -> List (Html msg) -> Html msg
groupAction attrs children =
    Html.button
        ([ type_ "button"
         , attribute "data-slot" "sidebar-group-action"
         , attribute "data-sidebar" "group-action"
         , class "absolute end-3 top-3.5 flex aspect-square w-5 items-center justify-center rounded-md p-0 text-sidebar-foreground ring-sidebar-ring outline-hidden transition-transform hover:bg-sidebar-accent hover:text-sidebar-accent-foreground focus-visible:ring-2 md:group-data-[collapsible=icon]:hidden [&>svg]:size-4 [&>svg]:shrink-0"
         , class "after:absolute after:-inset-2 md:after:hidden"
         ]
            ++ attrs
        )
        children


groupContent : List (Attribute msg) -> List (Html msg) -> Html msg
groupContent attrs children =
    Html.div
        ([ attribute "data-slot" "sidebar-group-content"
         , attribute "data-sidebar" "group-content"
         , class "w-full text-sm"
         ]
            ++ attrs
        )
        children


separator : List (Attribute msg) -> Html msg
separator attrs =
    Html.div
        ([ attribute "data-slot" "sidebar-separator"
         , attribute "data-sidebar" "separator"
         , attribute "role" "none"
         , class "mx-2 h-px w-auto shrink-0 bg-sidebar-border"
         ]
            ++ attrs
        )
        []



-- MENUS


menu : List (Attribute msg) -> List (Html msg) -> Html msg
menu attrs children =
    Html.ul
        ([ attribute "data-slot" "sidebar-menu"
         , attribute "data-sidebar" "menu"
         , class "flex w-full min-w-0 flex-col gap-1"
         ]
            ++ attrs
        )
        children


menuItem : List (Attribute msg) -> List (Html msg) -> Html msg
menuItem attrs children =
    Html.li
        ([ attribute "data-slot" "sidebar-menu-item"
         , attribute "data-sidebar" "menu-item"
         , class "group/menu-item relative"
         ]
            ++ attrs
        )
        children


menuButton : List (Attribute msg) -> List (Html msg) -> Html msg
menuButton attrs children =
    Html.button (type_ "button" :: menuButtonAttributes ++ attrs) children


menuLink : List (Attribute msg) -> List (Html msg) -> Html msg
menuLink attrs children =
    Html.a (menuButtonAttributes ++ attrs) children


menuButtonAttributes : List (Attribute msg)
menuButtonAttributes =
    [ attribute "data-slot" "sidebar-menu-button"
    , attribute "data-sidebar" "menu-button"
    , attribute "data-size" "default"
    , attribute "data-variant" "default"
    , class "peer/menu-button group/menu-button flex w-full items-center gap-2 overflow-hidden rounded-lg p-2 text-start text-sm font-medium text-muted-foreground ring-sidebar-ring outline-hidden transition-[width,height,padding] hover:bg-sidebar-accent hover:text-sidebar-accent-foreground focus-visible:ring-2 active:bg-sidebar-accent active:text-sidebar-accent-foreground disabled:pointer-events-none disabled:opacity-50 aria-disabled:pointer-events-none aria-disabled:opacity-50 data-active:bg-sidebar-accent data-active:font-medium data-active:text-sidebar-accent-foreground [&:has(+:popover-open)]:bg-sidebar-accent [&:has(+:popover-open)]:text-sidebar-accent-foreground group-has-data-[sidebar=menu-action]/menu-item:pe-8 md:group-data-[collapsible=icon]:size-8! md:group-data-[collapsible=icon]:p-2! [&_svg]:size-4 [&_svg]:shrink-0 [&>span:last-child]:truncate"
    , class "data-[size=default]:h-8 data-[size=default]:text-sm data-[size=sm]:h-7 data-[size=sm]:text-xs data-[size=lg]:h-12 data-[size=lg]:text-sm md:group-data-[collapsible=icon]:data-[size=lg]:p-0!"
    , class "data-[variant=outline]:bg-background data-[variant=outline]:shadow-[0_0_0_1px_var(--sidebar-border)] data-[variant=outline]:hover:shadow-[0_0_0_1px_var(--sidebar-accent)]"
    , class tooltipClass
    ]


tooltipClass : String
tooltipClass =
    "[anchor-name:--sidebar-tooltip] [anchor-scope:--sidebar-tooltip] after:[position-anchor:--sidebar-tooltip] after:pointer-events-none after:invisible after:fixed after:z-50 after:ms-1 after:-translate-y-1/2 after:rounded-md after:bg-foreground after:px-3 after:py-1.5 after:text-xs after:whitespace-nowrap after:text-background after:opacity-0 after:transition-[opacity,visibility] after:duration-150 after:content-[attr(data-tooltip)] after:[top:calc(anchor(top)+2px)] after:[inset-inline-start:calc(anchor(start)+8px)] supports-[anchor-scope:all]:md:group-data-[collapsible=icon]:data-[tooltip]:hover:after:visible supports-[anchor-scope:all]:md:group-data-[collapsible=icon]:data-[tooltip]:hover:after:opacity-100 supports-[anchor-scope:all]:md:group-data-[collapsible=icon]:data-[tooltip]:focus-visible:after:visible supports-[anchor-scope:all]:md:group-data-[collapsible=icon]:data-[tooltip]:focus-visible:after:opacity-100"


menuAction : List (Attribute msg) -> List (Html msg) -> Html msg
menuAction attrs children =
    Html.button (type_ "button" :: menuActionAttributes ++ attrs) children


menuActionAttributes : List (Attribute msg)
menuActionAttributes =
    [ attribute "data-slot" "sidebar-menu-action"
    , attribute "data-sidebar" "menu-action"
    , class "absolute end-1 top-1.5 flex aspect-square w-5 items-center justify-center rounded-md p-0 text-sidebar-foreground ring-sidebar-ring outline-hidden transition-transform hover:bg-sidebar-accent hover:text-sidebar-accent-foreground peer-hover/menu-button:text-sidebar-accent-foreground peer-data-[size=default]/menu-button:top-1.5 peer-data-[size=lg]/menu-button:top-2.5 peer-data-[size=sm]/menu-button:top-1 focus-visible:ring-2 md:group-data-[collapsible=icon]:hidden [&>svg]:size-4 [&>svg]:shrink-0"
    , class "after:absolute after:-inset-2 md:after:hidden"
    ]


menuBadge : List (Attribute msg) -> List (Html msg) -> Html msg
menuBadge attrs children =
    Html.div
        ([ attribute "data-slot" "sidebar-menu-badge"
         , attribute "data-sidebar" "menu-badge"
         , class "pointer-events-none absolute end-1 flex h-5 min-w-5 items-center justify-center rounded-md px-1 text-xs font-medium text-sidebar-foreground tabular-nums select-none peer-hover/menu-button:text-sidebar-accent-foreground peer-data-active/menu-button:text-sidebar-accent-foreground peer-data-[size=default]/menu-button:top-1.5 peer-data-[size=lg]/menu-button:top-2.5 peer-data-[size=sm]/menu-button:top-1 md:group-data-[collapsible=icon]:hidden"
         ]
            ++ attrs
        )
        children


menuSkeleton : List (Attribute msg) -> Html msg
menuSkeleton attrs =
    Html.div
        ([ attribute "data-slot" "sidebar-menu-skeleton"
         , attribute "data-sidebar" "menu-skeleton"
         , class "flex h-8 items-center gap-2 rounded-md px-2"
         ]
            ++ attrs
        )
        [ Skeleton.view [ attribute "data-sidebar" "menu-skeleton-icon", class "hidden size-4 rounded-md [[data-icon]>&]:block" ]
        , Skeleton.view [ attribute "data-sidebar" "menu-skeleton-text", class "h-4 max-w-(--skeleton-width,70%) flex-1" ]
        ]


menuSub : List (Attribute msg) -> List (Html msg) -> Html msg
menuSub attrs children =
    Html.ul
        ([ attribute "data-slot" "sidebar-menu-sub"
         , attribute "data-sidebar" "menu-sub"
         , class "mx-3.5 flex min-w-0 translate-x-px flex-col gap-1 border-s border-sidebar-border px-2.5 py-0.5 rtl:-translate-x-px md:group-data-[collapsible=icon]:hidden"
         ]
            ++ attrs
        )
        children


menuSubItem : List (Attribute msg) -> List (Html msg) -> Html msg
menuSubItem attrs children =
    Html.li
        ([ attribute "data-slot" "sidebar-menu-sub-item"
         , attribute "data-sidebar" "menu-sub-item"
         , class "group/menu-sub-item relative"
         ]
            ++ attrs
        )
        children


menuSubLink : List (Attribute msg) -> List (Html msg) -> Html msg
menuSubLink attrs children =
    Html.a (menuSubButtonAttributes ++ attrs) children


menuSubButton : List (Attribute msg) -> List (Html msg) -> Html msg
menuSubButton attrs children =
    Html.button (type_ "button" :: menuSubButtonAttributes ++ attrs) children


menuSubButtonAttributes : List (Attribute msg)
menuSubButtonAttributes =
    [ attribute "data-slot" "sidebar-menu-sub-button"
    , attribute "data-sidebar" "menu-sub-button"
    , attribute "data-size" "md"
    , class "flex h-7 min-w-0 -translate-x-px items-center gap-2 overflow-hidden rounded-md px-2 text-sidebar-foreground ring-sidebar-ring outline-hidden rtl:translate-x-px hover:bg-sidebar-accent hover:text-sidebar-accent-foreground focus-visible:ring-2 active:bg-sidebar-accent active:text-sidebar-accent-foreground disabled:pointer-events-none disabled:opacity-50 aria-disabled:pointer-events-none aria-disabled:opacity-50 data-active:bg-sidebar-accent data-active:text-sidebar-accent-foreground data-[size=md]:text-sm data-[size=sm]:text-xs md:group-data-[collapsible=icon]:hidden [&>span:last-child]:truncate [&>svg]:size-4 [&>svg]:shrink-0 [&>svg]:text-sidebar-accent-foreground"
    ]



-- COLLAPSIBLE


collapsible : List (Attribute msg) -> Trigger msg -> List (Html msg) -> Html msg
collapsible attrs (Trigger summary) children =
    Html.details
        ([ attribute "data-slot" "collapsible"
         , class "group/collapsible details-content:grid details-content:grid-rows-[0fr] details-content:[content-visibility:visible] details-content:transition-[grid-template-rows] details-content:duration-200 details-content:ease-out open:details-content:grid-rows-[1fr]"
         ]
            ++ attrs
        )
        [ summary
        , Html.div
            [ attribute "data-slot" "collapsible-content"
            , class "invisible min-h-0 overflow-hidden transition-[visibility] duration-200 group-open/collapsible:visible"
            ]
            children
        ]


type Trigger msg
    = Trigger (Html msg)


menuTrigger : List (Attribute msg) -> List (Html msg) -> Trigger msg
menuTrigger attrs children =
    Trigger (Html.summary (summaryClass :: menuButtonAttributes ++ attrs) children)


groupTrigger : List (Attribute msg) -> List (Html msg) -> Trigger msg
groupTrigger attrs children =
    Trigger
        (Html.summary
            ([ summaryClass
             , attribute "data-slot" "sidebar-group-label"
             , attribute "data-sidebar" "group-label"
             , class groupLabelClass
             ]
                ++ attrs
            )
            children
        )


summaryClass : Attribute msg
summaryClass =
    class "cursor-pointer list-none select-none [&::-webkit-details-marker]:hidden"


defaultOpen : Attribute msg
defaultOpen =
    property "open" (Encode.bool True)



-- MODIFIERS


active : Attribute msg
active =
    attribute "data-active" "true"


outline : Attribute msg
outline =
    attribute "data-variant" "outline"


small : Attribute msg
small =
    attribute "data-size" "sm"


large : Attribute msg
large =
    attribute "data-size" "lg"


tooltip : String -> Attribute msg
tooltip label =
    attribute "data-tooltip" label


showOnHover : Attribute msg
showOnHover =
    class "group-focus-within/menu-item:opacity-100 group-hover/menu-item:opacity-100 peer-data-active/menu-button:text-sidebar-accent-foreground [&:has(+:popover-open)]:opacity-100 md:opacity-0"


showIcon : Attribute msg
showIcon =
    attribute "data-icon" ""
