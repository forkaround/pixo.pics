module UI.InputGroup exposing
    ( bottom
    , button
    , end
    , input
    , start
    , text
    , textarea
    , top
    , view
    )

import Html exposing (Attribute, Html)
import Html.Attributes exposing (attribute, class)
import UI.Button as Button exposing (Button, NeedsContent)


view : List (Attribute msg) -> List (Html msg) -> Html msg
view attrs children =
    Html.div
        ([ attribute "data-slot" "input-group"
         , attribute "role" "group"
         , class "group/input-group relative flex h-8 w-full min-w-0 items-center rounded-lg border border-input transition-colors outline-none has-[>textarea]:h-auto dark:bg-input/30"
         , class "has-[[data-slot=input-group-control]:focus-visible]:border-ring has-[[data-slot=input-group-control]:focus-visible]:ring-3 has-[[data-slot=input-group-control]:focus-visible]:ring-ring/50"
         , class "has-[[data-slot][aria-invalid=true]]:border-destructive has-[[data-slot][aria-invalid=true]]:ring-3 has-[[data-slot][aria-invalid=true]]:ring-destructive/20 dark:has-[[data-slot][aria-invalid=true]]:ring-destructive/40"
         , class "has-disabled:bg-input/50 has-disabled:opacity-50 dark:has-disabled:bg-input/80"
         , class "has-[>[data-align=block-start]]:h-auto has-[>[data-align=block-start]]:flex-col has-[>[data-align=block-start]]:[&>input]:pb-3 has-[>[data-align=block-end]]:h-auto has-[>[data-align=block-end]]:flex-col has-[>[data-align=block-end]]:[&>input]:pt-3"
         , class "has-[>[data-align=inline-start]]:[&>input]:ps-1.5 has-[>[data-align=inline-end]]:[&>input]:pe-1.5"
         ]
            ++ attrs
        )
        children



-- CONTROLS


input : List (Attribute msg) -> Html msg
input attrs =
    Html.input
        (attribute "data-slot" "input-group-control"
            :: class "h-8 w-full min-w-0 flex-1 rounded-none border-0 bg-transparent px-2.5 py-1 text-base outline-none file:inline-flex file:h-6 file:border-0 file:bg-transparent file:text-sm file:font-medium file:text-foreground placeholder:text-muted-foreground disabled:pointer-events-none disabled:cursor-not-allowed disabled:opacity-50 md:text-sm"
            :: attrs
        )
        []


textarea : List (Attribute msg) -> Html msg
textarea attrs =
    Html.textarea
        (attribute "data-slot" "input-group-control"
            :: class "flex field-sizing-content min-h-16 w-full flex-1 resize-none rounded-none border-0 bg-transparent px-2.5 py-2 text-base outline-none placeholder:text-muted-foreground disabled:cursor-not-allowed disabled:opacity-50 md:text-sm"
            :: attrs
        )
        []



-- ADDONS


start : List (Attribute msg) -> List (Html msg) -> Html msg
start =
    addon "inline-start" "order-first ps-2 has-[>button]:ms-[-0.3rem] has-[>kbd]:ms-[-0.15rem]"


end : List (Attribute msg) -> List (Html msg) -> Html msg
end =
    addon "inline-end" "order-last pe-2 has-[>button]:me-[-0.3rem] has-[>kbd]:me-[-0.15rem]"


top : List (Attribute msg) -> List (Html msg) -> Html msg
top =
    addon "block-start" "order-first w-full justify-start px-2.5 pt-2 group-has-[>input]/input-group:pt-2 [.border-b]:pb-2"


bottom : List (Attribute msg) -> List (Html msg) -> Html msg
bottom =
    addon "block-end" "order-last w-full justify-start px-2.5 pb-2 group-has-[>input]/input-group:pb-2 [.border-t]:pt-2"


addon : String -> String -> List (Attribute msg) -> List (Html msg) -> Html msg
addon align classes attrs children =
    Html.div
        ([ attribute "data-slot" "input-group-addon"
         , attribute "data-align" align
         , attribute "role" "group"
         , class "flex h-auto items-center justify-center gap-2 py-1.5 text-sm font-medium text-muted-foreground select-none group-data-[disabled=true]/input-group:opacity-50 [&>kbd]:rounded-[calc(var(--radius)-5px)] [&>svg:not([class*='size-'])]:size-4"
         , class classes
         ]
            ++ attrs
        )
        children



-- ADDON CONTENT


text : List (Attribute msg) -> List (Html msg) -> Html msg
text attrs children =
    Html.span
        (attribute "data-slot" "input-group-text"
            :: class "flex items-center gap-2 text-sm text-muted-foreground [&_svg]:pointer-events-none [&_svg:not([class*='size-'])]:size-4"
            :: attrs
        )
        children


button : Button shape NeedsContent msg
button =
    Button.new
        |> Button.ghost
        |> Button.extraSmall
