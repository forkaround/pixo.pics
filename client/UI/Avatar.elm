module UI.Avatar exposing
    ( Avatar
    , Group
    , NeedsFallback
    , Ready
    , Single
    , badge
    , fallback
    , group
    , groupCount
    , img
    , large
    , new
    , small
    , toHtml
    )

import Html exposing (Attribute, Html)
import Html.Attributes exposing (attribute, class)


type Avatar shape status msg
    = Avatar (Content msg)


type Content msg
    = One (Config msg)
    | Many
        { members : List (Config msg)
        , count : Maybe (List (Html msg))
        }


type alias Config msg =
    { size : Size
    , fallback : List (Html msg)
    , image : Maybe (List (Attribute msg))
    , badge : Maybe (Html msg)
    }


type Single
    = Single


type Group
    = Group


type Ready
    = Ready


type NeedsFallback
    = NeedsFallback


type Size
    = Medium
    | Small
    | Large



-- CONSTRUCTOR


new : Avatar Single NeedsFallback msg
new =
    Avatar
        (One
            { size = Medium
            , fallback = []
            , image = Nothing
            , badge = Nothing
            }
        )



-- SETTERS


fallback : List (Html msg) -> Avatar Single status msg -> Avatar Single Ready msg
fallback children =
    update (\config -> { config | fallback = children })


img : List (Attribute msg) -> Avatar Single status msg -> Avatar Single status msg
img attrs =
    update (\config -> { config | image = Just attrs })


badge : List (Attribute msg) -> List (Html msg) -> Avatar Single status msg -> Avatar Single status msg
badge attrs children =
    update (\config -> { config | badge = Just (viewBadge attrs children) })


small : Avatar Single status msg -> Avatar Single status msg
small =
    update (\config -> { config | size = Small })


large : Avatar Single status msg -> Avatar Single status msg
large =
    update (\config -> { config | size = Large })


update : (Config msg -> Config msg) -> Avatar Single status msg -> Avatar Single other msg
update change (Avatar content) =
    case content of
        One config ->
            Avatar (One (change config))

        Many _ ->
            Avatar content



-- GROUPS


group : Avatar Single Ready msg -> List (Avatar Single Ready msg) -> Avatar Group Ready msg
group first rest =
    Avatar (Many { members = List.filterMap single (first :: rest), count = Nothing })


single : Avatar Single Ready msg -> Maybe (Config msg)
single (Avatar content) =
    case content of
        One config ->
            Just config

        Many _ ->
            Nothing


groupCount : List (Html msg) -> Avatar Group Ready msg -> Avatar Group Ready msg
groupCount children (Avatar content) =
    case content of
        Many stack ->
            Avatar (Many { stack | count = Just children })

        One _ ->
            Avatar content



-- RENDERING


toHtml : List (Attribute msg) -> Avatar shape Ready msg -> Html msg
toHtml attrs (Avatar content) =
    case content of
        One config ->
            viewAvatar attrs config

        Many stack ->
            Html.div
                (attribute "data-slot" "avatar-group"
                    :: class "group/avatar-group flex -space-x-2 *:data-[slot=avatar]:ring-2 *:data-[slot=avatar]:ring-background"
                    :: attrs
                )
                (List.map (viewAvatar []) stack.members
                    ++ List.filterMap identity [ Maybe.map viewCount stack.count ]
                )


viewAvatar : List (Attribute msg) -> Config msg -> Html msg
viewAvatar attrs config =
    Html.span
        ([ attribute "data-slot" "avatar"
         , attribute "data-size" (sizeName config.size)
         , class "group/avatar relative flex size-8 shrink-0 rounded-full select-none after:absolute after:inset-0 after:rounded-full after:border after:border-border after:mix-blend-darken data-[size=lg]:size-10 data-[size=sm]:size-6 dark:after:mix-blend-lighten"
         ]
            ++ attrs
        )
        (List.filterMap identity
            [ Just (viewFallback config.fallback)
            , Maybe.map viewImage config.image
            , config.badge
            ]
        )


viewImage : List (Attribute msg) -> Html msg
viewImage attrs =
    Html.img
        (attribute "data-slot" "avatar-image"
            :: class "absolute inset-0 aspect-square size-full rounded-full object-cover"
            :: attrs
        )
        []


viewFallback : List (Html msg) -> Html msg
viewFallback children =
    Html.span
        [ attribute "data-slot" "avatar-fallback"
        , class "flex size-full items-center justify-center rounded-full bg-muted text-sm text-muted-foreground group-data-[size=sm]/avatar:text-xs"
        ]
        children


viewBadge : List (Attribute msg) -> List (Html msg) -> Html msg
viewBadge attrs children =
    Html.span
        ([ attribute "data-slot" "avatar-badge"
         , class "absolute right-0 bottom-0 z-10 inline-flex items-center justify-center rounded-full bg-primary text-primary-foreground bg-blend-color ring-2 ring-background select-none"
         , class "group-data-[size=sm]/avatar:size-2 group-data-[size=sm]/avatar:[&>svg]:hidden"
         , class "group-data-[size=default]/avatar:size-2.5 group-data-[size=default]/avatar:[&>svg]:size-2"
         , class "group-data-[size=lg]/avatar:size-3 group-data-[size=lg]/avatar:[&>svg]:size-2"
         ]
            ++ attrs
        )
        children


viewCount : List (Html msg) -> Html msg
viewCount children =
    Html.div
        [ attribute "data-slot" "avatar-group-count"
        , class "relative flex size-8 shrink-0 items-center justify-center rounded-full bg-muted text-sm text-muted-foreground ring-2 ring-background group-has-data-[size=lg]/avatar-group:size-10 group-has-data-[size=sm]/avatar-group:size-6 [&>svg]:size-4 group-has-data-[size=lg]/avatar-group:[&>svg]:size-5 group-has-data-[size=sm]/avatar-group:[&>svg]:size-3"
        ]
        children


sizeName : Size -> String
sizeName size =
    case size of
        Medium ->
            "default"

        Small ->
            "sm"

        Large ->
            "lg"
