module UI.Empty exposing
    ( Empty
    , NeedsTitle
    , Ready
    , content
    , description
    , icon
    , media
    , new
    , title
    , toHtml
    )

import Html exposing (Attribute, Html, div)
import Html.Attributes exposing (attribute, class)


type Empty status msg
    = Empty
        { media : Maybe (Media msg)
        , title : List (Html msg)
        , description : List (Html msg)
        , content : List (Html msg)
        }


type Ready
    = Ready


type NeedsTitle
    = NeedsTitle


type Media msg
    = Icon (Html msg)
    | Media (List (Html msg))



-- CONSTRUCTOR


new : Empty NeedsTitle msg
new =
    Empty
        { media = Nothing
        , title = []
        , description = []
        , content = []
        }



-- SETTERS


title : List (Html msg) -> Empty status msg -> Empty Ready msg
title children (Empty config) =
    Empty { config | title = children }


description : List (Html msg) -> Empty status msg -> Empty status msg
description children (Empty config) =
    Empty { config | description = children }


icon : Html msg -> Empty status msg -> Empty status msg
icon child (Empty config) =
    Empty { config | media = Just (Icon child) }


media : List (Html msg) -> Empty status msg -> Empty status msg
media children (Empty config) =
    Empty { config | media = Just (Media children) }


content : List (Html msg) -> Empty status msg -> Empty status msg
content children (Empty config) =
    Empty { config | content = children }



-- RENDERING


toHtml : List (Attribute msg) -> Empty Ready msg -> Html msg
toHtml attrs (Empty config) =
    div
        ([ attribute "data-slot" "empty"
         , class "flex w-full min-w-0 flex-1 flex-col items-center justify-center gap-4 rounded-xl border-dashed p-6 text-center text-balance"
         ]
            ++ attrs
        )
        (List.filterMap identity
            [ Just
                (slot "empty-header"
                    "flex max-w-sm flex-col items-center gap-2"
                    (List.filterMap identity
                        [ Maybe.map viewMedia config.media
                        , Just (slot "empty-title" "text-sm font-medium tracking-tight" config.title)
                        , if List.isEmpty config.description then
                            Nothing

                          else
                            Just (slot "empty-description" descriptionClass config.description)
                        ]
                    )
                )
            , if List.isEmpty config.content then
                Nothing

              else
                Just (slot "empty-content" "flex w-full max-w-sm min-w-0 flex-col items-center gap-2.5 text-sm text-balance" config.content)
            ]
        )


viewMedia : Media msg -> Html msg
viewMedia value =
    let
        ( variant, extra, children ) =
            case value of
                Icon child ->
                    ( "icon", "size-8 rounded-lg bg-muted text-foreground [&_svg:not([class*='size-'])]:size-4", [ child ] )

                Media items ->
                    ( "default", "bg-transparent", items )
    in
    div
        [ attribute "data-slot" "empty-icon"
        , attribute "data-variant" variant
        , class "mb-2 flex shrink-0 items-center justify-center [&_svg]:pointer-events-none [&_svg]:shrink-0"
        , class extra
        ]
        children


descriptionClass : String
descriptionClass =
    "text-sm/relaxed text-muted-foreground [&>a]:underline [&>a]:underline-offset-4 [&>a:hover]:text-primary"


slot : String -> String -> List (Html msg) -> Html msg
slot name classes children =
    div [ attribute "data-slot" name, class classes ] children
