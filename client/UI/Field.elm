module UI.Field exposing
    ( content
    , description
    , disabled
    , error
    , errors
    , group
    , horizontal
    , invalid
    , label
    , legend
    , responsive
    , separator
    , set
    , smallLegend
    , title
    , view
    )

import Html exposing (Attribute, Html)
import Html.Attributes exposing (attribute, class)
import UI.Label as Label
import UI.Separator as Separator


view : List (Attribute msg) -> List (Html msg) -> Html msg
view =
    field "vertical" "flex-col *:w-full [&>.sr-only]:w-auto"


horizontal : List (Attribute msg) -> List (Html msg) -> Html msg
horizontal =
    field "horizontal" ("flex-row items-center has-[>[data-slot=field-content]]:items-start *:data-[slot=field-label]:flex-auto " ++ controlOffset "")


responsive : List (Attribute msg) -> List (Html msg) -> Html msg
responsive =
    field "responsive" ("flex-col *:w-full [&>.sr-only]:w-auto @md/field-group:flex-row @md/field-group:items-center @md/field-group:*:w-auto @md/field-group:has-[>[data-slot=field-content]]:items-start @md/field-group:*:data-[slot=field-label]:flex-auto " ++ controlOffset "@md/field-group:")


field : String -> String -> List (Attribute msg) -> List (Html msg) -> Html msg
field orientation classes attrs children =
    Html.div
        ([ attribute "role" "group"
         , attribute "data-slot" "field"
         , attribute "data-orientation" orientation
         , class "group/field flex w-full gap-2 data-[invalid=true]:text-destructive"
         , class classes
         ]
            ++ attrs
        )
        children


controlOffset : String -> String
controlOffset prefix =
    prefix ++ "has-[>[data-slot=field-content]]:[&>:has(>[data-slot=checkbox])]:mt-px " ++ prefix ++ "has-[>[data-slot=field-content]]:[&>:has(>[data-slot=radio-group-item])]:mt-px"


label : List (Attribute msg) -> List (Html msg) -> Html msg
label attrs children =
    Label.view
        ([ attribute "data-slot" "field-label"
         , class "group/field-label peer/field-label flex w-fit gap-2 leading-snug group-data-[disabled=true]/field:opacity-50"
         , class "has-[>[data-slot=field]]:w-full has-[>[data-slot=field]]:flex-col has-[>[data-slot=field]]:rounded-lg has-[>[data-slot=field]]:border has-[>[data-slot=field]]:not-has-[:disabled,[data-disabled]]:hover:bg-muted/50 has-[>[data-slot=field]]:has-[:focus-visible]:border-ring has-[>[data-slot=field]]:has-[:focus-visible]:ring-3 has-[>[data-slot=field]]:has-[:focus-visible]:ring-ring/50 *:data-[slot=field]:p-2.5"
         , class "has-checked:border-primary/30 has-checked:bg-primary/5 dark:has-checked:border-primary/20 dark:has-checked:bg-primary/10"
         ]
            ++ attrs
        )
        children


title : List (Attribute msg) -> List (Html msg) -> Html msg
title attrs children =
    Html.div
        (attribute "data-slot" "field-label"
            :: class "flex w-fit items-center gap-2 text-sm leading-snug font-medium group-data-[disabled=true]/field:opacity-50"
            :: attrs
        )
        children


description : List (Attribute msg) -> List (Html msg) -> Html msg
description attrs children =
    Html.p
        (attribute "data-slot" "field-description"
            :: class "text-left text-sm leading-normal font-normal text-muted-foreground group-data-[orientation=horizontal]/field:text-balance last:mt-0 nth-last-2:-mt-1 [[data-variant=legend]+&]:-mt-1.5 [&>a]:underline [&>a]:underline-offset-4 [&>a:hover]:text-primary"
            :: attrs
        )
        children


content : List (Attribute msg) -> List (Html msg) -> Html msg
content attrs children =
    Html.div
        (attribute "data-slot" "field-content"
            :: class "group/field-content flex flex-1 flex-col gap-0.5 leading-snug"
            :: attrs
        )
        children


error : List (Attribute msg) -> List (Html msg) -> Html msg
error attrs children =
    Html.div
        (attribute "role" "alert"
            :: attribute "data-slot" "field-error"
            :: class "text-sm font-normal text-destructive"
            :: attrs
        )
        children


errors : List (Attribute msg) -> List String -> Html msg
errors attrs messages =
    case unique messages of
        [] ->
            Html.text ""

        [ message ] ->
            error attrs [ Html.text message ]

        unique_ ->
            error attrs
                [ Html.ul [ class "ml-4 flex list-disc flex-col gap-1" ]
                    (List.map (\message -> Html.li [] [ Html.text message ]) unique_)
                ]


unique : List String -> List String
unique =
    List.foldr
        (\message acc ->
            if List.member message acc then
                acc

            else
                message :: acc
        )
        []


set : List (Attribute msg) -> List (Html msg) -> Html msg
set attrs children =
    Html.fieldset
        (attribute "data-slot" "field-set"
            :: class "flex flex-col gap-4 has-[>[data-slot=checkbox-group]]:gap-3 has-[>[data-slot=radio-group]]:gap-3"
            :: attrs
        )
        children


legend : List (Attribute msg) -> List (Html msg) -> Html msg
legend =
    legendWith "legend" "text-base"


smallLegend : List (Attribute msg) -> List (Html msg) -> Html msg
smallLegend =
    legendWith "label" "text-sm"


legendWith : String -> String -> List (Attribute msg) -> List (Html msg) -> Html msg
legendWith variant classes attrs children =
    Html.legend
        (attribute "data-slot" "field-legend"
            :: attribute "data-variant" variant
            :: class "mb-1.5 font-medium"
            :: class classes
            :: attrs
        )
        children


group : List (Attribute msg) -> List (Html msg) -> Html msg
group attrs children =
    Html.div
        (attribute "data-slot" "field-group"
            :: class "group/field-group @container/field-group flex w-full flex-col gap-5 data-[slot=checkbox-group]:gap-3 *:data-[slot=field-group]:gap-4"
            :: attrs
        )
        children


separator : List (Attribute msg) -> List (Html msg) -> Html msg
separator attrs children =
    Html.div
        (attribute "data-slot" "field-separator"
            :: attribute "data-content" (boolName (not (List.isEmpty children)))
            :: class "relative -my-2 h-5 text-sm group-data-[variant=outline]/field-group:-mb-2"
            :: attrs
        )
        (List.filterMap identity
            [ Just (Separator.toHtml [ class "absolute inset-0 top-1/2" ] Separator.new)
            , if List.isEmpty children then
                Nothing

              else
                Just
                    (Html.span
                        [ attribute "data-slot" "field-separator-content"
                        , class "relative mx-auto block w-fit bg-background px-2 text-muted-foreground"
                        ]
                        children
                    )
            ]
        )


boolName : Bool -> String
boolName bool =
    if bool then
        "true"

    else
        "false"


invalid : Attribute msg
invalid =
    attribute "data-invalid" "true"


disabled : Attribute msg
disabled =
    attribute "data-disabled" "true"
