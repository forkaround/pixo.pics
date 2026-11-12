module UI.Slider exposing
    ( NeedsHandler
    , NeedsValue
    , Ready
    , Slider
    , disabled
    , max
    , min
    , new
    , onChange
    , range
    , step
    , toHtml
    , value
    , values
    , vertical
    )

import Html exposing (Attribute, Html)
import Html.Attributes exposing (attribute, class, type_)
import Html.Events
import Json.Decode as Decode


type Slider a status msg
    = Slider
        { values : List Float
        , toValue : List Float -> Maybe a
        , onChange : Maybe (List Float -> Maybe msg)
        , min : Float
        , max : Float
        , step : Float
        , orientation : Orientation
        , disabled : Bool
        }


type NeedsValue
    = NeedsValue


type NeedsHandler
    = NeedsHandler


type Ready
    = Ready


type Orientation
    = Horizontal
    | Vertical



-- CONSTRUCTOR


new : Slider a NeedsValue msg
new =
    Slider
        { values = []
        , toValue = always Nothing
        , onChange = Nothing
        , min = 0
        , max = 100
        , step = 1
        , orientation = Horizontal
        , disabled = False
        }



-- VALUE


value : Float -> Slider a NeedsValue msg -> Slider Float NeedsHandler msg
value v =
    withValues [ v ] List.head


range : Float -> Float -> Slider a NeedsValue msg -> Slider ( Float, Float ) NeedsHandler msg
range low high =
    withValues [ low, high ]
        (\list ->
            case list of
                [ a, b ] ->
                    Just ( a, b )

                _ ->
                    Nothing
        )


values : List Float -> Slider a NeedsValue msg -> Slider (List Float) NeedsHandler msg
values list =
    withValues list Just


withValues : List Float -> (List Float -> Maybe b) -> Slider a NeedsValue msg -> Slider b NeedsHandler msg
withValues list toValue (Slider config) =
    Slider
        { values = list
        , toValue = toValue
        , onChange = Nothing
        , min = config.min
        , max = config.max
        , step = config.step
        , orientation = config.orientation
        , disabled = config.disabled
        }



-- HANDLER


onChange : (a -> msg) -> Slider a NeedsHandler msg -> Slider a Ready msg
onChange toMsg (Slider config) =
    Slider { config | onChange = Just (config.toValue >> Maybe.map toMsg) }



-- SCALE


min : Float -> Slider a status msg -> Slider a status msg
min v (Slider config) =
    Slider { config | min = v }


max : Float -> Slider a status msg -> Slider a status msg
max v (Slider config) =
    Slider { config | max = v }


step : Float -> Slider a status msg -> Slider a status msg
step v (Slider config) =
    Slider { config | step = v }



-- SETTERS


vertical : Slider a status msg -> Slider a status msg
vertical (Slider config) =
    Slider { config | orientation = Vertical }


disabled : Slider a status msg -> Slider a status msg
disabled (Slider config) =
    Slider { config | disabled = True }



-- RENDERING


toHtml : List (Attribute msg) -> Slider a Ready msg -> Html msg
toHtml attrs (Slider config) =
    let
        orientation =
            case config.orientation of
                Horizontal ->
                    "horizontal"

                Vertical ->
                    "vertical"

        fractions =
            List.map (fraction config) config.values

        rangeStart =
            case fractions of
                _ :: _ :: _ ->
                    List.head fractions |> Maybe.map trackPosition |> Maybe.withDefault "0px"

                _ ->
                    "0px"

        rangeEnd =
            List.reverse fractions |> List.head |> Maybe.map trackPosition |> Maybe.withDefault "0px"

        count =
            List.length config.values
    in
    Html.span
        ([ attribute "data-slot" "slider"
         , attribute "data-orientation" orientation
         , class "relative flex touch-none items-center select-none data-disabled:opacity-50"
         , class
            (case config.orientation of
                Horizontal ->
                    "min-h-3 w-full"

                Vertical ->
                    "h-full min-h-40 w-auto min-w-3 flex-col"
            )
         ]
            ++ (if config.disabled then
                    [ attribute "data-disabled" "" ]

                else
                    []
               )
            ++ attrs
        )
        (Html.span
            [ attribute "data-slot" "slider-track"
            , class "relative grow overflow-hidden rounded-full bg-muted select-none"
            , class
                (case config.orientation of
                    Horizontal ->
                        "h-1 w-full"

                    Vertical ->
                        "h-full w-1"
                )
            ]
            [ Html.span
                [ attribute "data-slot" "slider-range"
                , class "absolute bg-primary select-none"
                , class
                    (case config.orientation of
                        Horizontal ->
                            "h-full"

                        Vertical ->
                            "w-full"
                    )
                , attribute "style"
                    (case config.orientation of
                        Horizontal ->
                            "inset-inline-start: " ++ rangeStart ++ "; width: calc(" ++ rangeEnd ++ " - " ++ rangeStart ++ ")"

                        Vertical ->
                            "bottom: " ++ rangeStart ++ "; height: calc(" ++ rangeEnd ++ " - " ++ rangeStart ++ ")"
                    )
                ]
                []
            ]
            :: List.indexedMap (viewThumb config count) (List.map2 Tuple.pair config.values fractions)
        )


fraction : { c | min : Float, max : Float } -> Float -> Float
fraction config v =
    if config.max <= config.min then
        0

    else
        clamp 0 1 ((v - config.min) / (config.max - config.min))


trackPosition : Float -> String
trackPosition f =
    "calc(" ++ thumbRadius ++ " + " ++ String.fromFloat f ++ " * (100% - " ++ thumbSize ++ "))"


thumbSize : String
thumbSize =
    "0.75rem"


thumbRadius : String
thumbRadius =
    "0.375rem"


thumbInset : String
thumbInset =
    "0.5rem"


viewThumb :
    { c | values : List Float, onChange : Maybe (List Float -> Maybe msg), min : Float, max : Float, step : Float, orientation : Orientation, disabled : Bool }
    -> Int
    -> Int
    -> ( Float, Float )
    -> Html msg
viewThumb config count index ( v, f ) =
    let
        wrapperClass =
            case config.orientation of
                Horizontal ->
                    "pointer-events-none absolute top-1/2 -inset-x-2 h-7 -translate-y-1/2"

                Vertical ->
                    "pointer-events-none absolute left-1/2 -inset-y-2 w-7 -translate-x-1/2"

        inputClass =
            case config.orientation of
                Horizontal ->
                    ""

                Vertical ->
                    "[writing-mode:vertical-lr] [direction:rtl]"

        hitArea =
            if count == 1 then
                "pointer-events-auto"

            else
                "pointer-events-none [&::-webkit-slider-thumb]:pointer-events-auto [&::-moz-range-thumb]:pointer-events-auto"

        thumbPosition =
            "calc(" ++ thumbInset ++ " + " ++ String.fromFloat f ++ " * (100% - " ++ thumbSize ++ " - 2 * " ++ thumbInset ++ "))"

        ( thumbClass, thumbStyle ) =
            case config.orientation of
                Horizontal ->
                    ( "top-1/2 -translate-y-1/2", "inset-inline-start: " ++ thumbPosition )

                Vertical ->
                    ( "left-1/2 -translate-x-1/2", "bottom: " ++ thumbPosition )
    in
    Html.span
        [ class wrapperClass
        , Html.Attributes.style "z-index"
            (String.fromInt
                (if v >= config.max then
                    count - index

                 else
                    index + 1
                )
            )
        ]
        [ Html.input
            [ type_ "range"
            , attribute "data-slot" "slider-thumb"
            , attribute "min" (String.fromFloat config.min)
            , attribute "max" (String.fromFloat config.max)
            , attribute "step" (String.fromFloat config.step)
            , Html.Attributes.value (String.fromFloat v)
            , Html.Attributes.disabled config.disabled
            , Html.Events.on "input" (changeDecoder config index)
            , class "peer m-0 block size-full cursor-default appearance-none bg-transparent p-0 opacity-0 outline-none disabled:pointer-events-none"
            , class "[&::-webkit-slider-thumb]:size-7 [&::-webkit-slider-thumb]:appearance-none [&::-moz-range-thumb]:size-7 [&::-moz-range-thumb]:border-0"
            , class inputClass
            , class hitArea
            ]
            []
        , Html.span
            [ attribute "aria-hidden" "true"
            , class "pointer-events-none absolute block size-3 shrink-0 rounded-full border border-ring bg-white ring-ring/50 transition-[color,box-shadow] select-none peer-hover:ring-3 peer-focus-visible:ring-3 peer-active:ring-3 peer-disabled:pointer-events-none"
            , class thumbClass
            , attribute "style" thumbStyle
            ]
            []
        ]


changeDecoder : { c | values : List Float, onChange : Maybe (List Float -> Maybe msg), min : Float, max : Float } -> Int -> Decode.Decoder msg
changeDecoder config index =
    Html.Events.targetValue
        |> Decode.andThen
            (\string ->
                case ( String.toFloat string, config.onChange ) of
                    ( Just moved, Just toMsg ) ->
                        let
                            low =
                                if index == 0 then
                                    config.min

                                else
                                    List.drop (index - 1) config.values
                                        |> List.head
                                        |> Maybe.withDefault config.min

                            high =
                                List.drop (index + 1) config.values
                                    |> List.head
                                    |> Maybe.withDefault config.max

                            moved_ =
                                clamp (Basics.max config.min low) (Basics.min config.max high) moved

                            newValues =
                                List.indexedMap
                                    (\i old ->
                                        if i == index then
                                            moved_

                                        else
                                            old
                                    )
                                    config.values
                        in
                        case toMsg newValues of
                            Just msg ->
                                Decode.succeed msg

                            Nothing ->
                                Decode.fail "The slider's value has a different shape than its handler expects."

                    _ ->
                        Decode.fail "Not a number."
            )
