module UI.Toast exposing
    ( Msg
    , NeedsText
    , Ready
    , Toast
    , Toaster
    , action
    , add
    , close
    , closeAll
    , description
    , error
    , highPriority
    , id
    , info
    , init
    , loading
    , new
    , subscriptions
    , success
    , timeout
    , title
    , update
    , view
    , warning
    )

import Browser.Dom
import Browser.Events
import Html exposing (Html)
import Html.Attributes exposing (attribute, class, tabindex)
import Html.Events
import Html.Keyed
import Json.Decode as Decode exposing (Decoder)
import Process
import Task
import Time
import UI.Button as Button



-- TOAST


type Toast status msg
    = Toast (Content msg)


type alias Content msg =
    { id : Maybe String
    , title : Maybe String
    , description : Maybe String
    , kind : Maybe Kind
    , action : Maybe ( String, msg )
    , timeout : Float
    , highPriority : Bool
    }


type Kind
    = Success
    | Info
    | Warning
    | Error
    | Loading


type Ready
    = Ready


type NeedsText
    = NeedsText


new : Toast NeedsText msg
new =
    Toast
        { id = Nothing
        , title = Nothing
        , description = Nothing
        , kind = Nothing
        , action = Nothing
        , timeout = 5000
        , highPriority = False
        }


title : String -> Toast status msg -> Toast Ready msg
title text (Toast content) =
    Toast { content | title = Just text }


description : String -> Toast status msg -> Toast Ready msg
description text (Toast content) =
    Toast { content | description = Just text }


success : Toast status msg -> Toast status msg
success =
    kind Success


info : Toast status msg -> Toast status msg
info =
    kind Info


warning : Toast status msg -> Toast status msg
warning =
    kind Warning


error : Toast status msg -> Toast status msg
error =
    kind Error


loading : Toast status msg -> Toast status msg
loading =
    kind Loading


kind : Kind -> Toast status msg -> Toast status msg
kind value (Toast content) =
    Toast { content | kind = Just value }


action : String -> msg -> Toast status msg -> Toast status msg
action label msg (Toast content) =
    Toast { content | action = Just ( label, msg ) }


id : String -> Toast status msg -> Toast status msg
id name (Toast content) =
    Toast { content | id = Just name }


timeout : Float -> Toast status msg -> Toast status msg
timeout milliseconds (Toast content) =
    Toast { content | timeout = milliseconds }


highPriority : Toast status msg -> Toast status msg
highPriority (Toast content) =
    Toast { content | highPriority = True }



-- MESSAGES


add : Toast Ready msg -> Msg msg
add (Toast content) =
    Add content


close : String -> Msg msg
close =
    Close


closeAll : Msg msg
closeAll =
    CloseAll



-- TOASTER


type Toaster msg
    = Toaster (Model msg)


type alias Model msg =
    { toasts : List (Item msg)
    , counter : Int
    , hovering : Bool
    , leaving : Bool
    , touching : Bool
    , focused : Bool
    , pointerFocus : Bool
    , visible : Bool
    , closeFocus : Maybe Int
    }


type alias Item msg =
    { key : Int
    , content : Content msg
    , phase : Phase
    , height : Maybe Int
    , measuring : Bool
    , timer : Maybe Timer
    , swipe : Maybe Swipe
    , movement : Point
    , swipeDirection : Maybe Direction
    }


type Phase
    = Starting
    | Open
    | Ending


type alias Timer =
    { delay : Float
    , remaining : Float
    , startedAt : Maybe Int
    , generation : Int
    }


type alias Swipe =
    { pointer : Int
    , mouse : Bool
    , start : Point
    , baseline : Point
    , firstMove : Bool
    , real : Bool
    , lock : Maybe Axis
    , intended : Maybe Direction
    , maxDisplacement : Float
    , cancelled : Bool
    }


type alias Point =
    { x : Float, y : Float }


type Direction
    = Up
    | Down
    | Left
    | Right


type Axis
    = Horizontal
    | Vertical


init : Toaster msg
init =
    Toaster
        { toasts = []
        , counter = 0
        , hovering = False
        , leaving = False
        , touching = False
        , focused = False
        , pointerFocus = False
        , visible = True
        , closeFocus = Nothing
        }


limit : Int
limit =
    3


swipeDirections : List Direction
swipeDirections =
    [ Down, Right ]


swipeThreshold : Float
swipeThreshold =
    40


reverseCancelThreshold : Float
reverseCancelThreshold =
    10


type Msg msg
    = Add (Content msg)
    | Close String
    | CloseAll
    | Dismiss Int
    | Measure
    | Measured Int (Result Browser.Dom.Error Int)
    | Remove Int
    | Sync Int
    | Expire Int Int
    | PointerEnter
    | PointerLeave
    | PressOutside
    | ViewportPointerDown String
    | ViewportPointerUp
    | ViewportKeyDown
    | FocusIn
    | FocusOut
    | VisibilityChange Browser.Events.Visibility
    | CloseFocus (Maybe Int)
    | SwipeStart Int Pointer
    | SwipeMove Int Move
    | SwipeEnd Int Bool
    | NoOp


type alias Pointer =
    { id : Int, mouse : Bool, position : Point }


type alias Move =
    { position : Point, movement : Point }



-- UPDATE


update : (Msg msg -> msg) -> Msg msg -> Toaster msg -> ( Toaster msg, Cmd msg )
update toMsg msg (Toaster model) =
    let
        ( next, cmd ) =
            step msg model
    in
    ( Toaster next
    , Cmd.map toMsg
        (if timersOutOfStep next then
            Cmd.batch [ cmd, Task.perform (Time.posixToMillis >> Sync) Time.now ]

         else
            cmd
        )
    )


step : Msg msg -> Model msg -> ( Model msg, Cmd (Msg msg) )
step msg model =
    case msg of
        Add content ->
            case content.id |> Maybe.andThen (\name -> find (\item -> item.content.id == Just name) model.toasts) of
                Just item ->
                    if item.phase == Ending then
                        open content { model | toasts = List.filter (\other -> other.key /= item.key) model.toasts }

                    else
                        ( mapItem item.key (\current -> { current | content = content, timer = timerFor content, measuring = True }) model
                        , measure item.key content
                        )

                Nothing ->
                    open content model

        Close name ->
            closeWhere (\item -> item.content.id == Just name) model

        CloseAll ->
            closeWhere (\_ -> True) model

        Dismiss key ->
            closeWhere (\item -> item.key == key) model

        Measure ->
            let
                showing =
                    List.filter (\item -> item.phase /= Ending) model.toasts
            in
            ( { model
                | toasts =
                    List.map
                        (\item ->
                            if item.phase == Ending then
                                item

                            else
                                { item | measuring = True }
                        )
                        model.toasts
              }
            , Cmd.batch (List.map (\item -> measure item.key item.content) showing)
            )

        Measured key result ->
            ( mapItem key
                (\item ->
                    case ( item.phase, result ) of
                        ( Ending, _ ) ->
                            { item | measuring = False }

                        ( _, Ok height ) ->
                            { item | height = Just height, measuring = False, phase = Open }

                        ( _, Err _ ) ->
                            { item | measuring = False, phase = Open }
                )
                model
            , Cmd.none
            )

        Remove key ->
            ( flushLeave { model | toasts = List.filter (\item -> item.key /= key || item.phase /= Ending) model.toasts }
            , Cmd.none
            )

        Sync now ->
            sync now model

        Expire key generation ->
            if running model && List.any (expiresWith key generation) model.toasts then
                closeWhere (\item -> item.key == key) model

            else
                ( model, Cmd.none )

        PointerEnter ->
            ( { model | hovering = True, leaving = False }, Cmd.none )

        PointerLeave ->
            if List.any (\item -> item.swipe /= Nothing) model.toasts then
                ( model, Cmd.none )

            else
                ( flushLeave { model | leaving = True }, Cmd.none )

        PressOutside ->
            ( { model | hovering = False, focused = False, leaving = False, touching = False }, Cmd.none )

        ViewportPointerDown pointerType ->
            ( { model | pointerFocus = True, touching = model.touching || pointerType == "touch" }, Cmd.none )

        ViewportPointerUp ->
            ( flushLeave { model | touching = False }, Cmd.none )

        ViewportKeyDown ->
            ( { model | pointerFocus = False }, Cmd.none )

        FocusIn ->
            if model.pointerFocus then
                ( { model | pointerFocus = False }, Cmd.none )

            else
                ( { model | focused = True }, Cmd.none )

        FocusOut ->
            ( { model | focused = False }, Cmd.none )

        VisibilityChange visibility ->
            ( { model | visible = visibility == Browser.Events.Visible }, Cmd.none )

        CloseFocus key ->
            ( { model | closeFocus = key }, Cmd.none )

        SwipeStart key pointer ->
            ( mapItem key
                (\item ->
                    if item.phase == Ending then
                        item

                    else
                        { item
                            | swipe =
                                Just
                                    { pointer = pointer.id
                                    , mouse = pointer.mouse
                                    , start = pointer.position
                                    , baseline = pointer.position
                                    , firstMove = True
                                    , real = False
                                    , lock = Nothing
                                    , intended = Nothing
                                    , maxDisplacement = 0
                                    , cancelled = False
                                    }
                            , movement = origin
                        }
                )
                { model | hovering = True }
            , Cmd.none
            )

        SwipeMove key move ->
            ( mapItem key (swipeMove move) model, Cmd.none )

        SwipeEnd key cancelled ->
            case find (\item -> item.key == key && item.swipe /= Nothing) model.toasts of
                Just swiped ->
                    case
                        if cancelled || Maybe.map .cancelled swiped.swipe == Just True then
                            Nothing

                        else
                            find (\direction -> displacement direction swiped.movement > swipeThreshold) swipeDirections
                    of
                        Just direction ->
                            closeWhere (\item -> item.key == key)
                                (mapItem key (\item -> { item | swipe = Nothing, swipeDirection = Just direction }) model)

                        Nothing ->
                            ( flushLeave (mapItem key (\item -> { item | swipe = Nothing, movement = origin, swipeDirection = Nothing }) model)
                            , Cmd.none
                            )

                Nothing ->
                    ( model, Cmd.none )

        NoOp ->
            ( model, Cmd.none )


open : Content msg -> Model msg -> ( Model msg, Cmd (Msg msg) )
open content model =
    ( { model
        | toasts =
            { key = model.counter
            , content = content
            , phase = Starting
            , height = Nothing
            , measuring = True
            , timer = timerFor content
            , swipe = Nothing
            , movement = origin
            , swipeDirection = Nothing
            }
                :: model.toasts
        , counter = model.counter + 1
      }
    , measure model.counter content
    )


timerFor : Content msg -> Maybe Timer
timerFor content =
    if content.kind /= Just Loading && content.timeout > 0 then
        Just { delay = content.timeout, remaining = content.timeout, startedAt = Nothing, generation = 0 }

    else
        Nothing


closeWhere : (Item msg -> Bool) -> Model msg -> ( Model msg, Cmd (Msg msg) )
closeWhere matches model =
    let
        closing =
            List.filter (\item -> matches item && item.phase /= Ending) model.toasts
    in
    case closing of
        [] ->
            ( model, Cmd.none )

        _ ->
            let
                toasts =
                    List.map
                        (\item ->
                            if matches item && item.phase /= Ending then
                                { item | phase = Ending, timer = Nothing, swipe = Nothing }

                            else
                                item
                        )
                        model.toasts

                empty =
                    List.all (\item -> item.phase == Ending) toasts
            in
            ( { model
                | toasts = toasts
                , hovering = model.hovering && not empty
                , focused = model.focused && not empty
                , leaving = model.leaving && not empty
              }
            , Cmd.batch
                ((case ( model.focused, closing ) of
                    ( True, [ item ] ) ->
                        focusNext item.key toasts

                    _ ->
                        Cmd.none
                 )
                    :: List.map (\item -> Task.perform (\_ -> Remove item.key) (Process.sleep 1000)) closing
                )
            )


focusNext : Int -> List (Item msg) -> Cmd (Msg msg)
focusNext key toasts =
    let
        placed =
            place toasts

        before =
            takeWhile (\p -> p.item.key /= key) placed

        after =
            List.drop (List.length before + 1) placed
    in
    case find (\p -> p.item.phase /= Ending && not p.limited) (after ++ List.reverse before) of
        Just p ->
            Task.attempt (\_ -> NoOp) (Browser.Dom.focus (rootId p.item.key))

        Nothing ->
            Cmd.none


flushLeave : Model msg -> Model msg
flushLeave model =
    if model.leaving && not model.touching && not (List.any (\item -> item.phase == Ending || item.swipe /= Nothing) model.toasts) then
        { model | hovering = False, leaving = False }

    else
        model


running : Model msg -> Bool
running model =
    not (model.hovering || model.focused || not model.visible)


timersOutOfStep : Model msg -> Bool
timersOutOfStep model =
    List.any
        (\item ->
            case item.timer of
                Just timer ->
                    (timer.startedAt == Nothing) == running model

                Nothing ->
                    False
        )
        model.toasts


expiresWith : Int -> Int -> Item msg -> Bool
expiresWith key generation item =
    case item.timer of
        Just timer ->
            item.key == key && timer.generation == generation && timer.startedAt /= Nothing

        Nothing ->
            False


sync : Int -> Model msg -> ( Model msg, Cmd (Msg msg) )
sync now model =
    let
        run =
            running model

        ( toasts, counter, cmds ) =
            List.foldr
                (\item ( items, generation, commands ) ->
                    case item.timer of
                        Just timer ->
                            case ( run, timer.startedAt ) of
                                ( True, Nothing ) ->
                                    let
                                        remaining =
                                            if timer.remaining > 0 then
                                                timer.remaining

                                            else
                                                timer.delay
                                    in
                                    ( { item | timer = Just { timer | remaining = remaining, startedAt = Just now, generation = generation } } :: items
                                    , generation + 1
                                    , Task.perform (\_ -> Expire item.key generation) (Process.sleep remaining) :: commands
                                    )

                                ( False, Just started ) ->
                                    ( { item | timer = Just { timer | remaining = max 0 (timer.remaining - toFloat (now - started)), startedAt = Nothing } } :: items
                                    , generation
                                    , commands
                                    )

                                _ ->
                                    ( item :: items, generation, commands )

                        Nothing ->
                            ( item :: items, generation, commands )
                )
                ( [], model.counter, [] )
                model.toasts
    in
    ( { model | toasts = toasts, counter = counter }, Cmd.batch cmds )


measure : Int -> Content msg -> Cmd (Msg msg)
measure key content =
    let
        label =
            labelId key content
    in
    Task.map3
        (\root text box ->
            let
                scale =
                    if box.viewport.height > 0 then
                        text.element.height / box.viewport.height

                    else
                        1
            in
            round (root.element.height / scale)
        )
        (Browser.Dom.getElement (rootId key))
        (Browser.Dom.getElement label)
        (Browser.Dom.getViewportOf label)
        |> Task.attempt (Measured key)


mapItem : Int -> (Item msg -> Item msg) -> Model msg -> Model msg
mapItem key f model =
    { model
        | toasts =
            List.map
                (\item ->
                    if item.key == key then
                        f item

                    else
                        item
                )
                model.toasts
    }



-- SWIPE


origin : Point
origin =
    { x = 0, y = 0 }


swipeMove : Move -> Item msg -> Item msg
swipeMove move item =
    case item.swipe of
        Just current ->
            let
                started =
                    if current.firstMove then
                        { current | start = move.position, firstMove = False }

                    else
                        current

                baseline =
                    { x = followBack move.position.x started.baseline.x move.movement.x
                    , y = followBack move.position.y started.baseline.y move.movement.y
                    }

                delta =
                    { x = move.position.x - started.start.x, y = move.position.y - started.start.y }

                ( real, lock ) =
                    if not started.real && sqrt (delta.x ^ 2 + delta.y ^ 2) >= 1 then
                        ( True
                        , Just
                            (if abs delta.x > abs delta.y then
                                Horizontal

                             else
                                Vertical
                            )
                        )

                    else
                        ( started.real, started.lock )

                swipe =
                    { started | baseline = baseline, real = real, lock = lock }

                ( next, direction ) =
                    case swipe.intended of
                        Nothing ->
                            case candidate lock delta of
                                Just picked ->
                                    if List.member picked swipeDirections then
                                        ( { swipe | intended = Just picked, maxDisplacement = displacement picked delta }, Just picked )

                                    else
                                        ( swipe, item.swipeDirection )

                                Nothing ->
                                    ( swipe, item.swipeDirection )

                        Just intended ->
                            let
                                progress =
                                    displacement intended { x = move.position.x - baseline.x, y = move.position.y - baseline.y }
                            in
                            if progress > swipeThreshold then
                                ( { swipe | cancelled = False }, Just intended )

                            else if swipe.maxDisplacement - progress >= reverseCancelThreshold then
                                ( { swipe | cancelled = True }, item.swipeDirection )

                            else
                                ( swipe, item.swipeDirection )

                damped =
                    { x = damp delta.x Left Right, y = damp delta.y Up Down }
            in
            { item
                | swipe = Just next
                , swipeDirection = direction
                , movement =
                    { x =
                        if lock == Just Vertical then
                            0

                        else
                            damped.x
                    , y =
                        if lock == Just Horizontal then
                            0

                        else
                            damped.y
                    }
            }

        Nothing ->
            item


candidate : Maybe Axis -> Point -> Maybe Direction
candidate lock delta =
    case lock of
        Just Vertical ->
            if delta.y == 0 then
                Nothing

            else if delta.y > 0 then
                Just Down

            else
                Just Up

        Just Horizontal ->
            if delta.x == 0 then
                Nothing

            else if delta.x > 0 then
                Just Right

            else
                Just Left

        Nothing ->
            if abs delta.x >= abs delta.y then
                if delta.x > 0 then
                    Just Right

                else
                    Just Left

            else if delta.y > 0 then
                Just Down

            else
                Just Up


followBack : Float -> Float -> Float -> Float
followBack position baseline movement =
    if (movement < 0 && position > baseline) || (movement > 0 && position < baseline) then
        position

    else
        baseline


displacement : Direction -> Point -> Float
displacement direction delta =
    case direction of
        Up ->
            -delta.y

        Down ->
            delta.y

        Left ->
            -delta.x

        Right ->
            delta.x


damp : Float -> Direction -> Direction -> Float
damp delta negative positive =
    if (delta < 0 && not (List.member negative swipeDirections)) || (delta > 0 && not (List.member positive swipeDirections)) then
        (if delta < 0 then
            -1

         else
            1
        )
            * sqrt (abs delta)

    else
        delta



-- SUBSCRIPTIONS


subscriptions : (Msg msg -> msg) -> Toaster msg -> Sub msg
subscriptions toMsg (Toaster model) =
    Sub.map toMsg
        (Sub.batch
            (Browser.Events.onVisibilityChange VisibilityChange
                :: List.concat
                    [ when (not (List.isEmpty model.toasts)) (Browser.Events.onResize (\_ _ -> Measure))
                    , when model.hovering (Browser.Events.onMouseMove (outsideViewport PointerLeave))
                    , when (model.hovering || model.focused) (Browser.Events.onMouseDown (outsideViewport PressOutside))
                    , case find (\item -> Maybe.map .mouse item.swipe == Just True) model.toasts of
                        Just item ->
                            [ Browser.Events.onMouseMove (Decode.map (SwipeMove item.key) moveDecoder)
                            , Browser.Events.onMouseUp (Decode.succeed (SwipeEnd item.key False))
                            ]

                        Nothing ->
                            []
                    ]
            )
        )


when : Bool -> a -> List a
when condition value =
    if condition then
        [ value ]

    else
        []



-- VIEW


view : (Msg msg -> msg) -> Toaster msg -> Html msg
view toMsg (Toaster model) =
    let
        expanded =
            model.hovering || model.focused

        urgent =
            List.filter (\item -> item.content.highPriority) model.toasts
    in
    Html.div [ attribute "data-slot" "toast-portal" ]
        (Html.Keyed.node "div"
            (List.concat
                [ [ tabindex -1
                  , attribute "role" "region"
                  , attribute "aria-live" "polite"
                  , attribute "aria-atomic" "false"
                  , attribute "aria-relevant" "additions text"
                  , attribute "aria-label" "Notifications"
                  , attribute "data-slot" "toast-viewport"
                  , class "pointer-events-none fixed inset-x-4 bottom-4 z-50 mx-auto w-auto max-w-sm outline-none sm:right-4 sm:left-auto sm:mx-0 sm:w-full"
                  ]
                , frontmostHeight model.toasts
                    |> Maybe.map (\height -> [ attribute "style" ("--toast-frontmost-height: " ++ px (toFloat height) ++ ";") ])
                    |> Maybe.withDefault []
                , when expanded (attribute "data-expanded" "")
                , [ Html.Events.onMouseEnter (toMsg PointerEnter)
                  , Html.Events.onMouseLeave (toMsg PointerLeave)
                  , Html.Events.on "focusin" (Decode.succeed (toMsg FocusIn))
                  , Html.Events.on "focusout" (Decode.map toMsg focusOutDecoder)
                  , Html.Events.on "pointerdown" (Decode.map (ViewportPointerDown >> toMsg) (Decode.field "pointerType" Decode.string))
                  ]
                , when (not model.hovering || model.leaving) (Html.Events.on "mousemove" (Decode.succeed (toMsg PointerEnter)))
                , if model.touching then
                    [ Html.Events.on "pointerup" (Decode.succeed (toMsg ViewportPointerUp))
                    , Html.Events.on "pointercancel" (Decode.succeed (toMsg ViewportPointerUp))
                    ]

                  else
                    []
                , when model.pointerFocus (Html.Events.on "keydown" (Decode.succeed (toMsg ViewportKeyDown)))
                ]
            )
            (List.map (\placed -> ( String.fromInt placed.item.key, viewToast toMsg model expanded placed )) (place model.toasts))
            :: (if not model.focused && not (List.isEmpty urgent) then
                    [ viewAnnouncements urgent ]

                else
                    []
               )
        )


type alias Placed msg =
    { item : Item msg
    , index : Int
    , behind : Bool
    , offset : Int
    , limited : Bool
    }


place : List (Item msg) -> List (Placed msg)
place toasts =
    let
        ( placed, _ ) =
            List.foldl
                (\( position, item ) ( list, ( visible, offset ) ) ->
                    if item.phase == Ending then
                        ( { item = item, index = position, behind = False, offset = offset, limited = False } :: list
                        , ( visible, offset )
                        )

                    else
                        ( { item = item, index = visible, behind = visible > 0, offset = offset, limited = visible >= limit } :: list
                        , ( visible + 1, offset + Maybe.withDefault 0 item.height )
                        )
                )
                ( [], ( 0, 0 ) )
                (List.indexedMap Tuple.pair toasts)
    in
    List.reverse placed


frontmostHeight : List (Item msg) -> Maybe Int
frontmostHeight toasts =
    case toasts of
        front :: rest ->
            case ( front.phase, front.height ) of
                ( Ending, _ ) ->
                    Nothing

                ( Starting, Nothing ) ->
                    List.head rest
                        |> Maybe.andThen
                            (\next ->
                                if next.phase == Ending then
                                    Nothing

                                else
                                    next.height
                            )

                ( _, height ) ->
                    height

        [] ->
            Nothing


viewToast : (Msg msg -> msg) -> Model msg -> Bool -> Placed msg -> Html msg
viewToast toMsg model expanded { item, index, behind, offset, limited } =
    let
        content =
            item.content

        typeAttribute =
            Maybe.map (\value -> attribute "data-type" (kindName value)) content.kind
                |> Maybe.map List.singleton
                |> Maybe.withDefault []
    in
    Html.div
        (List.concat
            [ [ Html.Attributes.id (rootId item.key) ]
            , typeAttribute
            , [ attribute "role"
                    (if content.highPriority then
                        "alertdialog"

                     else
                        "dialog"
                    )
              , tabindex 0
              , attribute "aria-modal" "false"
              ]
            , when (content.highPriority && not model.focused) (attribute "aria-hidden" "true")
            , [ attribute "data-slot" "toast"
              , class "group/toast pointer-events-auto absolute right-0 bottom-0 z-[calc(1000-var(--toast-index))] w-full origin-bottom rounded-2xl border bg-popover text-popover-foreground shadow-lg will-change-transform outline-none select-none focus-visible:border-ring focus-visible:ring-[3px] focus-visible:ring-ring/50 [--gap:0.75rem] [--height:var(--toast-frontmost-height,var(--toast-height))] [--offset-y:calc(var(--toast-offset-y)*-1+calc(var(--toast-index)*var(--gap)*-1)+var(--toast-swipe-movement-y))] [--peek:0.75rem] [--scale:calc(max(0,1-(var(--toast-index)*0.1)))] [--shrink:calc(1-var(--scale))] h-(--height) [transform:translateX(var(--toast-swipe-movement-x))_translateY(calc(var(--toast-swipe-movement-y)-(var(--toast-index)*var(--peek))-(var(--shrink)*var(--height))))_scale(var(--scale))] [transition:transform_500ms_cubic-bezier(0.22,1,0.36,1),opacity_500ms,height_150ms] after:absolute after:top-full after:left-0 after:h-[calc(var(--gap)+1px)] after:w-full after:content-[''] data-expanded:h-(--toast-height) data-expanded:[transform:translateX(var(--toast-swipe-movement-x))_translateY(var(--offset-y))] data-limited:opacity-0 data-starting-style:[transform:translateY(150%)] [&[data-ending-style]:not([data-limited]):not([data-swipe-direction])]:[transform:translateY(150%)] data-ending-style:data-[swipe-direction=down]:[transform:translateY(calc(var(--toast-swipe-movement-y)+150%))] data-ending-style:data-[swipe-direction=left]:[transform:translateX(calc(var(--toast-swipe-movement-x)-150%))_translateY(var(--offset-y))] data-ending-style:data-[swipe-direction=right]:[transform:translateX(calc(var(--toast-swipe-movement-x)+150%))_translateY(var(--offset-y))] data-ending-style:data-[swipe-direction=up]:[transform:translateY(calc(var(--toast-swipe-movement-y)-150%))] data-expanded:data-ending-style:data-[swipe-direction=down]:[transform:translateY(calc(var(--toast-swipe-movement-y)+150%))] data-expanded:data-ending-style:data-[swipe-direction=left]:[transform:translateX(calc(var(--toast-swipe-movement-x)-150%))_translateY(var(--offset-y))] data-expanded:data-ending-style:data-[swipe-direction=right]:[transform:translateX(calc(var(--toast-swipe-movement-x)+150%))_translateY(var(--offset-y))] data-expanded:data-ending-style:data-[swipe-direction=up]:[transform:translateY(calc(var(--toast-swipe-movement-y)-150%))]"
              ]
            , Maybe.map (\_ -> attribute "aria-labelledby" (titleId item.key)) content.title
                |> Maybe.map List.singleton
                |> Maybe.withDefault []
            , Maybe.map (\_ -> attribute "aria-describedby" (descriptionId item.key)) content.description
                |> Maybe.map List.singleton
                |> Maybe.withDefault []
            , [ attribute "style" (styleFor item index offset) ]
            , when expanded (attribute "data-expanded" "")
            , when limited (attribute "data-limited" "")
            , when limited (attribute "inert" "")
            , when (item.swipe /= Nothing) (attribute "data-swiping" "")
            , Maybe.map (\direction -> [ attribute "data-swipe-direction" (directionName direction) ]) item.swipeDirection
                |> Maybe.withDefault []
            , case item.phase of
                Starting ->
                    [ attribute "data-starting-style" "" ]

                Open ->
                    []

                Ending ->
                    [ attribute "data-ending-style" ""
                    , Html.Events.on "transitionend" (Decode.map toMsg (transitionEndDecoder item.key))
                    ]
            , [ Html.Events.on "keydown" (Decode.map toMsg (escapeDecoder item.key))
              , Html.Events.on "click" (Decode.map toMsg (actionClickDecoder item.key))
              ]
            , when (item.phase /= Ending) (Html.Events.on "pointerdown" (Decode.map (SwipeStart item.key >> toMsg) swipeStartDecoder))
            , case item.swipe of
                Just swipe ->
                    if swipe.mouse then
                        []

                    else
                        [ Html.Events.on "pointermove" (Decode.map (SwipeMove item.key >> toMsg) (pointerMoveDecoder swipe.pointer))
                        , Html.Events.on "pointerup" (Decode.succeed (toMsg (SwipeEnd item.key False)))
                        , Html.Events.on "pointercancel" (Decode.succeed (toMsg (SwipeEnd item.key True)))
                        , Html.Events.preventDefaultOn "touchmove" (Decode.succeed ( toMsg NoOp, True ))
                        ]

                Nothing ->
                    []
            ]
        )
        [ Html.div
            (List.concat
                [ [ attribute "data-slot" "toast-content"
                  , class "flex h-full items-center gap-3 overflow-hidden p-4 transition-opacity duration-250 ease-[cubic-bezier(0.22,1,0.36,1)] data-behind:opacity-0 data-expanded:opacity-100"
                  ]
                , when expanded (attribute "data-expanded" "")
                , when behind (attribute "data-behind" "")
                ]
            )
            (List.filterMap identity
                [ Maybe.map viewIcon content.kind
                , Just
                    (Html.div [ class "flex min-w-0 flex-1 flex-col gap-1" ]
                        (List.filterMap identity
                            [ Maybe.map
                                (\text ->
                                    Html.h2 (typeAttribute ++ [ attribute "data-slot" "toast-title", Html.Attributes.id (titleId item.key), class "text-sm font-medium" ])
                                        [ Html.text text ]
                                )
                                content.title
                            , Maybe.map
                                (\text ->
                                    Html.p (typeAttribute ++ [ attribute "data-slot" "toast-description", Html.Attributes.id (descriptionId item.key), class "text-sm text-muted-foreground" ])
                                        [ Html.text text ]
                                )
                                content.description
                            ]
                        )
                    )
                , Maybe.map
                    (\( label, msg ) ->
                        Button.new
                            |> Button.text label
                            |> Button.outline
                            |> Button.small
                            |> Button.toHtml
                                (tabindex 0
                                    :: attribute "data-slot" "toast-action"
                                    :: typeAttribute
                                    ++ [ class "shrink-0", Html.Events.onClick msg ]
                                )
                    )
                    content.action
                , Just
                    (Button.new
                        |> Button.icon (maskIcon "" xIcon)
                        |> Button.label "Close toast"
                        |> Button.ghost
                        |> Button.small
                        |> Button.toHtml
                            (tabindex 0
                                :: attribute "data-slot" "toast-close"
                                :: attribute "aria-hidden"
                                    (if not expanded && model.closeFocus /= Just item.key then
                                        "true"

                                     else
                                        "false"
                                    )
                                :: typeAttribute
                                ++ [ class "relative shrink-0 text-muted-foreground after:absolute after:-inset-2 after:content-[''] hover:text-foreground"
                                   , Html.Events.onClick (toMsg (Dismiss item.key))
                                   , Html.Events.onFocus (toMsg (CloseFocus (Just item.key)))
                                   , Html.Events.onBlur (toMsg (CloseFocus Nothing))
                                   ]
                            )
                    )
                ]
            )
        ]


styleFor : Item msg -> Int -> Int -> String
styleFor item index offset =
    String.join " "
        (List.concat
            [ when (item.swipe /= Nothing) "transition: none;"
            , [ "--toast-swipe-movement-x: " ++ px item.movement.x ++ ";"
              , "--toast-swipe-movement-y: " ++ px item.movement.y ++ ";"
              , "--toast-index: " ++ String.fromInt index ++ ";"
              , "--toast-offset-y: " ++ px (toFloat offset) ++ ";"
              ]
            , case ( item.phase, item.height ) of
                ( Ending, _ ) ->
                    []

                ( _, Just height ) ->
                    [ "--toast-height: " ++ px (toFloat height) ++ ";" ]

                ( _, Nothing ) ->
                    []
            , when item.measuring "height: auto;"
            ]
        )


px : Float -> String
px value =
    String.fromFloat value ++ "px"


viewAnnouncements : List (Item msg) -> Html msg
viewAnnouncements urgent =
    Html.Keyed.node "div"
        [ attribute "style" "clip-path: inset(50%); overflow: hidden; white-space: nowrap; border: 0px; padding: 0px; width: 1px; height: 1px; margin: -1px; position: fixed; top: 0px; left: 0px;" ]
        (List.map
            (\item ->
                ( String.fromInt item.key
                , Html.div [ attribute "role" "alert", attribute "aria-atomic" "true" ]
                    [ Html.div [] [ Html.text (Maybe.withDefault "" item.content.title) ]
                    , Html.div [] [ Html.text (Maybe.withDefault "" item.content.description) ]
                    ]
                )
            )
            urgent
        )


rootId : Int -> String
rootId key =
    "toast-" ++ String.fromInt key


titleId : Int -> String
titleId key =
    rootId key ++ "-title"


descriptionId : Int -> String
descriptionId key =
    rootId key ++ "-description"


labelId : Int -> Content msg -> String
labelId key content =
    case content.title of
        Just _ ->
            titleId key

        Nothing ->
            descriptionId key


kindName : Kind -> String
kindName value =
    case value of
        Success ->
            "success"

        Info ->
            "info"

        Warning ->
            "warning"

        Error ->
            "error"

        Loading ->
            "loading"


directionName : Direction -> String
directionName direction =
    case direction of
        Up ->
            "up"

        Down ->
            "down"

        Left ->
            "left"

        Right ->
            "right"



-- ICONS


viewIcon : Kind -> Html msg
viewIcon value =
    Html.span
        [ attribute "data-slot" "toast-icon"
        , class "shrink-0 [&_svg]:pointer-events-none [&_svg:not([class*='size-'])]:size-4"
        ]
        [ case value of
            Success ->
                maskIcon "" "<circle cx='12' cy='12' r='10'/><path d='m9 12 2 2 4-4'/>"

            Info ->
                maskIcon "" "<circle cx='12' cy='12' r='10'/><path d='M12 16v-4'/><path d='M12 8h.01'/>"

            Warning ->
                maskIcon "" "<path d='m21.73 18-8-14a2 2 0 0 0-3.48 0l-8 14A2 2 0 0 0 4 21h16a2 2 0 0 0 1.73-3'/><path d='M12 9v4'/><path d='M12 17h.01'/>"

            Error ->
                maskIcon "text-destructive" "<path d='m15 9-6 6'/><path d='M2.586 16.726A2 2 0 0 1 2 15.312V8.688a2 2 0 0 1 .586-1.414l4.688-4.688A2 2 0 0 1 8.688 2h6.624a2 2 0 0 1 1.414.586l4.688 4.688A2 2 0 0 1 22 8.688v6.624a2 2 0 0 1-.586 1.414l-4.688 4.688a2 2 0 0 1-1.414.586H8.688a2 2 0 0 1-1.414-.586z'/><path d='m9 9 6 6'/>"

            Loading ->
                maskIcon "animate-spin" "<path d='M21 12a9 9 0 1 1-6.219-8.56'/>"
        ]


xIcon : String
xIcon =
    "<path d='M18 6 6 18'/><path d='m6 6 12 12'/>"


maskIcon : String -> String -> Html msg
maskIcon classes paths =
    let
        url =
            "url(\"data:image/svg+xml,%3Csvg xmlns='http://www.w3.org/2000/svg' viewBox='0 0 24 24' fill='none' stroke='black' stroke-width='2' stroke-linecap='round' stroke-linejoin='round'%3E"
                ++ String.replace ">" "%3E" (String.replace "<" "%3C" paths)
                ++ "%3C/svg%3E\") center / contain no-repeat"
    in
    Html.span
        [ attribute "aria-hidden" "true"
        , class "block size-4 shrink-0 bg-current"
        , class classes
        , attribute "style" ("-webkit-mask: " ++ url ++ "; mask: " ++ url ++ ";")
        ]
        []



-- DECODERS


swipeStartDecoder : Decoder Pointer
swipeStartDecoder =
    Decode.map3
        (\button interactive pointer ->
            if button == 0 && not interactive then
                Just pointer

            else
                Nothing
        )
        (Decode.field "button" Decode.int)
        (Decode.field "target" interactiveDecoder)
        (Decode.map3 Pointer
            (Decode.field "pointerId" Decode.int)
            (Decode.map ((==) "mouse") (Decode.field "pointerType" Decode.string))
            positionDecoder
        )
        |> Decode.andThen (Maybe.map Decode.succeed >> Maybe.withDefault (Decode.fail "not a swipe"))


interactiveDecoder : Decoder Bool
interactiveDecoder =
    Decode.map3
        (\tag role slot -> ( List.member tag [ "BUTTON", "A", "INPUT", "TEXTAREA" ] || role == "button", slot == "toast" ))
        (optionalString [ "tagName" ])
        (optionalString [ "role" ])
        (optionalString [ "dataset", "slot" ])
        |> Decode.andThen
            (\( interactive, root ) ->
                if interactive then
                    Decode.succeed True

                else if root then
                    Decode.succeed False

                else
                    Decode.oneOf
                        [ Decode.field "parentElement" (Decode.lazy (\_ -> interactiveDecoder))
                        , Decode.succeed False
                        ]
            )


outsideViewport : Msg msg -> Decoder (Msg msg)
outsideViewport msg =
    Decode.field "target" (Decode.nullable insideViewport)
        |> Decode.andThen
            (\inside ->
                if inside == Just True then
                    Decode.fail "inside the viewport"

                else
                    Decode.succeed msg
            )


insideViewport : Decoder Bool
insideViewport =
    optionalString [ "dataset", "slot" ]
        |> Decode.andThen
            (\slot ->
                if slot == "toast-viewport" then
                    Decode.succeed True

                else
                    Decode.oneOf
                        [ Decode.field "parentElement" (Decode.nullable (Decode.lazy (\_ -> insideViewport)))
                            |> Decode.map (Maybe.withDefault False)
                        , Decode.succeed False
                        ]
            )


focusOutDecoder : Decoder (Msg msg)
focusOutDecoder =
    Decode.field "relatedTarget" (Decode.nullable insideViewport)
        |> Decode.andThen
            (\inside ->
                if inside == Just True then
                    Decode.fail "focus stays in the viewport"

                else
                    Decode.succeed FocusOut
            )


optionalString : List String -> Decoder String
optionalString path =
    Decode.oneOf [ Decode.at path Decode.string, Decode.succeed "" ]


positionDecoder : Decoder Point
positionDecoder =
    Decode.map2 Point (Decode.field "clientX" Decode.float) (Decode.field "clientY" Decode.float)


moveDecoder : Decoder Move
moveDecoder =
    Decode.map2 Move
        positionDecoder
        (Decode.map2 Point (Decode.field "movementX" Decode.float) (Decode.field "movementY" Decode.float))


pointerMoveDecoder : Int -> Decoder Move
pointerMoveDecoder pointer =
    Decode.field "pointerId" Decode.int
        |> Decode.andThen
            (\current ->
                if current == pointer then
                    moveDecoder

                else
                    Decode.fail "another pointer"
            )


escapeDecoder : Int -> Decoder (Msg msg)
escapeDecoder key =
    Decode.field "key" Decode.string
        |> Decode.andThen
            (\pressed ->
                if pressed == "Escape" then
                    Decode.succeed (Dismiss key)

                else
                    Decode.fail "not Escape"
            )


actionClickDecoder : Int -> Decoder (Msg msg)
actionClickDecoder key =
    optionalString [ "target", "dataset", "slot" ]
        |> Decode.andThen
            (\slot ->
                if slot == "toast-action" then
                    Decode.succeed (Dismiss key)

                else
                    Decode.fail "not the action"
            )


transitionEndDecoder : Int -> Decoder (Msg msg)
transitionEndDecoder key =
    Decode.map2 Tuple.pair
        (Decode.field "propertyName" Decode.string)
        (optionalString [ "target", "dataset", "slot" ])
        |> Decode.andThen
            (\( property, slot ) ->
                if property == "transform" && slot == "toast" then
                    Decode.succeed (Remove key)

                else
                    Decode.fail "another transition"
            )



-- LISTS


find : (a -> Bool) -> List a -> Maybe a
find predicate list =
    case list of
        first :: rest ->
            if predicate first then
                Just first

            else
                find predicate rest

        [] ->
            Nothing


takeWhile : (a -> Bool) -> List a -> List a
takeWhile predicate list =
    case list of
        first :: rest ->
            if predicate first then
                first :: takeWhile predicate rest

            else
                []

        [] ->
            []
