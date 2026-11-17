module Page.Sync exposing (Model, Msg, Outcome(..), init, outcome, toasts, update, view)

import Api exposing (SyncProvider(..))
import ApiClient
import Dict exposing (Dict)
import Effect exposing (Effect(..))
import Html exposing (Html, div, p, span, text)
import Html.Attributes exposing (attribute, class, disabled, href)
import Html.Events exposing (onClick)
import Notice
import Path
import Phosphor exposing (IconWeight(..))
import Session exposing (Session)
import UI.Button as Button
import UI.Card as Card
import UI.Empty as Empty
import UI.Skeleton as Skeleton



-- MODEL


type Busy
    = Connecting
    | Disconnecting


type alias Model =
    { finished : List Api.SyncConnection
    , removed : List SyncProvider
    , returning : Maybe { state : String, code : String }
    , declined : Bool
    , busy : Dict String Busy
    }


blank : Model
blank =
    { finished = []
    , removed = []
    , returning = Nothing
    , declined = False
    , busy = Dict.empty
    }


init : Path.SyncReturn -> ( Model, Effect Msg )
init back =
    case ( back.code, back.state, back.error ) of
        ( Just code, Just state, _ ) ->
            ( { blank | returning = Just { state = state, code = code } }
            , Batch
                [ clearAddress
                , Request (ApiClient.request Api.syncFinish { state = state, code = code } GotFinished)
                ]
            )

        ( _, _, Just _ ) ->
            ( { blank | declined = True }
            , Batch
                [ clearAddress
                , Notify { id = syncToast, tone = Notice.Information, title = "Connection cancelled", description = Nothing, retry = Notice.NoRetry }
                ]
            )

        _ ->
            ( blank, NoEffect )


clearAddress : Effect msg
clearAddress =
    ReplaceUrl (Path.toString (Path.Sync { code = Nothing, state = Nothing, error = Nothing }))



-- UPDATE


type Msg
    = ConnectClicked SyncProvider
    | ConnectRetried SyncProvider
    | GotStart SyncProvider (Result ApiClient.Error Api.SyncStart)
    | GotFinished (Result ApiClient.Error Api.SyncConnection)
    | DisconnectClicked SyncProvider
    | DisconnectRetried SyncProvider
    | GotDisconnected SyncProvider (Result ApiClient.Error ())


type Outcome
    = Unchanged
    | Changed


outcome : Msg -> Outcome
outcome msg =
    case msg of
        GotFinished (Ok _) ->
            Changed

        GotDisconnected _ (Ok ()) ->
            Changed

        ConnectClicked _ ->
            Unchanged

        ConnectRetried _ ->
            Unchanged

        GotStart _ _ ->
            Unchanged

        GotFinished (Err _) ->
            Unchanged

        DisconnectClicked _ ->
            Unchanged

        DisconnectRetried _ ->
            Unchanged

        GotDisconnected _ (Err _) ->
            Unchanged


update : Msg -> Model -> ( Model, Effect Msg )
update msg model =
    case msg of
        ConnectClicked provider ->
            if Dict.member (key provider) model.busy then
                ( model, NoEffect )

            else
                ( { model | busy = Dict.insert (key provider) Connecting model.busy }, startConnecting provider )

        ConnectRetried provider ->
            if Dict.get (key provider) model.busy == Just Connecting then
                ( model, startConnecting provider )

            else
                ( model, NoEffect )

        GotStart provider (Ok started) ->
            if Dict.get (key provider) model.busy == Just Connecting then
                ( model, Load started.url )

            else
                ( model, NoEffect )

        GotStart provider (Err error) ->
            if Dict.get (key provider) model.busy == Just Connecting then
                ( settled provider error model
                , Effect.failure
                    { id = syncToast
                    , title = "Couldn't connect"
                    , retry =
                        Just
                            (case error of
                                ApiClient.Throttled _ _ ->
                                    ConnectRetried provider

                                _ ->
                                    ConnectClicked provider
                            )
                    }
                    error
                )

            else
                ( model, NoEffect )

        GotFinished (Ok connection) ->
            ( { model
                | finished = connection :: List.filter (\other -> other.provider /= connection.provider) model.finished
                , removed = List.filter ((/=) connection.provider) model.removed
                , returning = Nothing
              }
            , Notify { id = syncToast, tone = Notice.Success, title = Api.providerName connection.provider ++ " connected", description = Nothing, retry = Notice.NoRetry }
            )

        GotFinished (Err error) ->
            ( { model | returning = Nothing }
            , Effect.failure { id = syncToast, title = "Couldn't connect", retry = Nothing } error
            )

        DisconnectClicked provider ->
            if Dict.member (key provider) model.busy then
                ( model, NoEffect )

            else
                ( { model | busy = Dict.insert (key provider) Disconnecting model.busy }, startDisconnecting provider )

        DisconnectRetried provider ->
            if Dict.get (key provider) model.busy == Just Disconnecting then
                ( model, startDisconnecting provider )

            else
                ( model, NoEffect )

        GotDisconnected provider (Ok ()) ->
            ( { model
                | removed = provider :: List.filter ((/=) provider) model.removed
                , finished = List.filter (\connection -> connection.provider /= provider) model.finished
                , busy = Dict.remove (key provider) model.busy
              }
            , Notify { id = syncToast, tone = Notice.Success, title = Api.providerName provider ++ " disconnected", description = Nothing, retry = Notice.NoRetry }
            )

        GotDisconnected provider (Err error) ->
            if Dict.get (key provider) model.busy == Just Disconnecting then
                ( settled provider error model
                , Effect.failure
                    { id = syncToast
                    , title = "Couldn't disconnect"
                    , retry =
                        Just
                            (case error of
                                ApiClient.Throttled _ _ ->
                                    DisconnectRetried provider

                                _ ->
                                    DisconnectClicked provider
                            )
                    }
                    error
                )

            else
                ( model, NoEffect )


settled : SyncProvider -> ApiClient.Error -> Model -> Model
settled provider error model =
    case error of
        ApiClient.Throttled _ _ ->
            model

        _ ->
            { model | busy = Dict.remove (key provider) model.busy }


startConnecting : SyncProvider -> Effect Msg
startConnecting provider =
    Request (ApiClient.request (Api.syncStart provider) () (GotStart provider))


startDisconnecting : SyncProvider -> Effect Msg
startDisconnecting provider =
    Request (ApiClient.request (Api.syncStop provider) () (GotDisconnected provider))


key : SyncProvider -> String
key =
    Api.providerToString


toasts : List String
toasts =
    [ syncToast ]


syncToast : String
syncToast =
    "sync"



-- VIEW


view : Session -> Maybe Api.Me -> Model -> List (Html Msg)
view session me model =
    case session of
        Session.SignedOut ->
            [ Empty.new
                |> Empty.icon (Phosphor.cloudArrowUp Bold |> Phosphor.toHtml [ attribute "aria-hidden" "true" ])
                |> Empty.title [ text "Not logged in" ]
                |> Empty.description [ text "Log in to sync your library." ]
                |> Empty.content
                    [ Button.new
                        |> Button.text "Log in"
                        |> Button.small
                        |> Button.toAnchor [ href (Path.toString Path.SignIn) ]
                    ]
                |> Empty.toHtml [ class "border" ]
            ]

        _ ->
            case me of
                Nothing ->
                    List.map (viewProvider Nothing) Api.providers

                Just found ->
                    case found.billing.plan of
                        Api.Free ->
                            [ viewFree ]

                        Api.Pro ->
                            List.map (viewProvider (Just ( connections found model, model.busy ))) Api.providers


connections : Api.Me -> Model -> List Api.SyncConnection
connections me model =
    List.filter (\connection -> not (List.member connection.provider model.removed) && not (List.any (\own -> own.provider == connection.provider) model.finished)) me.sync
        ++ model.finished


viewFree : Html Msg
viewFree =
    Card.new
        |> Card.title [ Html.h3 [] [ text "Cloud sync" ] ]
        |> Card.description [ text "Keeps a copy of your library pictures in Dropbox or Google Drive. Needs Pro." ]
        |> Card.footer [ class "justify-end gap-2" ]
            [ Button.new
                |> Button.text "Get Pro"
                |> Button.small
                |> Button.toAnchor [ href (Path.toString Path.Account) ]
            ]
        |> Card.toHtml cardClasses


viewProvider : Maybe ( List Api.SyncConnection, Dict String Busy ) -> SyncProvider -> Html Msg
viewProvider known provider =
    let
        name =
            Api.providerName provider

        connected =
            known
                |> Maybe.andThen (\( list, _ ) -> List.filter (\connection -> connection.provider == provider) list |> List.head)

        busy =
            known
                |> Maybe.andThen (\( _, pending ) -> Dict.get (key provider) pending)
    in
    Card.new
        |> Card.title [ Html.h3 [] [ text name ] ]
        |> Card.description [ text (description provider) ]
        |> Card.footer [ class "justify-between gap-2" ]
            (case known of
                Nothing ->
                    [ span [ class "flex h-5 items-center", attribute "aria-hidden" "true" ]
                        [ Skeleton.view [ class "h-3.5 w-32" ] ]
                    , Button.new
                        |> Button.text "Connect"
                        |> Button.small
                        |> viewButtonSkeleton []
                    ]

                Just _ ->
                    case connected of
                        Just connection ->
                            [ p [ class "text-sm text-muted-foreground" ] [ text ("Connected " ++ String.left 10 connection.connectedAt) ]
                            , Button.new
                                |> Button.text
                                    (if busy == Just Disconnecting then
                                        "Disconnecting…"

                                     else
                                        "Disconnect"
                                    )
                                |> Button.outline
                                |> Button.small
                                |> Button.toHtml
                                    [ onClick (DisconnectClicked provider)
                                    , disabled (busy /= Nothing)
                                    , attribute "aria-label"
                                        (if busy == Just Disconnecting then
                                            "Disconnecting " ++ name ++ "…"

                                         else
                                            "Disconnect " ++ name
                                        )
                                    ]
                            ]

                        Nothing ->
                            [ span [] []
                            , Button.new
                                |> Button.text
                                    (if busy == Just Connecting then
                                        "Connecting…"

                                     else
                                        "Connect"
                                    )
                                |> Button.small
                                |> Button.toHtml
                                    [ onClick (ConnectClicked provider)
                                    , disabled (busy /= Nothing)
                                    , attribute "aria-label"
                                        (if busy == Just Connecting then
                                            "Connecting " ++ name ++ "…"

                                         else
                                            "Connect " ++ name
                                        )
                                    ]
                            ]
            )
        |> Card.toHtml cardClasses


description : SyncProvider -> String
description provider =
    case provider of
        Dropbox ->
            "Keeps a copy of your library pictures in your Dropbox, in Apps/pixo."

        GoogleDrive ->
            "Keeps a copy of your library pictures in your Google Drive, in a pixo folder."


cardClasses : List (Html.Attribute msg)
cardClasses =
    [ class "w-full [--card-spacing:--spacing(5)] sm:[--card-spacing:--spacing(6)]" ]


viewButtonSkeleton : List (Html.Attribute msg) -> Button.Button shape Button.Ready msg -> Html msg
viewButtonSkeleton attrs button =
    div (class "relative inline-flex" :: attribute "aria-hidden" "true" :: attrs)
        [ Button.toHtml [ class "invisible w-full", disabled True ] button
        , Skeleton.view [ class "absolute inset-0 rounded-lg!" ]
        ]
