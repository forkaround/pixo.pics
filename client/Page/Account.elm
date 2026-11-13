module Page.Account exposing (Model, Msg, Outcome(..), enrolment, init, load, outcome, update, view)

import Api
import ApiClient
import Ceremony exposing (Ceremony(..))
import Data.CredentialId exposing (CredentialId)
import Effect exposing (Effect(..))
import Enrolment
import Html exposing (Html, div, li, p, span, text, ul)
import Html.Attributes exposing (attribute, class, disabled)
import Html.Events exposing (onClick)
import Path
import Phosphor exposing (IconWeight(..))
import RelativeTime
import Session exposing (Session)
import Time
import UI.AlertDialog as AlertDialog
import UI.Button as Button
import UI.Card as Card
import UI.Field as Field
import UI.Item as Item
import UI.Skeleton as Skeleton



-- MODEL


type Passkeys
    = Loading
    | Loaded (List Api.Passkey)
    | NotLoaded String


type alias Model =
    { passkeys : Passkeys
    , confirming : Maybe CredentialId
    , removalProblem : Maybe String
    , deletion : AccountDeletion
    , now : Maybe Time.Posix
    , ceremony : Ceremony
    }


type AccountDeletion
    = KeepAccount
    | ConfirmDeletion (Maybe String)
    | DeletingAccount


init : Model
init =
    { passkeys = Loading
    , confirming = Nothing
    , removalProblem = Nothing
    , deletion = KeepAccount
    , now = Nothing
    , ceremony = Resting Nothing
    }


load : Model -> ( Model, Effect Msg )
load model =
    ( { model | passkeys = Loading }, Request (ApiClient.request Api.passkeys () GotPasskeys) )



-- UPDATE


type Msg
    = GotPasskeys (Result ApiClient.Error (List Api.Passkey))
    | GotTime Time.Posix
    | GotEnrolment Enrolment.Msg
    | RemoveRequested CredentialId
    | RemoveCancelled
    | RemoveConfirmed CredentialId
    | GotRemoved CredentialId (Result ApiClient.Error ())
    | DeleteRequested
    | DeleteCancelled
    | DeleteConfirmed
    | GotDeleted (Result ApiClient.Error ())


type Outcome
    = Unchanged
    | SignedOut


enrolment : Enrolment.Msg -> Msg
enrolment =
    GotEnrolment


outcome : Msg -> Outcome
outcome msg =
    case msg of
        GotDeleted (Ok ()) ->
            SignedOut

        GotDeleted (Err (ApiClient.Unauthorized _)) ->
            SignedOut

        _ ->
            Unchanged


update : Session -> Msg -> Model -> ( Model, Effect Msg )
update session msg model =
    case msg of
        GotPasskeys (Ok found) ->
            ( { model | passkeys = Loaded found }, ReadTime GotTime )

        GotPasskeys (Err error) ->
            ( { model | passkeys = NotLoaded (ApiClient.errorToString error) }, NoEffect )

        GotTime now ->
            ( { model | now = Just now }, NoEffect )

        GotEnrolment enrolmentMsg ->
            let
                ( ceremony, effect, enrolled ) =
                    Enrolment.update enrolmentMsg model.ceremony
            in
            case enrolled of
                Enrolment.Enrolled passkey ->
                    ( { model
                        | ceremony = ceremony
                        , passkeys =
                            case model.passkeys of
                                Loaded found ->
                                    Loaded (found ++ [ passkey ])

                                _ ->
                                    model.passkeys
                      }
                    , ReadTime GotTime
                    )

                Enrolment.Pending ->
                    ( { model | ceremony = ceremony }, Effect.map GotEnrolment effect )

        RemoveRequested id ->
            ( { model | confirming = Just id }, NoEffect )

        RemoveCancelled ->
            ( { model | confirming = Nothing }, NoEffect )

        RemoveConfirmed id ->
            ( { model | confirming = Nothing, removalProblem = Nothing }
            , Request (ApiClient.request (Api.deletePasskey id) () (GotRemoved id))
            )

        GotRemoved id (Ok ()) ->
            ( case model.passkeys of
                Loaded found ->
                    { model | passkeys = Loaded (List.filter (\passkey -> passkey.id /= id) found) }

                _ ->
                    model
            , NoEffect
            )

        GotRemoved _ (Err error) ->
            ( { model | removalProblem = Just (ApiClient.errorToString error) }, NoEffect )

        DeleteRequested ->
            ( { model | deletion = ConfirmDeletion Nothing }, NoEffect )

        DeleteCancelled ->
            if model.deletion == DeletingAccount then
                ( model, NoEffect )

            else
                ( { model | deletion = KeepAccount }, NoEffect )

        DeleteConfirmed ->
            case ( session, model.deletion ) of
                ( Session.SignedIn _, ConfirmDeletion _ ) ->
                    ( { model | deletion = DeletingAccount }
                    , Request (ApiClient.request Api.deleteAccount () GotDeleted)
                    )

                _ ->
                    ( model, NoEffect )

        GotDeleted (Ok ()) ->
            ( { model | ceremony = Resting Nothing }, PushUrl (Path.toString Path.Explore) )

        GotDeleted (Err (ApiClient.Unauthorized _)) ->
            ( { model | ceremony = Resting Nothing }, PushUrl (Path.toString Path.SignIn) )

        GotDeleted (Err error) ->
            ( { model | deletion = ConfirmDeletion (Just (ApiClient.errorToString error)) }, NoEffect )



-- VIEW


view : Session -> Model -> List (Html Msg)
view session account =
    case session of
        Session.SignedIn _ ->
            [ viewPasskeys session account.ceremony account, viewDeleteAccount (Just account.deletion) ]

        Session.Unknown ->
            [ viewPasskeys session (Resting Nothing) { account | passkeys = Loading, removalProblem = Nothing }
            , viewDeleteAccount Nothing
            ]

        Session.Unavailable _ ->
            []

        Session.SignedOut ->
            []


viewDeleteAccount : Maybe AccountDeletion -> Html Msg
viewDeleteAccount deletion =
    div []
        [ Card.new
            |> Card.title [ Html.h3 [ class "text-destructive" ] [ text "Delete account" ] ]
            |> Card.description [ text "Permanently delete your account, passkeys and sessions. This cannot be undone." ]
            |> Card.content []
                [ p [ class "text-sm text-muted-foreground" ] [ text "Images saved in this browser will remain." ] ]
            |> Card.footer [ class "justify-end" ]
                [ Button.new
                    |> Button.text "Delete account"
                    |> Button.destructive
                    |> Button.small
                    |> (if deletion == Nothing then
                            viewButtonSkeleton []

                        else
                            Button.toHtml [ onClick DeleteRequested, disabled (deletion == Just DeletingAccount) ]
                       )
                ]
            |> Card.toHtml [ class "w-full ring-destructive/40! [--card-spacing:--spacing(5)] sm:[--card-spacing:--spacing(6)]" ]
        , viewDeleteAccountDialog deletion
        ]


viewDeleteAccountDialog : Maybe AccountDeletion -> Html Msg
viewDeleteAccountDialog deletion =
    AlertDialog.new
        |> AlertDialog.id "delete-account"
        |> AlertDialog.open (deletion /= Nothing && deletion /= Just KeepAccount) DeleteCancelled
        |> AlertDialog.busy (deletion == Just DeletingAccount)
        |> AlertDialog.small
        |> AlertDialog.media [ class "bg-destructive/10 text-destructive dark:bg-destructive/20" ]
            [ Phosphor.trash Regular |> Phosphor.toHtml [ attribute "aria-hidden" "true" ] ]
        |> AlertDialog.title [ text "Are you sure?" ]
        |> AlertDialog.description
            [ text "This will permanently delete your account and cannot be undone."
            , case deletion of
                Just (ConfirmDeletion (Just reason)) ->
                    span [ class "mt-3 block text-destructive", attribute "role" "alert" ] [ text reason ]

                _ ->
                    text ""
            ]
        |> AlertDialog.cancel
            (Button.new
                |> Button.text "Cancel"
                |> Button.outline
                |> Button.small
            )
        |> AlertDialog.action DeleteConfirmed
            (Button.new
                |> Button.text
                    (if deletion == Just DeletingAccount then
                        "Deleting…"

                     else
                        "Delete"
                    )
                |> Button.destructive
                |> Button.small
            )
        |> AlertDialog.toHtml []


viewPasskeys : Session -> Ceremony -> Model -> Html Msg
viewPasskeys session ceremony { passkeys, confirming, removalProblem, now } =
    Card.new
        |> Card.title [ Html.h3 [] [ text "Passkeys" ] ]
        |> Card.description [ text "Sign in with your fingerprint, face or screen lock." ]
        |> Card.content []
            [ div [ class "flex flex-col gap-3" ]
                [ div [ class "max-h-48 overflow-y-auto" ]
                    [ case passkeys of
                        Loading ->
                            ul [ class "flex flex-col gap-2", attribute "aria-hidden" "true" ]
                                [ li []
                                    [ Item.new
                                        |> Item.muted
                                        |> Item.icon (Skeleton.view [ class "size-4" ])
                                        |> Item.title [ Skeleton.view [ class "h-[1.375em] w-28 max-w-full" ] ]
                                        |> Item.actions
                                            [ Skeleton.view [ class "h-4 w-16" ]
                                            , Skeleton.view [ class "size-7" ]
                                            ]
                                        |> Item.toHtml [ class "flex-nowrap! [&_[data-slot=item-content]]:min-w-0 [&_[data-slot=item-title]]:w-full" ]
                                    ]
                                ]

                        Loaded [] ->
                            Item.new
                                |> Item.outline
                                |> Item.icon (Phosphor.fingerprint Regular |> Phosphor.toHtml [ attribute "aria-hidden" "true" ])
                                |> Item.title [ span [ class "flex h-7 items-center" ] [ text "No passkeys yet." ] ]
                                |> Item.toHtml [ class "border-dashed" ]

                        Loaded found ->
                            ul [ class "flex flex-col gap-2" ] (List.map (viewPasskey now confirming) found)

                        NotLoaded message ->
                            p [ class "text-sm text-muted-foreground" ] [ text message ]
                    ]
                , Ceremony.problem ceremony
                , case removalProblem of
                    Just reason ->
                        Field.error [] [ text reason ]

                    Nothing ->
                        text ""
                ]
            ]
        |> Card.footer [ class "flex-col items-start gap-3 sm:flex-row sm:items-center sm:justify-between" ]
            [ p [ class "text-xs text-muted-foreground" ] [ text "You can still log in with an email link." ]
            , if session == Session.Unknown then
                Button.new
                    |> Button.text "Add a passkey"
                    |> Button.startIcon (Phosphor.plus Bold |> Phosphor.toHtml [ attribute "aria-hidden" "true" ])
                    |> Button.small
                    |> viewButtonSkeleton [ class "max-sm:w-full" ]

              else
                Button.new
                    |> Button.text
                        (if ceremony == Waiting then
                            "Waiting for your device…"

                         else
                            "Add a passkey"
                        )
                    |> Button.startIcon (Phosphor.plus Bold |> Phosphor.toHtml [ attribute "aria-hidden" "true" ])
                    |> Button.small
                    |> Button.toHtml [ onClick (GotEnrolment Enrolment.requested), disabled (ceremony == Waiting), class "max-sm:w-full" ]
            ]
        |> Card.toHtml [ class "w-full [--card-spacing:--spacing(5)] sm:[--card-spacing:--spacing(6)]" ]


viewButtonSkeleton : List (Html.Attribute msg) -> Button.Button shape Button.Ready msg -> Html msg
viewButtonSkeleton attrs button =
    div (class "relative inline-flex" :: attribute "aria-hidden" "true" :: attrs)
        [ Button.toHtml [ class "invisible w-full", disabled True ] button
        , Skeleton.view [ class "absolute inset-0 rounded-lg!" ]
        ]


viewPasskey : Maybe Time.Posix -> Maybe CredentialId -> Api.Passkey -> Html Msg
viewPasskey now confirming passkey =
    let
        name =
            Maybe.withDefault "Passkey" passkey.name
    in
    li []
        [ Item.new
            |> Item.muted
            |> Item.icon (Phosphor.fingerprint Regular |> Phosphor.toHtml [ attribute "aria-hidden" "true" ])
            |> Item.title
                [ span [ class "truncate", Html.Attributes.title name ]
                    [ text
                        (if confirming == Just passkey.id then
                            "Remove " ++ name ++ "?"

                         else
                            name
                        )
                    ]
                ]
            |> Item.actions
                (if confirming == Just passkey.id then
                    [ Button.new
                        |> Button.text "Cancel"
                        |> Button.ghost
                        |> Button.small
                        |> Button.toHtml [ onClick RemoveCancelled ]
                    , Button.new
                        |> Button.text "Remove"
                        |> Button.destructive
                        |> Button.small
                        |> Button.toHtml [ onClick (RemoveConfirmed passkey.id) ]
                    ]

                 else
                    [ case now of
                        Just current ->
                            Html.time
                                [ attribute "datetime" passkey.createdAt
                                , Html.Attributes.title ("Added " ++ passkey.createdAt)
                                , class "whitespace-nowrap text-xs text-muted-foreground"
                                ]
                                [ text (RelativeTime.toString current passkey.createdAt) ]

                        Nothing ->
                            Skeleton.view [ class "h-4 w-16", attribute "aria-hidden" "true" ]
                    , Button.new
                        |> Button.icon (Phosphor.trash Regular |> Phosphor.toHtml [ attribute "aria-hidden" "true" ])
                        |> Button.label ("Remove " ++ name)
                        |> Button.ghost
                        |> Button.small
                        |> Button.toHtml [ onClick (RemoveRequested passkey.id) ]
                    ]
                )
            |> Item.toHtml [ class "flex-nowrap! [&_[data-slot=item-content]]:min-w-0 [&_[data-slot=item-title]]:w-full" ]
        ]
