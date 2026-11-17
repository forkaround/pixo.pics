module Page.Account exposing (Model, Msg, Outcome(..), checkoutClosed, enrolment, init, load, outcome, toasts, update, view)

import Api
import ApiClient
import Ceremony exposing (Ceremony(..))
import Data.CredentialId exposing (CredentialId)
import Effect exposing (Effect(..))
import Enrolment
import Html exposing (Html, div, li, p, span, text, ul)
import Html.Attributes exposing (attribute, class, disabled, href)
import Html.Events exposing (onClick)
import InteropDefinitions as Interop
import Path
import Phosphor exposing (IconWeight(..))
import RelativeTime
import Session exposing (Session)
import Time
import UI.AlertDialog as AlertDialog
import UI.Button as Button
import UI.Card as Card
import UI.Empty as Empty
import UI.Item as Item
import UI.Skeleton as Skeleton



-- MODEL


type Passkeys
    = Loading
    | Loaded (List Api.Passkey)


type alias Model =
    { passkeys : Passkeys
    , confirming : Maybe CredentialId
    , deletion : AccountDeletion
    , now : Maybe Time.Posix
    , ceremony : Ceremony
    , billing : BillingAction
    }


type BillingAction
    = Idle
    | Upgrading
    | Managing


type AccountDeletion
    = KeepAccount
    | ConfirmDeletion
    | DeletingAccount
    | WaitingToDelete


init : Model
init =
    { passkeys = Loading
    , confirming = Nothing
    , deletion = KeepAccount
    , now = Nothing
    , ceremony = Resting
    , billing = Idle
    }


load : Model -> ( Model, Effect Msg )
load model =
    ( { model | passkeys = Loading }, Request (ApiClient.request Api.passkeys () GotPasskeys) )



-- UPDATE


type Msg
    = GotPasskeys (Result ApiClient.Error (List Api.Passkey))
    | PasskeysRetried
    | GotTime Time.Posix
    | GotEnrolment Enrolment.Msg
    | RemoveRequested CredentialId
    | RemoveCancelled
    | RemoveConfirmed CredentialId
    | GotRemoved CredentialId (Result ApiClient.Error ())
    | DeleteRequested
    | DeleteCancelled
    | DeleteConfirmed
    | DeleteRetried
    | GotDeleted (Result ApiClient.Error ())
    | UpgradeClicked
    | GotCheckout (Result ApiClient.Error Api.Checkout)
    | ManageClicked
    | GotPortal (Result ApiClient.Error Api.Portal)
    | CheckoutClosed


type Outcome
    = Unchanged
    | SignedOut


enrolment : Enrolment.Msg -> Msg
enrolment =
    GotEnrolment


checkoutClosed : Msg
checkoutClosed =
    CheckoutClosed


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
            ( { model | passkeys = Loaded found }, Batch [ ReadTime GotTime, Dismiss passkeysToast ] )

        GotPasskeys (Err (ApiClient.Unauthorized _)) ->
            ( model, NoEffect )

        GotPasskeys (Err error) ->
            ( model
            , Effect.failure { id = passkeysToast, title = "Can't load passkeys", retry = Just PasskeysRetried } error
            )

        PasskeysRetried ->
            if model.passkeys == Loading then
                load model

            else
                ( model, NoEffect )

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
                    , Batch [ Effect.map GotEnrolment effect, ReadTime GotTime ]
                    )

                Enrolment.Pending ->
                    ( { model | ceremony = ceremony }, Effect.map GotEnrolment effect )

        RemoveRequested id ->
            ( { model | confirming = Just id }, NoEffect )

        RemoveCancelled ->
            ( { model | confirming = Nothing }, NoEffect )

        RemoveConfirmed id ->
            ( { model | confirming = Nothing }
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

        GotRemoved id (Err error) ->
            ( model
            , Effect.failure { id = removalToast, title = "Couldn't remove passkey", retry = Just (RemoveConfirmed id) } error
            )

        DeleteRequested ->
            if model.deletion == KeepAccount then
                ( { model | deletion = ConfirmDeletion }, NoEffect )

            else
                ( model, NoEffect )

        DeleteCancelled ->
            if model.deletion == ConfirmDeletion then
                ( { model | deletion = KeepAccount }, NoEffect )

            else
                ( model, NoEffect )

        DeleteConfirmed ->
            case ( session, model.deletion ) of
                ( Session.SignedIn _, ConfirmDeletion ) ->
                    ( { model | deletion = DeletingAccount }, deleteAccount )

                _ ->
                    ( model, NoEffect )

        DeleteRetried ->
            case ( session, model.deletion ) of
                ( Session.SignedIn _, WaitingToDelete ) ->
                    ( model, deleteAccount )

                _ ->
                    ( model, NoEffect )

        GotDeleted (Ok ()) ->
            ( { model | ceremony = Resting }, PushUrl (Path.toString Path.Explore) )

        GotDeleted (Err (ApiClient.Unauthorized _)) ->
            ( { model | ceremony = Resting }, PushUrl (Path.toString Path.SignIn) )

        GotDeleted (Err error) ->
            case error of
                ApiClient.Throttled _ _ ->
                    ( { model | deletion = WaitingToDelete }, deletionFailure DeleteRetried error )

                _ ->
                    ( { model | deletion = KeepAccount }, deletionFailure DeleteRequested error )

        UpgradeClicked ->
            if model.billing == Idle then
                ( { model | billing = Upgrading }, Request (ApiClient.request Api.checkout () GotCheckout) )

            else
                ( model, NoEffect )

        GotCheckout (Ok checkout) ->
            ( model
            , SendToJs
                (Interop.OpenCheckout
                    { transactionId = checkout.transactionId
                    , clientToken = checkout.clientToken
                    , environment = checkout.environment
                    }
                )
            )

        GotCheckout (Err error) ->
            ( { model | billing = Idle }
            , Effect.failure { id = checkoutToast, title = "Couldn't start the checkout", retry = Just UpgradeClicked } error
            )

        CheckoutClosed ->
            ( { model | billing = Idle }, NoEffect )

        ManageClicked ->
            if model.billing == Idle then
                ( { model | billing = Managing }, Request (ApiClient.request Api.portal () GotPortal) )

            else
                ( model, NoEffect )

        GotPortal (Ok opened) ->
            ( model, Load opened.url )

        GotPortal (Err error) ->
            ( { model | billing = Idle }
            , Effect.failure { id = portalToast, title = "Couldn't open billing", retry = Just ManageClicked } error
            )


toasts : List String
toasts =
    [ passkeysToast, removalToast, deletionToast, checkoutToast, portalToast, Enrolment.toast ]


checkoutToast : String
checkoutToast =
    "checkout"


portalToast : String
portalToast =
    "portal"


passkeysToast : String
passkeysToast =
    "passkeys"


removalToast : String
removalToast =
    "passkey-removal"


deletionToast : String
deletionToast =
    "account-deletion"


deleteAccount : Effect Msg
deleteAccount =
    Request (ApiClient.request Api.deleteAccount () GotDeleted)


deletionFailure : Msg -> ApiClient.Error -> Effect Msg
deletionFailure retry =
    Effect.failure { id = deletionToast, title = "Couldn't delete account", retry = Just retry }



-- VIEW


view : Session -> Maybe Api.Billing -> Model -> List (Html Msg)
view session billing account =
    case session of
        Session.SignedIn _ ->
            [ viewPro billing account.billing
            , viewPasskeys session account.ceremony account
            , viewDeleteAccount (Just account.deletion)
            ]

        Session.Unknown ->
            [ viewPro Nothing Idle
            , viewPasskeys session Resting { account | passkeys = Loading }
            , viewDeleteAccount Nothing
            ]

        Session.SignedOut ->
            [ Empty.new
                |> Empty.icon (Phosphor.userCircle Bold |> Phosphor.toHtml [ attribute "aria-hidden" "true" ])
                |> Empty.title [ text "Not logged in" ]
                |> Empty.description [ text "Log in to manage your passkeys and account." ]
                |> Empty.content
                    [ Button.new
                        |> Button.text "Log in"
                        |> Button.small
                        |> Button.toAnchor [ href (Path.toString Path.SignIn) ]
                    ]
                |> Empty.toHtml [ class "border" ]
            ]


viewPro : Maybe Api.Billing -> BillingAction -> Html Msg
viewPro billing action =
    Card.new
        |> Card.title [ Html.h3 [] [ text "Pro" ] ]
        |> Card.description
            [ case billing of
                Nothing ->
                    span [ class "flex h-5 items-center", attribute "aria-hidden" "true" ]
                        [ Skeleton.view [ class "h-3.5 w-80 max-w-full" ] ]

                Just found ->
                    text
                        (case found.plan of
                            Api.Free ->
                                "$5 a month. Publish to your profile and the feed, and sync to Dropbox or Google Drive."

                            Api.Pro ->
                                "You have Pro."
                        )
            ]
        |> Card.content []
            (case billing of
                Just found ->
                    if found.plan == Api.Pro then
                        List.filterMap identity
                            [ Just (p [ class "text-sm text-muted-foreground" ] [ text (proDates found) ])
                            , if found.pastDue then
                                Just (p [ class "text-sm text-muted-foreground" ] [ text "Your last payment failed. Update your payment method in Manage." ])

                              else
                                Nothing
                            ]

                    else
                        []

                Nothing ->
                    []
            )
        |> Card.footer [ class "justify-end gap-2" ]
            (case billing of
                Nothing ->
                    [ Button.new
                        |> Button.text "Upgrade"
                        |> Button.small
                        |> viewButtonSkeleton []
                    ]

                Just found ->
                    case found.plan of
                        Api.Free ->
                            List.filterMap identity
                                [ if found.canManage then
                                    Just (viewManageButton "Invoices" True action)

                                  else
                                    Nothing
                                , Just
                                    (Button.new
                                        |> Button.text
                                            (if action == Upgrading then
                                                "Upgrading…"

                                             else
                                                "Upgrade"
                                            )
                                        |> Button.small
                                        |> Button.toHtml [ onClick UpgradeClicked, disabled (action /= Idle) ]
                                    )
                                ]

                        Api.Pro ->
                            [ viewManageButton "Manage" False action ]
            )
        |> Card.toHtml [ class "w-full [--card-spacing:--spacing(5)] sm:[--card-spacing:--spacing(6)]" ]


viewManageButton : String -> Bool -> BillingAction -> Html Msg
viewManageButton label outlined action =
    Button.new
        |> Button.text
            (if action == Managing then
                "Managing…"

             else
                label
            )
        |> (if outlined then
                Button.outline

            else
                identity
           )
        |> Button.small
        |> Button.toHtml [ onClick ManageClicked, disabled (action /= Idle) ]


proDates : Api.Billing -> String
proDates found =
    case ( found.endsAt, found.renewsAt ) of
        ( Just ends, _ ) ->
            "Ends on " ++ String.left 10 ends ++ "."

        ( Nothing, Just renews ) ->
            "Renews on " ++ String.left 10 renews ++ "."

        ( Nothing, Nothing ) ->
            ""


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
                    |> Button.text
                        (if deletion == Just DeletingAccount || deletion == Just WaitingToDelete then
                            "Deleting account…"

                         else
                            "Delete account"
                        )
                    |> Button.destructive
                    |> Button.small
                    |> Button.startIcon (Phosphor.trash Bold |> Phosphor.withClass "size-4" |> Phosphor.toHtml [ attribute "aria-hidden" "true" ])
                    |> (if deletion == Nothing then
                            viewButtonSkeleton []

                        else
                            Button.toHtml [ onClick DeleteRequested, disabled (deletion == Just DeletingAccount || deletion == Just WaitingToDelete) ]
                       )
                ]
            |> Card.toHtml [ class "w-full ring-destructive/40! [--card-spacing:--spacing(5)] sm:[--card-spacing:--spacing(6)]" ]
        , viewDeleteAccountDialog deletion
        ]


viewDeleteAccountDialog : Maybe AccountDeletion -> Html Msg
viewDeleteAccountDialog deletion =
    AlertDialog.new
        |> AlertDialog.id "delete-account"
        |> AlertDialog.open (deletion == Just ConfirmDeletion || deletion == Just DeletingAccount) DeleteCancelled
        |> AlertDialog.busy (deletion == Just DeletingAccount)
        |> AlertDialog.small
        |> AlertDialog.media [ class "bg-destructive/10 text-destructive dark:bg-destructive/20" ]
            [ Phosphor.trash Regular |> Phosphor.toHtml [ attribute "aria-hidden" "true" ] ]
        |> AlertDialog.title [ text "Are you sure?" ]
        |> AlertDialog.description
            [ text "This will permanently delete your account and cannot be undone." ]
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
viewPasskeys session ceremony { passkeys, confirming, now } =
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
                    ]
                ]
            ]
        |> Card.footer [ class "flex-col items-start gap-3 sm:flex-row sm:items-center sm:justify-between" ]
            [ p [ class "text-sm text-muted-foreground" ] [ text "You can still log in with an email link." ]
            , if session == Session.Unknown then
                Button.new
                    |> Button.text "Add a passkey"
                    |> Button.outline
                    |> Button.small
                    |> Button.startIcon (Phosphor.plus Bold |> Phosphor.withClass "size-4" |> Phosphor.toHtml [ attribute "aria-hidden" "true" ])
                    |> viewButtonSkeleton [ class "max-sm:w-full" ]

              else
                Button.new
                    |> Button.text
                        (if ceremony == Waiting then
                            "Waiting for your device…"

                         else
                            "Add a passkey"
                        )
                    |> Button.outline
                    |> Button.small
                    |> Button.startIcon (Phosphor.plus Bold |> Phosphor.withClass "size-4" |> Phosphor.toHtml [ attribute "aria-hidden" "true" ])
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
