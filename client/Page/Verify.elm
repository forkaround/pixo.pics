module Page.Verify exposing (Model, Msg, Outcome(..), init, outcome, update, view)

import Api
import ApiClient
import Data.Email as Email
import Data.LinkToken exposing (LinkToken)
import Data.Username as Username
import Effect exposing (Effect(..))
import Html exposing (Html, dd, div, dl, dt, p, text)
import Html.Attributes exposing (attribute, class, disabled, href)
import Html.Events exposing (onClick)
import Path
import Plain
import UI.Avatar as Avatar
import UI.Button as Button
import UI.Skeleton as Skeleton



-- MODEL


type Model
    = Looking LinkToken
    | Found LinkToken Api.Verification Acceptance
    | Refused String


type Acceptance
    = Asking
    | Accepting


init : Maybe LinkToken -> ( Model, Effect Msg )
init link =
    case link of
        Just token ->
            ( Looking token
            , Request
                (ApiClient.request Api.verification () GotVerification
                    |> ApiClient.withToken token
                )
            )

        Nothing ->
            ( Refused missingToken, NoEffect )


missingToken : String
missingToken =
    "This link is incomplete. Open the one from your email again."



-- UPDATE


type Msg
    = GotVerification (Result ApiClient.Error Api.Verification)
    | Accepted
    | GotAccount (Result ApiClient.Error Api.Account)
    | GotPasskeyCheck (Result ApiClient.Error (List Api.Passkey))


type Outcome
    = Unchanged
    | SignedIn Api.Account


outcome : Msg -> Outcome
outcome msg =
    case msg of
        GotAccount (Ok account) ->
            SignedIn account

        _ ->
            Unchanged


update : Msg -> Model -> ( Model, Effect Msg )
update msg model =
    case ( msg, model ) of
        ( GotVerification (Ok found), Looking token ) ->
            ( Found token found Asking, NoEffect )

        ( GotVerification (Err error), Looking _ ) ->
            ( Refused (ApiClient.errorToString error), NoEffect )

        ( Accepted, Found token found Asking ) ->
            ( Found token found Accepting
            , Request
                (ApiClient.request Api.verify () GotAccount
                    |> ApiClient.withToken token
                )
            )

        ( GotAccount (Ok _), Found _ _ Accepting ) ->
            ( model, Request (ApiClient.request Api.passkeys () GotPasskeyCheck) )

        ( GotAccount (Err error), Found _ _ Accepting ) ->
            ( Refused (ApiClient.errorToString error), NoEffect )

        ( GotPasskeyCheck result, Found _ _ Accepting ) ->
            ( model
            , PushUrl
                (Path.toString
                    (case result of
                        Ok [] ->
                            Path.Passkey

                        _ ->
                            Path.Explore
                    )
                )
            )

        _ ->
            ( model, NoEffect )



-- VIEW


view : Model -> List (Html Msg)
view model =
    case model of
        Looking _ ->
            [ Plain.lead "Checking your link" "This takes a moment."
            , div [ class "flex flex-col gap-4", attribute "aria-hidden" "true" ]
                [ viewIdentity
                    (Skeleton.view [ class "size-10 shrink-0 rounded-full!" ])
                    [ Skeleton.view [ class "h-3.5 w-16" ]
                    , Skeleton.view [ class "mt-1.5 h-3.5 w-40" ]
                    ]
                , Skeleton.view [ class "h-10 w-full" ]
                , Skeleton.view [ class "my-0.5 h-4 w-40" ]
                ]
            ]

        Found _ found acceptance ->
            viewFound found acceptance

        Refused reason ->
            [ Plain.lead "This link didn't work" reason
            , div [ class "flex flex-col gap-4" ]
                [ Button.new
                    |> Button.text "Sign up again"
                    |> Button.toAnchor [ href (Path.toString Path.SignUp), class "h-10" ]
                , Button.new
                    |> Button.text "Log in"
                    |> Button.outline
                    |> Button.toAnchor [ href (Path.toString Path.SignIn), class "h-10" ]
                ]
            ]


viewFound : Api.Verification -> Acceptance -> List (Html Msg)
viewFound found acceptance =
    let
        username =
            Username.toString found.username

        words =
            if found.newAccount then
                { heading = "Create this account?"
                , body = "This creates your account and signs you in."
                , accept = "Create my account"
                , accepting = "Creating your account…"
                }

            else
                { heading = "Log in to this account?"
                , body = "This signs you in on this device."
                , accept = "Log in"
                , accepting = "Logging in…"
                }
    in
    [ Plain.lead words.heading words.body
    , div [ class "flex flex-col gap-4" ]
        [ viewIdentity
            (Avatar.new
                |> Avatar.fallback [ text (String.toUpper (String.left 2 username)) ]
                |> Avatar.large
                |> Avatar.toHtml [ attribute "aria-hidden" "true" ]
            )
            [ dl [ class "grid text-sm leading-tight" ]
                [ dt [ class "sr-only" ] [ text "Username" ]
                , dd [ class "truncate font-medium" ] [ text username ]
                , dt [ class "sr-only" ] [ text "Email" ]
                , dd [ class "break-all text-muted-foreground" ] [ text (Email.toString found.email) ]
                ]
            ]
        , Button.new
            |> Button.text
                (if acceptance == Accepting then
                    words.accepting

                 else
                    words.accept
                )
            |> Button.toHtml [ onClick Accepted, disabled (acceptance == Accepting), class "h-10" ]
        , p [ class "text-sm text-muted-foreground" ] [ text "Not you? Close this page." ]
        ]
    ]


viewIdentity : Html Msg -> List (Html Msg) -> Html Msg
viewIdentity avatar details =
    div [ class "flex items-center gap-3 rounded-xl border p-3" ]
        [ avatar
        , div [ class "min-w-0 flex-1" ] details
        ]
