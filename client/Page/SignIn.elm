module Page.SignIn exposing (Model, Msg, Outcome(..), init, outcome, passkeyFailed, passkeyUsed, toasts, update, view)

import Api
import ApiClient
import Ceremony exposing (Ceremony(..))
import Effect exposing (Effect(..))
import Html exposing (Html, form, text)
import Html.Attributes exposing (attribute, class, disabled, type_)
import Html.Events exposing (onClick, onSubmit)
import InteropDefinitions as Interop
import LinkForm exposing (Attempt(..), State(..))
import Notice exposing (Notice)
import Path
import Phosphor exposing (IconWeight(..))
import Plain
import UI.Button as Button
import UI.Field as Field



-- MODEL


type alias Model =
    { email : String
    , state : LinkForm.State
    , ceremony : Ceremony
    }


init : Model
init =
    { email = "", state = Open Editing, ceremony = Resting }



-- UPDATE


type Msg
    = EmailChanged String
    | Submitted
    | Resubmitted
    | GotSignIn (Result ApiClient.Error ())
    | PasskeyRequested
    | PasskeyRetried
    | GotOptions (Result ApiClient.Error Api.SignInOptions)
    | PasskeyUsed Api.Assertion
    | PasskeyFailed String
    | GotPasskeySignIn (Result ApiClient.Error Api.Account)


type Outcome
    = Unchanged
    | SignedIn Api.Account


passkeyUsed : Api.Assertion -> Msg
passkeyUsed =
    PasskeyUsed


passkeyFailed : String -> Msg
passkeyFailed =
    PasskeyFailed


outcome : Msg -> Outcome
outcome msg =
    case msg of
        GotPasskeySignIn (Ok account) ->
            SignedIn account

        _ ->
            Unchanged


update : Msg -> Model -> ( Model, Effect Msg )
update msg model =
    case msg of
        EmailChanged email ->
            ( { model | email = email }, NoEffect )

        Submitted ->
            if model.state == Open Editing then
                ( { model | state = Open Submitting }, sendLink model.email )

            else
                ( model, NoEffect )

        Resubmitted ->
            if model.state == Open Submitting then
                ( model, sendLink model.email )

            else
                ( model, NoEffect )

        GotSignIn (Ok ()) ->
            ( { model | state = Sent }, Dismiss linkToast )

        GotSignIn (Err error) ->
            case error of
                ApiClient.Throttled _ _ ->
                    ( model, linkFailure Resubmitted error )

                _ ->
                    ( { model | state = Open Editing }, linkFailure Submitted error )

        PasskeyRequested ->
            if model.ceremony == Waiting then
                ( model, NoEffect )

            else
                ( { model | ceremony = Waiting }, askOptions )

        PasskeyRetried ->
            if model.ceremony == Waiting then
                ( model, askOptions )

            else
                ( model, NoEffect )

        GotOptions (Ok options) ->
            ( model, SendToJs (Interop.GetPasskey options) )

        GotOptions (Err error) ->
            passkeyFailure error model

        PasskeyUsed assertion ->
            if model.ceremony == Waiting then
                ( model, Request (ApiClient.request Api.passkeySignIn assertion GotPasskeySignIn) )

            else
                ( model, NoEffect )

        PasskeyFailed name ->
            ( { model | ceremony = Resting }, Notify (passkeyProblem name) )

        GotPasskeySignIn (Ok _) ->
            ( model, PushUrl (Path.toString Path.Explore) )

        GotPasskeySignIn (Err error) ->
            passkeyFailure error model


toasts : List String
toasts =
    [ linkToast, passkeyToast ]


linkToast : String
linkToast =
    "sign-in-link"


passkeyToast : String
passkeyToast =
    "passkey-sign-in"


passkeyTitle : String
passkeyTitle =
    "Couldn't log in with a passkey"


sendLink : String -> Effect Msg
sendLink email =
    Request (ApiClient.request Api.signIn { email = email } GotSignIn)


linkFailure : Msg -> ApiClient.Error -> Effect Msg
linkFailure retry =
    Effect.failure { id = linkToast, title = "Couldn't send the link", retry = Just retry }


askOptions : Effect Msg
askOptions =
    Request (ApiClient.request Api.passkeySignInOptions () GotOptions)


passkeyFailure : ApiClient.Error -> Model -> ( Model, Effect Msg )
passkeyFailure error model =
    case error of
        ApiClient.Throttled _ _ ->
            ( model, Effect.failure { id = passkeyToast, title = passkeyTitle, retry = Just PasskeyRetried } error )

        _ ->
            ( { model | ceremony = Resting }
            , Effect.failure { id = passkeyToast, title = passkeyTitle, retry = Just PasskeyRequested } error
            )


passkeyProblem : String -> Notice Msg
passkeyProblem name =
    { id = passkeyToast
    , tone = Notice.Problem
    , title = passkeyTitle
    , description = Just (signInProblem name)
    , retry =
        if name == "NotSupportedError" then
            Notice.NoRetry

        else
            Notice.Retry PasskeyRequested
    }


signInProblem : String -> String
signInProblem name =
    case name of
        "NotAllowedError" ->
            "No passkey was used."

        "NotSupportedError" ->
            "This browser can't use passkeys."

        _ ->
            "The passkey could not be used."



-- VIEW


view : Model -> List (Html Msg)
view { email, state, ceremony } =
    case state of
        Sent ->
            LinkForm.sent ("If " ++ String.trim email ++ " has an account, we sent it a link. It expires in 15 minutes.")

        Open attempt ->
            [ Plain.lead "Log in to your account" "Use your passkey, or get a link by email."
            , form [ onSubmit Submitted ]
                [ Field.group []
                    [ Button.new
                        |> Button.text
                            (if ceremony == Waiting then
                                "Waiting for your device…"

                             else
                                "Log in with a passkey"
                            )
                        |> Button.startIcon (Phosphor.fingerprint Bold |> Phosphor.toHtml [ attribute "aria-hidden" "true" ])
                        |> Button.toHtml
                            [ type_ "button"
                            , onClick PasskeyRequested
                            , disabled (ceremony == Waiting)
                            , class "h-10"
                            ]
                    , Field.separator [] [ text "or" ]
                    , LinkForm.emailField email EmailChanged
                    , Button.new
                        |> Button.text (LinkForm.submitLabel attempt)
                        |> Button.outline
                        |> Button.toHtml [ type_ "submit", disabled (attempt == Submitting), class "h-10" ]
                    ]
                ]
            ]
