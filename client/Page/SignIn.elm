module Page.SignIn exposing (Model, Msg, Outcome(..), init, outcome, passkeyFailed, passkeyUsed, update, view)

import Api
import ApiClient
import Ceremony exposing (Ceremony(..))
import Effect exposing (Effect(..))
import Html exposing (Html, form, text)
import Html.Attributes exposing (attribute, class, disabled, type_)
import Html.Events exposing (onClick, onSubmit)
import InteropDefinitions as Interop
import LinkForm exposing (Attempt(..), State(..))
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
    { email = "", state = Open (Editing Nothing), ceremony = Resting Nothing }



-- UPDATE


type Msg
    = EmailChanged String
    | Submitted
    | GotSignIn (Result ApiClient.Error ())
    | PasskeyRequested
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
            if model.state == Open Submitting then
                ( model, NoEffect )

            else
                ( { model | state = Open Submitting }
                , Request (ApiClient.request Api.signIn { email = model.email } GotSignIn)
                )

        GotSignIn (Ok ()) ->
            ( { model | state = Sent }, NoEffect )

        GotSignIn (Err error) ->
            ( { model | state = Open (Editing (Just (ApiClient.errorToString error))) }, NoEffect )

        PasskeyRequested ->
            if model.ceremony == Waiting then
                ( model, NoEffect )

            else
                ( { model | ceremony = Waiting }
                , Request (ApiClient.request Api.passkeySignInOptions () GotOptions)
                )

        GotOptions (Ok options) ->
            ( model, SendToJs (Interop.GetPasskey options) )

        GotOptions (Err error) ->
            ( { model | ceremony = Resting (Just (ApiClient.errorToString error)) }, NoEffect )

        PasskeyUsed assertion ->
            if model.ceremony == Waiting then
                ( model, Request (ApiClient.request Api.passkeySignIn assertion GotPasskeySignIn) )

            else
                ( model, NoEffect )

        PasskeyFailed name ->
            ( { model | ceremony = Resting (Just (signInProblem name)) }, NoEffect )

        GotPasskeySignIn (Ok _) ->
            ( model, PushUrl (Path.toString Path.Explore) )

        GotPasskeySignIn (Err error) ->
            ( { model | ceremony = Resting (Just (ApiClient.errorToString error)) }, NoEffect )


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
                    [ Ceremony.problem ceremony
                    , Button.new
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
                    , LinkForm.problem attempt
                    , Button.new
                        |> Button.text (LinkForm.submitLabel attempt)
                        |> Button.outline
                        |> Button.toHtml [ type_ "submit", disabled (attempt == Submitting), class "h-10" ]
                    ]
                ]
            ]
