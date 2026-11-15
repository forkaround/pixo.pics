module Page.SignUp exposing (Model, Msg, init, toasts, update, view)

import Api
import ApiClient
import Effect exposing (Effect(..))
import Html exposing (Html, form, text)
import Html.Attributes exposing (attribute, class, for, id, name, required, value)
import Html.Events exposing (onInput, onSubmit)
import LinkForm exposing (Attempt(..), State(..))
import Phosphor exposing (IconWeight(..))
import Plain
import UI.Field as Field
import UI.InputGroup as InputGroup



-- MODEL


type alias Model =
    { email : String
    , username : String
    , state : LinkForm.State
    }


init : Model
init =
    { email = "", username = "", state = Open Editing }



-- UPDATE


type Msg
    = EmailChanged String
    | UsernameChanged String
    | Submitted
    | Resubmitted
    | GotSignUp (Result ApiClient.Error ())


update : Msg -> Model -> ( Model, Effect Msg )
update msg model =
    case msg of
        EmailChanged email ->
            ( { model | email = email }, NoEffect )

        UsernameChanged username ->
            ( { model | username = username }, NoEffect )

        Submitted ->
            if model.state == Open Editing then
                ( { model | state = Open Submitting }, sendLink model )

            else
                ( model, NoEffect )

        Resubmitted ->
            if model.state == Open Submitting then
                ( model, sendLink model )

            else
                ( model, NoEffect )

        GotSignUp (Ok ()) ->
            ( { model | state = Sent }, Dismiss linkToast )

        GotSignUp (Err error) ->
            case error of
                ApiClient.Throttled _ _ ->
                    ( model, failure Resubmitted error )

                _ ->
                    ( { model | state = Open Editing }, failure Submitted error )


toasts : List String
toasts =
    [ linkToast ]


linkToast : String
linkToast =
    "sign-up-link"


sendLink : Model -> Effect Msg
sendLink model =
    Request
        (ApiClient.request Api.signUp
            { email = model.email, username = model.username }
            GotSignUp
        )


failure : Msg -> ApiClient.Error -> Effect Msg
failure retry error =
    Effect.failure { id = linkToast, title = "Couldn't send the link", retry = Just retry } error



-- VIEW


view : Model -> List (Html Msg)
view model =
    case model.state of
        Sent ->
            LinkForm.sent ("We sent a link to " ++ String.trim model.email ++ ". It expires in 15 minutes.")

        Open attempt ->
            [ Plain.lead "Create your account" "We'll email you a link. No password needed."
            , form [ onSubmit Submitted ]
                [ Field.group []
                    [ LinkForm.emailField model.email EmailChanged
                    , Field.view []
                        [ Field.label [ for "username", class "sr-only" ] [ text "Username" ]
                        , InputGroup.view [ class "h-10" ]
                            [ InputGroup.input
                                [ id "username"
                                , name "username"
                                , Html.Attributes.placeholder "Username"
                                , class "font-medium"
                                , attribute "autocapitalize" "none"
                                , attribute "spellcheck" "false"
                                , required True
                                , value model.username
                                , onInput UsernameChanged
                                ]
                            , InputGroup.start [] [ Phosphor.at Bold |> Phosphor.toHtml [ attribute "aria-hidden" "true" ] ]
                            ]
                        , Field.description [] [ text "3 to 20 letters, digits or underscores." ]
                        ]
                    , LinkForm.submit attempt
                    ]
                ]
            ]
