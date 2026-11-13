module Page.Passkey exposing (Model, Msg, enrolment, init, update, view)

import Ceremony exposing (Ceremony(..))
import Effect exposing (Effect(..))
import Enrolment
import Html exposing (Html, div)
import Html.Attributes exposing (attribute, class, disabled, href)
import Html.Events exposing (onClick)
import Path
import Phosphor exposing (IconWeight(..))
import Plain
import UI.Button as Button



-- MODEL


type alias Model =
    Ceremony


init : Model
init =
    Resting Nothing



-- UPDATE


type Msg
    = GotEnrolment Enrolment.Msg


enrolment : Enrolment.Msg -> Msg
enrolment =
    GotEnrolment


update : Msg -> Model -> ( Model, Effect Msg )
update (GotEnrolment msg) model =
    let
        ( ceremony, effect, outcome ) =
            Enrolment.update msg model
    in
    case outcome of
        Enrolment.Enrolled _ ->
            ( ceremony, PushUrl (Path.toString Path.Explore) )

        Enrolment.Pending ->
            ( ceremony, Effect.map GotEnrolment effect )



-- VIEW


view : Model -> List (Html Msg)
view ceremony =
    [ Plain.lead "Create your passkey"
        "A passkey signs you in with your fingerprint, face or screen lock, with no email to wait for."
    , div [ class "flex flex-col gap-4" ]
        [ Ceremony.problem ceremony
        , Button.new
            |> Button.text
                (if ceremony == Waiting then
                    "Waiting for your device…"

                 else
                    "Create a passkey"
                )
            |> Button.startIcon (Phosphor.fingerprint Bold |> Phosphor.toHtml [ attribute "aria-hidden" "true" ])
            |> Button.toHtml [ onClick (GotEnrolment Enrolment.requested), disabled (ceremony == Waiting), class "h-10" ]
        , Button.new
            |> Button.text "Not now"
            |> Button.outline
            |> Button.toAnchor [ href (Path.toString Path.Explore), class "h-10" ]
        ]
    ]
