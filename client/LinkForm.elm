module LinkForm exposing (Attempt(..), State(..), emailField, problem, sent, submit, submitLabel)

import Html exposing (Html, text)
import Html.Attributes exposing (attribute, autocomplete, class, disabled, for, id, name, required, type_, value)
import Html.Events exposing (onInput)
import Phosphor exposing (IconWeight(..))
import Plain
import UI.Button as Button
import UI.Field as Field
import UI.InputGroup as InputGroup


type State
    = Open Attempt
    | Sent


type Attempt
    = Editing (Maybe String)
    | Submitting


sent : String -> List (Html msg)
sent body =
    [ Plain.lead "Check your inbox" body ]


emailField : String -> (String -> msg) -> Html msg
emailField email toMsg =
    Field.view []
        [ Field.label [ for "email", class "sr-only" ] [ text "Email" ]
        , InputGroup.view [ class "h-10" ]
            [ InputGroup.input
                [ id "email"
                , name "email"
                , type_ "email"
                , Html.Attributes.placeholder "Email"
                , class "font-medium"
                , autocomplete True
                , required True
                , value email
                , onInput toMsg
                ]
            , InputGroup.start [] [ Phosphor.envelopeSimple Bold |> Phosphor.toHtml [ attribute "aria-hidden" "true" ] ]
            ]
        ]


problem : Attempt -> Html msg
problem attempt =
    case attempt of
        Editing (Just reason) ->
            Field.error [] [ text reason ]

        _ ->
            text ""


submit : Attempt -> Html msg
submit attempt =
    Button.new
        |> Button.text (submitLabel attempt)
        |> Button.toHtml [ type_ "submit", disabled (attempt == Submitting), class "h-10" ]


submitLabel : Attempt -> String
submitLabel attempt =
    case attempt of
        Submitting ->
            "Sending the link…"

        Editing _ ->
            "Email me a link"
