module Page.About exposing (Model, Msg, load, update, view)

import Api
import ApiClient
import Effect exposing (Effect(..))
import Html exposing (Html, a, dd, div, dl, dt, p, span, text)
import Html.Attributes exposing (attribute, class, href, rel, target)
import Phosphor exposing (IconWeight(..))
import UI.Card as Card
import UI.Skeleton as Skeleton



-- MODEL


type Model
    = Checking
    | Checked Api.Health
    | Unreachable String


load : ( Model, Effect Msg )
load =
    ( Checking, Request (ApiClient.request Api.health () GotHealth) )



-- UPDATE


type Msg
    = GotHealth (Result ApiClient.Error Api.Health)


update : Msg -> Model -> ( Model, Effect Msg )
update msg _ =
    case msg of
        GotHealth (Ok health) ->
            ( Checked health, NoEffect )

        GotHealth (Err error) ->
            ( Unreachable (ApiClient.errorToString error), NoEffect )



-- VIEW


view : Model -> List (Html msg)
view model =
    [ Card.new
        |> Card.title [ Html.h3 [] [ text "Privacy" ] ]
        |> Card.description [ text "Your prompts and pictures stay on this device." ]
        |> Card.content []
            [ p [ class "text-sm text-muted-foreground" ]
                [ text "The models run in your browser, on your graphics card. No prompt or picture is ever sent to the server, and your library lives in this browser alone." ]
            ]
        |> Card.footer []
            [ p [ class "flex items-center gap-2 text-xs text-muted-foreground" ]
                [ Phosphor.lockSimple Regular |> Phosphor.toHtml [ attribute "aria-hidden" "true" ]
                , text "The server is only used for authentication: your account, passkeys and sessions."
                ]
            ]
        |> Card.toHtml [ class "w-full [--card-spacing:--spacing(5)] sm:[--card-spacing:--spacing(6)]" ]
    , Card.new
        |> Card.title [ Html.h3 [] [ text "Server" ] ]
        |> Card.description [ text "The release running now, where it answers from and how fast it reaches the database." ]
        |> Card.content []
            [ dl [ class "flex flex-col gap-3 text-sm", attribute "role" "status" ]
                (case model of
                    Checking ->
                        [ row "Release" (pending "w-16")
                        , row "Region" (pending "w-10")
                        , row "Latency" (pending "w-12")
                        ]

                    Checked health ->
                        [ row "Release" (release health.release)
                        , row "Region" (text (Maybe.withDefault "Unknown" health.release.region))
                        , row "Latency" (latency health.database)
                        ]

                    Unreachable message ->
                        [ row "Server" (span [ class "text-destructive" ] [ text message ]) ]
                )
            ]
        |> Card.footer []
            [ p [ class "flex items-center gap-2 text-xs text-muted-foreground" ]
                [ Phosphor.arrowsClockwise Regular |> Phosphor.toHtml [ attribute "aria-hidden" "true" ]
                , text "Checked each time you open this page."
                ]
            ]
        |> Card.toHtml [ class "w-full [--card-spacing:--spacing(5)] sm:[--card-spacing:--spacing(6)]" ]
    ]


row : String -> Html msg -> Html msg
row label value =
    div [ class "flex min-h-5 items-baseline justify-between gap-4" ]
        [ dt [ class "text-muted-foreground" ] [ text label ]
        , dd [ class "min-w-0 text-end wrap-anywhere" ] [ value ]
        ]


pending : String -> Html msg
pending width =
    Skeleton.view [ class ("inline-block h-4 align-middle " ++ width), attribute "aria-hidden" "true" ]


release : Api.Release -> Html msg
release { releaseId, releaseUrl } =
    case ( releaseId, releaseUrl ) of
        ( Just id, Just url ) ->
            a
                [ href url
                , target "_blank"
                , rel "noopener noreferrer"
                , class "font-mono underline underline-offset-2"
                ]
                [ text (shortRelease id) ]

        ( Just id, Nothing ) ->
            span [ class "font-mono" ] [ text (shortRelease id) ]

        ( Nothing, _ ) ->
            text "Unknown"


shortRelease : String -> String
shortRelease id =
    if String.startsWith "dpl_" id then
        id

    else
        String.left 7 id


latency : Api.Check -> Html msg
latency check =
    let
        measured =
            Maybe.withDefault "" (Maybe.map (\ms -> String.fromInt ms ++ " ms") check.latencyMs)
    in
    case ( check.status, check.output ) of
        ( Api.Pass, _ ) ->
            text measured

        ( _, output ) ->
            span [ class "flex flex-col items-end gap-0.5" ]
                [ text measured
                , span
                    [ class
                        (if check.status == Api.Fail then
                            "text-destructive"

                         else
                            "text-muted-foreground"
                        )
                    ]
                    [ text (Maybe.withDefault "" output) ]
                ]
