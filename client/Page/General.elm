module Page.General exposing (Model, Msg, load, update, view)

import Api
import ApiClient
import Effect exposing (Effect(..))
import Html exposing (Html, a, dd, div, dl, dt, span, text)
import Html.Attributes exposing (attribute, class, href, rel, target)
import UI.Card as Card
import UI.Item as Item
import UI.Spinner as Spinner



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
        |> Card.description [ text "Your prompts and pictures stay on this device unless you decide to share them on the public feed." ]
        |> Card.toHtml [ class "w-full [--card-spacing:--spacing(5)] sm:[--card-spacing:--spacing(6)]" ]
    , Card.new
        |> Card.title [ Html.h3 [] [ text "Release" ] ]
        |> Card.description [ text "The app release running" ]
        |> Card.content [ class "flex flex-col gap-2", attribute "role" "status" ]
            (indicator model
                :: (case model of
                        Checked health ->
                            if health.status == Api.Fail then
                                []

                            else
                                [ details
                                    [ row "Release" (release health.release)
                                    , row "Region" (text (Maybe.withDefault "Unknown" health.release.region))
                                    , row "Latency" (text (latency health.database))
                                    ]
                                ]

                        Checking ->
                            []

                        Unreachable _ ->
                            []
                   )
            )
        |> Card.toHtml [ class "w-full [--card-spacing:--spacing(5)] sm:[--card-spacing:--spacing(6)]" ]
    ]


indicator : Model -> Html msg
indicator model =
    case model of
        Checking ->
            status
                (Spinner.view [ class "size-3! text-muted-foreground motion-reduce:animate-none", attribute "aria-hidden" "true" ])
                "Checking health status"

        Checked health ->
            case health.status of
                Api.Pass ->
                    status (dot "bg-green-500") (Maybe.withDefault "Healthy" health.database.output)

                Api.Warn ->
                    status (dot "bg-yellow-500") (Maybe.withDefault "Degraded" health.database.output)

                Api.Fail ->
                    status (dot "bg-red-500") (Maybe.withDefault "Down" health.database.output)

        Unreachable message ->
            status (dot "bg-red-500") message


status : Html msg -> String -> Html msg
status icon label =
    Item.new
        |> Item.muted
        |> Item.icon icon
        |> Item.title [ text label ]
        |> Item.toHtml [ class "[&_[data-slot=item-title]]:line-clamp-none" ]


details : List (Html msg) -> Html msg
details rows =
    Item.new
        |> Item.muted
        |> Item.title [ dl [ class "flex w-full flex-col gap-3 text-sm" ] rows ]
        |> Item.toHtml [ class "[&_[data-slot=item-title]]:line-clamp-none [&_[data-slot=item-title]]:w-full [&_[data-slot=item-title]]:font-normal" ]


dot : String -> Html msg
dot color =
    span [ class ("size-3 rounded-full " ++ color), attribute "aria-hidden" "true" ] []


row : String -> Html msg -> Html msg
row label value =
    div [ class "flex min-h-5 items-baseline justify-between gap-4" ]
        [ dt [ class "text-muted-foreground" ] [ text label ]
        , dd [ class "min-w-0 text-end wrap-anywhere" ] [ value ]
        ]


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


latency : Api.Check -> String
latency check =
    Maybe.withDefault "Unknown" (Maybe.map (\ms -> String.fromInt ms ++ " ms") check.latencyMs)
