module Flow.Billing exposing (Msg, update, webhook, webhookChecked)

import Data.PaddleSignature as PaddleSignature exposing (PaddleSignature)
import Db
import Effect exposing (Effect(..), log, query, respond)
import InteropDefinitions as Interop
import Json.Decode as Decode
import Json.Encode as Encode
import Reply exposing (Reply(..))
import Request exposing (Request)
import Response
import Server exposing (Server)
import Services.Paddle as Paddle
import Time


type Msg
    = WebhookArrived Interop.Resolver { signature : PaddleSignature, body : String } Time.Posix
    | SubscriptionApplied Interop.Resolver { eventId : String, eventType : String } String


webhook : Server -> Request -> Reply Msg
webhook server request =
    case server.paddle of
        Nothing ->
            Now Response.unavailable

        Just _ ->
            case Request.header "paddle-signature" request |> Maybe.andThen PaddleSignature.parse of
                Nothing ->
                    Now (Response.error 400 "Expected a Paddle-Signature header.")

                Just signature ->
                    Later (\resolver -> Clock (WebhookArrived resolver { signature = signature, body = request.body }))


webhookChecked : Server -> Interop.Resolver -> String -> Bool -> Effect Msg
webhookChecked server resolver body valid =
    case server.paddle of
        Nothing ->
            respond resolver Response.unavailable

        Just paddle ->
            if not valid then
                Batch
                    [ respond resolver (Response.error 401 "Signature refused.")
                    , log Interop.Warn "Paddle webhook refused: the signature does not match."
                    ]

            else
                case Decode.decodeString Paddle.event body of
                    Err problem ->
                        Batch
                            [ respond resolver (Response.error 400 "Expected a Paddle event.")
                            , log Interop.Error ("Paddle webhook did not decode: " ++ String.left 200 (Decode.errorToString problem))
                            ]

                    Ok (Paddle.Ignored { eventType, eventId }) ->
                        answered resolver Interop.Info ("Paddle " ++ eventType ++ " " ++ eventId ++ " ignored.")

                    Ok (Paddle.UnknownStatus { eventType, eventId, status }) ->
                        answered resolver
                            Interop.Warn
                            ("Paddle " ++ eventType ++ " " ++ eventId ++ " has an unknown status, " ++ String.left 40 status ++ "; ignored.")

                    Ok (Paddle.SubscriptionChanged { eventId, eventType, subscription }) ->
                        if not (List.member paddle.proPriceId subscription.priceIds) then
                            answered resolver Interop.Warn ("Paddle " ++ eventType ++ " " ++ eventId ++ " is not for the Pro price; ignored.")

                        else
                            query server.db
                                resolver
                                "Paddle webhook failed"
                                (Db.applySubscription
                                    { id = subscription.id
                                    , userId = subscription.userId
                                    , customerId = subscription.customerId
                                    , status = Paddle.statusToString subscription.status
                                    , priceId = paddle.proPriceId
                                    , periodEnds = subscription.currentPeriodEndsAt
                                    , scheduledChange = Maybe.map .action subscription.scheduledChange
                                    , scheduledChangeAt = Maybe.map .effectiveAt subscription.scheduledChange
                                    , updatedAt = subscription.updatedAt
                                    }
                                )
                                (SubscriptionApplied resolver { eventId = eventId, eventType = eventType })


update : Server -> Msg -> Effect Msg
update server msg =
    case msg of
        WebhookArrived resolver arrived now ->
            case server.paddle of
                Nothing ->
                    respond resolver Response.unavailable

                Just paddle ->
                    let
                        seconds =
                            PaddleSignature.timestamp arrived.signature
                    in
                    if abs (Time.posixToMillis now - seconds * 1000) > 5000 then
                        Batch
                            [ respond resolver (Response.error 401 "Signature refused.")
                            , log Interop.Warn "Paddle webhook refused: its timestamp is more than 5 s away."
                            ]

                    else
                        SendToJs
                            (Interop.VerifyWebhook resolver
                                { secret = paddle.webhookSecret
                                , signed = String.fromInt seconds ++ ":" ++ arrived.body
                                , signatures = PaddleSignature.signatures arrived.signature
                                , body = arrived.body
                                }
                            )

        SubscriptionApplied resolver ids outcome ->
            let
                name =
                    "Paddle " ++ ids.eventType ++ " " ++ ids.eventId
            in
            case outcome of
                "applied" ->
                    answered resolver Interop.Info (name ++ " applied.")

                "stale" ->
                    answered resolver Interop.Info (name ++ " is older than the stored state; skipped.")

                "unmatched" ->
                    answered resolver Interop.Warn (name ++ " names no account; skipped.")

                _ ->
                    Batch
                        [ respond resolver Response.unavailable
                        , log Interop.Error (name ++ " got an unknown outcome from the database.")
                        ]


answered : Interop.Resolver -> Interop.Level -> String -> Effect msg
answered resolver level message =
    Batch
        [ respond resolver (Response.json 200 (Encode.object []))
        , log level message
        ]
