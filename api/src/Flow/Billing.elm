module Flow.Billing exposing (Msg, checkout, portal, update, webhook, webhookChecked)

import Api
import Data.PaddleSignature as PaddleSignature exposing (PaddleSignature)
import Data.UserId exposing (UserId)
import Db
import Effect exposing (Effect(..), log, query, respond, transact)
import InteropDefinitions as Interop
import Json.Decode as Decode
import Json.Encode as Encode
import RateLimit
import Reply exposing (Reply(..), withSession)
import Request exposing (Request)
import Response
import Server exposing (Server)
import Services.Paddle as Paddle
import Time


type Msg
    = WebhookArrived Interop.Resolver { signature : PaddleSignature, body : String } Time.Posix
    | SubscriptionApplied Interop.Resolver { eventId : String, eventType : String } String
    | CheckoutStateFound Interop.Resolver (Maybe { userId : UserId, pro : Bool, customerId : Maybe String })
    | TransactionCreated Interop.Resolver (Result String String)
    | PortalRowsFound Interop.Resolver (Maybe (List Db.Subscription))
    | PortalCreated Interop.Resolver (Result String String)


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


checkout : Server -> Request -> Reply Msg
checkout server request =
    case server.paddle of
        Nothing ->
            Now Response.unavailable

        Just _ ->
            Limit [ RateLimit.checkouts (Request.clientAddress request) ]
                (withSession request
                    (\token resolver ->
                        transact server.db resolver "Checkout lookup failed" (Db.checkoutState token) (CheckoutStateFound resolver)
                    )
                )


portal : Server -> Request -> Reply Msg
portal server request =
    case server.paddle of
        Nothing ->
            Now Response.unavailable

        Just _ ->
            Limit [ RateLimit.portals (Request.clientAddress request) ]
                (withSession request
                    (\token resolver ->
                        transact server.db resolver "Portal lookup failed" (Db.subscriptions token) (PortalRowsFound resolver)
                    )
                )


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
                            , log Interop.Error ("Paddle webhook did not decode at " ++ failingPath problem ++ ".")
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

        CheckoutStateFound resolver found ->
            case ( server.paddle, found ) of
                ( Nothing, _ ) ->
                    respond resolver Response.unavailable

                ( _, Nothing ) ->
                    respond resolver Response.notSignedIn

                ( Just paddle, Just state ) ->
                    if state.pro then
                        respond resolver (Response.error 409 "You already have Pro.")

                    else
                        Effect.map (TransactionCreated resolver)
                            (Paddle.createTransaction paddle { userId = state.userId, customerId = state.customerId })

        TransactionCreated resolver outcome ->
            case ( server.paddle, outcome ) of
                ( Nothing, _ ) ->
                    respond resolver Response.unavailable

                ( Just paddle, Ok id ) ->
                    respond resolver
                        (Response.json 200
                            (Api.encodeResponse Api.checkout
                                { transactionId = id
                                , clientToken = paddle.clientToken
                                , environment = Paddle.environmentName paddle.environment
                                }
                            )
                        )

                ( _, Err reason ) ->
                    Batch
                        [ respond resolver (Response.error 503 "Billing is unavailable right now.")
                        , log Interop.Error ("Paddle checkout failed: " ++ reason)
                        ]

        PortalRowsFound resolver found ->
            case ( server.paddle, found ) of
                ( Nothing, _ ) ->
                    respond resolver Response.unavailable

                ( _, Nothing ) ->
                    respond resolver Response.notSignedIn

                ( Just paddle, Just rows ) ->
                    case rows of
                        [] ->
                            respond resolver (Response.error 404 "No billing account yet.")

                        first :: _ ->
                            Effect.map (PortalCreated resolver)
                                (Paddle.createPortalSession paddle
                                    { customerId = first.customerId
                                    , subscriptionIds =
                                        rows
                                            |> List.filter (\row -> List.member row.status [ "active", "trialing", "past_due" ])
                                            |> List.map .subscriptionId
                                    }
                                )

        PortalCreated resolver outcome ->
            case outcome of
                Ok url ->
                    respond resolver (Response.json 200 (Api.encodeResponse Api.portal { url = url }))

                Err reason ->
                    Batch
                        [ respond resolver (Response.error 503 "Billing is unavailable right now.")
                        , log Interop.Error ("Paddle portal failed: " ++ reason)
                        ]

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


failingPath : Decode.Error -> String
failingPath problem =
    case problemPath problem of
        [] ->
            "the top"

        steps ->
            String.join "." steps


problemPath : Decode.Error -> List String
problemPath problem =
    case problem of
        Decode.Field name inner ->
            name :: problemPath inner

        Decode.Index i inner ->
            String.fromInt i :: problemPath inner

        Decode.OneOf (first :: _) ->
            problemPath first

        Decode.OneOf [] ->
            []

        Decode.Failure _ _ ->
            []


answered : Interop.Resolver -> Interop.Level -> String -> Effect msg
answered resolver level message =
    Batch
        [ respond resolver (Response.json 200 (Encode.object []))
        , log level message
        ]
