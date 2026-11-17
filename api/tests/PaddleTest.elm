module PaddleTest exposing (suite)

import Api
import Data.PaddleSignature as PaddleSignature
import Data.UserId as UserId
import Expect
import Json.Decode as Decode
import Services.Paddle as Paddle
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "Paddle"
        [ describe "PaddleSignature.parse"
            [ test "reads a timestamp and one signature" <|
                \() ->
                    PaddleSignature.parse ("ts=1671552777;h1=" ++ hash)
                        |> Maybe.map (\signature -> ( PaddleSignature.timestamp signature, PaddleSignature.signatures signature ))
                        |> Expect.equal (Just ( 1671552777, [ hash ] ))
            , test "keeps both signatures of a secret rotation" <|
                \() ->
                    PaddleSignature.parse ("ts=1671552777;h1=" ++ hash ++ ";h1=" ++ otherHash)
                        |> Maybe.map PaddleSignature.signatures
                        |> Expect.equal (Just [ hash, otherHash ])
            , test "accepts upper-case hex, lower-cased" <|
                \() ->
                    PaddleSignature.parse ("ts=1;h1=" ++ String.toUpper hash)
                        |> Maybe.map PaddleSignature.signatures
                        |> Expect.equal (Just [ hash ])
            , test "refuses a header without ts" <|
                \() ->
                    PaddleSignature.parse ("h1=" ++ hash)
                        |> Expect.equal Nothing
            , test "refuses a ts that is not digits" <|
                \() ->
                    PaddleSignature.parse ("ts=12ab;h1=" ++ hash)
                        |> Expect.equal Nothing
            , test "drops a 63-character signature" <|
                \() ->
                    PaddleSignature.parse ("ts=1;h1=" ++ String.dropLeft 1 hash ++ ";h1=" ++ otherHash)
                        |> Maybe.map PaddleSignature.signatures
                        |> Expect.equal (Just [ otherHash ])
            , test "refuses a header with only bad signatures" <|
                \() ->
                    PaddleSignature.parse ("ts=1;h1=" ++ String.dropLeft 1 hash ++ ";h1=" ++ String.repeat 64 "g")
                        |> Expect.equal Nothing
            , test "ignores unknown keys" <|
                \() ->
                    PaddleSignature.parse ("ts=1;x=2;h1=" ++ hash)
                        |> Maybe.map PaddleSignature.signatures
                        |> Expect.equal (Just [ hash ])
            ]
        , describe "config"
            [ test "is off when nothing is set" <|
                \() ->
                    Paddle.config (flags Nothing Nothing Nothing Nothing Nothing)
                        |> Expect.equal (Ok Nothing)
            , test "reads all five" <|
                \() ->
                    Paddle.config (flags (Just "sandbox") (Just "key") (Just "secret") (Just "token") (Just "pri_pro"))
                        |> Result.map (Maybe.map (\config -> ( config.environment == Paddle.Sandbox, config.proPriceId )))
                        |> Expect.equal (Ok (Just ( True, "pri_pro" )))
            , test "refuses one missing" <|
                \() ->
                    Paddle.config (flags (Just "sandbox") (Just "key") Nothing (Just "token") (Just "pri_pro"))
                        |> Expect.equal (Err "PADDLE_ENVIRONMENT, PADDLE_API_KEY, PADDLE_WEBHOOK_SECRET, PADDLE_CLIENT_TOKEN and PADDLE_PRO_PRICE_ID must all be set, or none.")
            , test "refuses an unknown environment" <|
                \() ->
                    Paddle.config (flags (Just "live") (Just "key") (Just "secret") (Just "token") (Just "pri_pro"))
                        |> Expect.equal (Err "PADDLE_ENVIRONMENT must be sandbox or production.")
            ]
        , describe "event"
            [ test "reads a subscription" <|
                \() ->
                    decode example
                        |> Result.map summary
                        |> Expect.equal
                            (Ok
                                (Just
                                    ( ( "evt_01hv8x2a0e9xjn3k0q0p1y3z4a", "subscription.updated", "sub_01hv8x2a0e9xjn3k0q0p1y3z4b" )
                                    , ( "ctm_01hv8x2a0e9xjn3k0q0p1y3z4c", Just "6f1c1f0e-7f3a-4c2b-9a51-0d4b1d7c8e21", [ "pri_01hv8x2a0e9xjn3k0q0p1y3z4d" ] )
                                    , ( Just "2026-12-17T10:00:00Z", Nothing, "2026-11-17T10:00:00.500000Z" )
                                    )
                                )
                            )
            , test "reads a null custom_data as no user" <|
                \() ->
                    decode (String.replace """"custom_data": { "user_id": "6f1c1f0e-7f3a-4c2b-9a51-0d4b1d7c8e21" }""" """"custom_data": null""" example)
                        |> Result.map (summary >> Maybe.map (\( _, ( _, user, _ ), _ ) -> user))
                        |> Expect.equal (Ok (Just Nothing))
            , test "reads a user id that is not a UUID as no user" <|
                \() ->
                    decode (String.replace "6f1c1f0e-7f3a-4c2b-9a51-0d4b1d7c8e21" "not-a-uuid" example)
                        |> Result.map (summary >> Maybe.map (\( _, ( _, user, _ ), _ ) -> user))
                        |> Expect.equal (Ok (Just Nothing))
            , test "reads a null billing period as no end" <|
                \() ->
                    decode (String.replace """"current_billing_period": {
      "starts_at": "2026-11-17T10:00:00Z",
      "ends_at": "2026-12-17T10:00:00Z"
    }""" """"current_billing_period": null""" example)
                        |> Result.map (summary >> Maybe.map (\( _, _, ( ends, _, _ ) ) -> ends))
                        |> Expect.equal (Ok (Just Nothing))
            , test "reads a scheduled cancellation" <|
                \() ->
                    decode (String.replace """"scheduled_change": null""" """"scheduled_change": { "action": "cancel", "effective_at": "2026-12-17T10:00:00Z", "resume_at": null }""" example)
                        |> Result.map (summary >> Maybe.map (\( _, _, ( _, change, _ ) ) -> change))
                        |> Expect.equal (Ok (Just (Just "cancel")))
            , test "refuses an unknown scheduled change" <|
                \() ->
                    decode (String.replace """"scheduled_change": null""" """"scheduled_change": { "action": "explode", "effective_at": "2026-12-17T10:00:00Z" }""" example)
                        |> Result.map (always ())
                        |> Expect.err
            , test "refuses a subscription id that is not sub_" <|
                \() ->
                    decode (String.replace "sub_01hv8x2a0e9xjn3k0q0p1y3z4b" "ctm_01hv8x2a0e9xjn3k0q0p1y3z4b" example)
                        |> Result.map (always ())
                        |> Expect.err
            , test "keeps an unknown status apart" <|
                \() ->
                    decode (String.replace "\"status\": \"active\"" "\"status\": \"suspended\"" example)
                        |> Result.map
                            (\found ->
                                case found of
                                    Paddle.UnknownStatus { status } ->
                                        Just status

                                    _ ->
                                        Nothing
                            )
                        |> Expect.equal (Ok (Just "suspended"))
            , test "ignores other kinds of event" <|
                \() ->
                    decode """{"event_id":"evt_1","event_type":"transaction.completed","occurred_at":"2026-11-17T10:00:01Z","data":{"id":"txn_1"}}"""
                        |> Result.map
                            (\found ->
                                case found of
                                    Paddle.Ignored { eventType } ->
                                        Just eventType

                                    _ ->
                                        Nothing
                            )
                        |> Expect.equal (Ok (Just "transaction.completed"))
            ]
        , describe "grantsPro"
            [ test "is true while the subscription is active, trialing or past due" <|
                \() ->
                    List.map Paddle.grantsPro [ Paddle.Active, Paddle.Trialing, Paddle.PastDue, Paddle.Canceled, Paddle.Paused ]
                        |> Expect.equal [ True, True, True, False, False ]
            ]
        , describe "billingOf"
            [ test "is Free without a way to manage when there are no rows" <|
                \() ->
                    Paddle.billingOf []
                        |> Expect.equal { plan = Api.Free, renewsAt = Nothing, endsAt = Nothing, pastDue = False, canManage = False }
            , test "is Pro and renews at the period's end" <|
                \() ->
                    Paddle.billingOf [ row "active" Nothing Nothing (Just "2026-12-17T10:00:00Z") ]
                        |> Expect.equal { plan = Api.Pro, renewsAt = Just "2026-12-17T10:00:00Z", endsAt = Nothing, pastDue = False, canManage = True }
            , test "is Pro and ends at the scheduled cancellation" <|
                \() ->
                    Paddle.billingOf [ row "active" (Just "cancel") (Just "2027-01-01T00:00:00Z") (Just "2026-12-17T10:00:00Z") ]
                        |> Expect.equal { plan = Api.Pro, renewsAt = Nothing, endsAt = Just "2027-01-01T00:00:00Z", pastDue = False, canManage = True }
            , test "is Pro and past due when the last payment failed" <|
                \() ->
                    Paddle.billingOf [ row "past_due" Nothing Nothing (Just "2026-12-17T10:00:00Z") ]
                        |> Expect.equal { plan = Api.Pro, renewsAt = Just "2026-12-17T10:00:00Z", endsAt = Nothing, pastDue = True, canManage = True }
            , test "is Free but can manage after the subscription is canceled" <|
                \() ->
                    Paddle.billingOf [ row "canceled" Nothing Nothing Nothing ]
                        |> Expect.equal { plan = Api.Free, renewsAt = Nothing, endsAt = Nothing, pastDue = False, canManage = True }
            ]
        , describe "UserId.fromString"
            [ test "accepts a UUID" <|
                \() ->
                    UserId.fromString "6f1c1f0e-7f3a-4c2b-9a51-0d4b1d7c8e21"
                        |> Maybe.map UserId.toString
                        |> Expect.equal (Just "6f1c1f0e-7f3a-4c2b-9a51-0d4b1d7c8e21")
            , test "refuses text that is not a UUID" <|
                \() ->
                    List.map UserId.fromString [ "", "user-1", "6f1c1f0e7f3a4c2b9a510d4b1d7c8e21", "6f1c1f0e-7f3a-4c2b-9a51-0d4b1d7c8e2g" ]
                        |> List.map ((==) Nothing)
                        |> Expect.equal [ True, True, True, True ]
            ]
        ]


hash : String
hash =
    String.repeat 32 "0a"


otherHash : String
otherHash =
    String.repeat 16 "1f2e"


flags :
    Maybe String
    -> Maybe String
    -> Maybe String
    -> Maybe String
    -> Maybe String
    ->
        { paddleEnvironment : Maybe String
        , paddleApiKey : Maybe String
        , paddleWebhookSecret : Maybe String
        , paddleClientToken : Maybe String
        , paddleProPriceId : Maybe String
        }
flags environment apiKey secret token price =
    { paddleEnvironment = environment
    , paddleApiKey = apiKey
    , paddleWebhookSecret = secret
    , paddleClientToken = token
    , paddleProPriceId = price
    }


decode : String -> Result Decode.Error Paddle.Event
decode =
    Decode.decodeString Paddle.event


summary :
    Paddle.Event
    ->
        Maybe
            ( ( String, String, String )
            , ( String, Maybe String, List String )
            , ( Maybe String, Maybe String, String )
            )
summary found =
    case found of
        Paddle.SubscriptionChanged { eventId, eventType, subscription } ->
            Just
                ( ( eventId, eventType, subscription.id )
                , ( subscription.customerId, Maybe.map UserId.toString subscription.userId, subscription.priceIds )
                , ( subscription.currentPeriodEndsAt, Maybe.map .action subscription.scheduledChange, subscription.updatedAt )
                )

        _ ->
            Nothing


example : String
example =
    """{
  "event_id": "evt_01hv8x2a0e9xjn3k0q0p1y3z4a",
  "event_type": "subscription.updated",
  "occurred_at": "2026-11-17T10:00:01.000000Z",
  "data": {
    "id": "sub_01hv8x2a0e9xjn3k0q0p1y3z4b",
    "status": "active",
    "customer_id": "ctm_01hv8x2a0e9xjn3k0q0p1y3z4c",
    "custom_data": { "user_id": "6f1c1f0e-7f3a-4c2b-9a51-0d4b1d7c8e21" },
    "items": [{ "price": { "id": "pri_01hv8x2a0e9xjn3k0q0p1y3z4d" } }],
    "current_billing_period": {
      "starts_at": "2026-11-17T10:00:00Z",
      "ends_at": "2026-12-17T10:00:00Z"
    },
    "scheduled_change": null,
    "updated_at": "2026-11-17T10:00:00.500000Z"
  }
}"""


row : String -> Maybe String -> Maybe String -> Maybe String -> { status : String, scheduledChange : Maybe String, scheduledChangeAt : Maybe String, periodEndsAt : Maybe String }
row status change changeAt periodEnds =
    { status = status, scheduledChange = change, scheduledChangeAt = changeAt, periodEndsAt = periodEnds }
