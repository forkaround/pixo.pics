module Services.Paddle exposing (Config, Environment(..), Event(..), Status(..), Subscription, apiBase, billingOf, config, createPortalSession, createTransaction, environmentName, event, grantsPro, statusToString)

import Api
import Data.UserId as UserId exposing (UserId)
import Effect exposing (Effect)
import Http
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode


type Environment
    = Sandbox
    | Production


type alias Config =
    { environment : Environment
    , apiKey : String
    , webhookSecret : String
    , clientToken : String
    , proPriceId : String
    }


type Status
    = Active
    | Canceled
    | PastDue
    | Paused
    | Trialing


type alias Subscription =
    { id : String
    , status : Status
    , customerId : String
    , userId : Maybe UserId
    , priceIds : List String
    , currentPeriodEndsAt : Maybe String
    , scheduledChange : Maybe { action : String, effectiveAt : String }
    , updatedAt : String
    }


type Event
    = SubscriptionChanged { eventId : String, eventType : String, subscription : Subscription }
    | UnknownStatus { eventId : String, eventType : String, status : String }
    | Ignored { eventId : String, eventType : String }


config :
    { a
        | paddleEnvironment : Maybe String
        , paddleApiKey : Maybe String
        , paddleWebhookSecret : Maybe String
        , paddleClientToken : Maybe String
        , paddleProPriceId : Maybe String
    }
    -> Result String (Maybe Config)
config flags =
    if List.all ((==) Nothing) [ flags.paddleEnvironment, flags.paddleApiKey, flags.paddleWebhookSecret, flags.paddleClientToken, flags.paddleProPriceId ] then
        Ok Nothing

    else
        case
            Maybe.map5 (\environment apiKey webhookSecret clientToken proPriceId -> { environment = environment, apiKey = apiKey, webhookSecret = webhookSecret, clientToken = clientToken, proPriceId = proPriceId })
                flags.paddleEnvironment
                flags.paddleApiKey
                flags.paddleWebhookSecret
                flags.paddleClientToken
                flags.paddleProPriceId
        of
            Just set ->
                case environmentFromString set.environment of
                    Just chosen ->
                        if String.startsWith "pri_" set.proPriceId then
                            Ok
                                (Just
                                    { environment = chosen
                                    , apiKey = set.apiKey
                                    , webhookSecret = set.webhookSecret
                                    , clientToken = set.clientToken
                                    , proPriceId = set.proPriceId
                                    }
                                )

                        else
                            Err "PADDLE_PRO_PRICE_ID must start with pri_."

                    Nothing ->
                        Err "PADDLE_ENVIRONMENT must be sandbox or production."

            Nothing ->
                Err "PADDLE_ENVIRONMENT, PADDLE_API_KEY, PADDLE_WEBHOOK_SECRET, PADDLE_CLIENT_TOKEN and PADDLE_PRO_PRICE_ID must all be set, or none."


environmentFromString : String -> Maybe Environment
environmentFromString value =
    case value of
        "sandbox" ->
            Just Sandbox

        "production" ->
            Just Production

        _ ->
            Nothing


statusFromString : String -> Maybe Status
statusFromString value =
    case value of
        "active" ->
            Just Active

        "canceled" ->
            Just Canceled

        "past_due" ->
            Just PastDue

        "paused" ->
            Just Paused

        "trialing" ->
            Just Trialing

        _ ->
            Nothing


statusToString : Status -> String
statusToString status =
    case status of
        Active ->
            "active"

        Canceled ->
            "canceled"

        PastDue ->
            "past_due"

        Paused ->
            "paused"

        Trialing ->
            "trialing"


grantsPro : Status -> Bool
grantsPro status =
    case status of
        Active ->
            True

        Trialing ->
            True

        PastDue ->
            True

        Canceled ->
            False

        Paused ->
            False


event : Decoder Event
event =
    Decode.map2 Tuple.pair
        (Decode.field "event_id" Decode.string)
        (Decode.field "event_type" Decode.string)
        |> Decode.andThen
            (\( eventId, eventType ) ->
                if String.startsWith "subscription." eventType then
                    Decode.field "data" (Decode.field "status" Decode.string)
                        |> Decode.andThen
                            (\status ->
                                case statusFromString status of
                                    Just known ->
                                        Decode.field "data" (subscription known)
                                            |> Decode.map
                                                (\decoded ->
                                                    SubscriptionChanged { eventId = eventId, eventType = eventType, subscription = decoded }
                                                )

                                    Nothing ->
                                        Decode.succeed (UnknownStatus { eventId = eventId, eventType = eventType, status = status })
                            )

                else
                    Decode.succeed (Ignored { eventId = eventId, eventType = eventType })
            )


subscription : Status -> Decoder Subscription
subscription status =
    Decode.map7 (\id customerId user priceIds periodEnds scheduled updatedAt -> Subscription id status customerId user priceIds periodEnds scheduled updatedAt)
        (prefixed "sub_" "id")
        (prefixed "ctm_" "customer_id")
        userId
        (Decode.field "items" (Decode.list (Decode.at [ "price", "id" ] Decode.string)))
        periodEnd
        scheduledChange
        (Decode.field "updated_at" Decode.string)


prefixed : String -> String -> Decoder String
prefixed prefix name =
    Decode.field name Decode.string
        |> Decode.andThen
            (\value ->
                if String.startsWith prefix value then
                    Decode.succeed value

                else
                    Decode.fail ("Expected " ++ name ++ " to start with " ++ prefix ++ ".")
            )


userId : Decoder (Maybe UserId)
userId =
    Decode.oneOf
        [ Decode.at [ "custom_data", "user_id" ] Decode.string |> Decode.map UserId.fromString
        , Decode.succeed Nothing
        ]


periodEnd : Decoder (Maybe String)
periodEnd =
    Decode.oneOf
        [ Decode.at [ "current_billing_period", "ends_at" ] Decode.string |> Decode.map Just
        , Decode.succeed Nothing
        ]


scheduledChange : Decoder (Maybe { action : String, effectiveAt : String })
scheduledChange =
    Decode.field "scheduled_change" (Decode.nullable change)


change : Decoder { action : String, effectiveAt : String }
change =
    Decode.map2 (\action effectiveAt -> { action = action, effectiveAt = effectiveAt })
        (Decode.field "action" Decode.string
            |> Decode.andThen
                (\action ->
                    if List.member action [ "cancel", "pause", "resume" ] then
                        Decode.succeed action

                    else
                        Decode.fail ("Unknown scheduled change: " ++ action)
                )
        )
        (Decode.field "effective_at" Decode.string)


apiBase : Environment -> String
apiBase environment =
    case environment of
        Sandbox ->
            "https://sandbox-api.paddle.com"

        Production ->
            "https://api.paddle.com"


environmentName : Environment -> String
environmentName environment =
    case environment of
        Sandbox ->
            "sandbox"

        Production ->
            "production"


billingOf :
    List
        { a
            | status : String
            , scheduledChange : Maybe String
            , scheduledChangeAt : Maybe String
            , periodEndsAt : Maybe String
        }
    -> Api.Billing
billingOf rows =
    case rows of
        [] ->
            { plan = Api.Free, renewsAt = Nothing, endsAt = Nothing, pastDue = False, canManage = False }

        first :: _ ->
            if statusGrantsPro first.status then
                if first.scheduledChange == Just "cancel" || first.scheduledChange == Just "pause" then
                    { plan = Api.Pro, renewsAt = Nothing, endsAt = first.scheduledChangeAt, pastDue = first.status == "past_due", canManage = True }

                else
                    { plan = Api.Pro, renewsAt = first.periodEndsAt, endsAt = Nothing, pastDue = first.status == "past_due", canManage = True }

            else
                { plan = Api.Free, renewsAt = Nothing, endsAt = Nothing, pastDue = False, canManage = True }


statusGrantsPro : String -> Bool
statusGrantsPro status =
    List.member status [ "active", "trialing", "past_due" ]


timeout : Float
timeout =
    10000


createTransaction : Config -> { userId : UserId, customerId : Maybe String } -> Effect (Result String String)
createTransaction paddle wanted =
    call paddle
        "/transactions"
        (Encode.object
            ([ ( "items"
               , Encode.list identity
                    [ Encode.object
                        [ ( "price_id", Encode.string paddle.proPriceId )
                        , ( "quantity", Encode.int 1 )
                        ]
                    ]
               )
             , ( "custom_data", Encode.object [ ( "user_id", Encode.string (UserId.toString wanted.userId) ) ] )
             ]
                ++ (wanted.customerId
                        |> Maybe.map (\id -> [ ( "customer_id", Encode.string id ) ])
                        |> Maybe.withDefault []
                   )
            )
        )
        (Decode.at [ "data", "id" ] Decode.string |> Decode.andThen (startingWith "txn_"))


createPortalSession : Config -> { customerId : String, subscriptionIds : List String } -> Effect (Result String String)
createPortalSession paddle { customerId, subscriptionIds } =
    call paddle
        ("/customers/" ++ customerId ++ "/portal-sessions")
        (Encode.object [ ( "subscription_ids", Encode.list Encode.string subscriptionIds ) ])
        (Decode.at [ "data", "urls", "general", "overview" ] Decode.string |> Decode.andThen (startingWith "https://"))


startingWith : String -> String -> Decoder String
startingWith prefix value =
    if String.startsWith prefix value then
        Decode.succeed value

    else
        Decode.fail ("Expected " ++ prefix)


call : Config -> String -> Encode.Value -> Decoder String -> Effect (Result String String)
call paddle path body decoder =
    Effect.Http
        { method = "POST"
        , headers =
            [ ( "authorization", "Bearer " ++ paddle.apiKey )
            , ( "paddle-version", "1" )
            ]
        , url = apiBase paddle.environment ++ path
        , body = Effect.JsonBody body
        , timeout = Just timeout
        , onResponse = outcome decoder
        }


outcome : Decoder String -> Http.Response String -> Result String String
outcome decoder response =
    case response of
        Http.GoodStatus_ _ body ->
            Decode.decodeString decoder body
                |> Result.mapError (\_ -> "Paddle's answer was not what we expected.")

        Http.BadStatus_ metadata body ->
            Decode.decodeString (rejection metadata.statusCode) body
                |> Result.withDefault ("Paddle answered " ++ String.fromInt metadata.statusCode ++ ".")
                |> Err

        Http.NetworkError_ ->
            Err "Could not reach Paddle."

        Http.Timeout_ ->
            Err "Paddle took too long to answer."

        Http.BadUrl_ url ->
            Err ("Bad Paddle endpoint: " ++ url)


rejection : Int -> Decoder String
rejection status =
    Decode.map2
        (\code detail ->
            String.left 200 ("Paddle answered " ++ String.fromInt status ++ " (" ++ code ++ "): " ++ detail)
        )
        (Decode.at [ "error", "code" ] Decode.string)
        (Decode.at [ "error", "detail" ] Decode.string)
