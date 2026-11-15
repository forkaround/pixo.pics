module EffectTest exposing (suite)

import Api
import ApiClient
import Effect exposing (Effect(..))
import Expect
import Notice
import Test exposing (Test, describe, test)
import Test.Http
import Time


suite : Test
suite =
    describe "Effect"
        [ describe "map"
            [ test "wraps the message a response becomes" <|
                \() ->
                    case Effect.map Just (Request (ApiClient.request Api.signOut () identity)) of
                        Request request ->
                            request.onResponse (Test.Http.httpResponse { statusCode = 200, headers = [], body = "{}" })
                                |> Expect.equal (Just (Ok ()))

                        _ ->
                            Expect.fail "Expected a request."
            , test "wraps the message the time becomes" <|
                \() ->
                    case Effect.map String.fromInt (ReadTime Time.posixToMillis) of
                        ReadTime toMsg ->
                            toMsg (Time.millisToPosix 5)
                                |> Expect.equal "5"

                        _ ->
                            Expect.fail "Expected the time to be read."
            , test "reaches every effect in a batch" <|
                \() ->
                    case Effect.map String.fromInt (Batch [ PushUrl "/", ReadTime Time.posixToMillis ]) of
                        Batch [ PushUrl "/", ReadTime toMsg ] ->
                            toMsg (Time.millisToPosix 7)
                                |> Expect.equal "7"

                        _ ->
                            Expect.fail "Expected the same batch."
            , test "wraps the message a sleep ends with" <|
                \() ->
                    Effect.map String.fromInt (Sleep 10 5)
                        |> Expect.equal (Sleep 10 "5")
            , test "wraps the message a notice retries with" <|
                \() ->
                    case Effect.map String.fromInt (Notify { id = "a", tone = Notice.Problem, title = "T", description = Nothing, retry = Notice.RetryIn 3 5 }) of
                        Notify notice ->
                            notice.retry
                                |> Expect.equal (Notice.RetryIn 3 "5")

                        _ ->
                            Expect.fail "Expected a notice."
            ]
        , describe "failure"
            [ test "sends a throttled request again after the wait" <|
                \() ->
                    case Effect.failure { id = "a", title = "T", retry = Just 1 } (ApiClient.Throttled 2 "x") of
                        Batch [ Notify _, Sleep milliseconds 1 ] ->
                            Expect.equal 2000 milliseconds

                        _ ->
                            Expect.fail "Expected a notice and a sleep."
            , test "only notifies of another failure" <|
                \() ->
                    case Effect.failure { id = "a", title = "T", retry = Just 1 } (ApiClient.Failed "x") of
                        Notify notice ->
                            notice.retry
                                |> Expect.equal (Notice.Retry 1)

                        _ ->
                            Expect.fail "Expected a single notice."
            ]
        ]
