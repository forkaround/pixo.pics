module EffectTest exposing (suite)

import Api
import ApiClient
import Effect exposing (Effect(..))
import Expect
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
            ]
        ]
