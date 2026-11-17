module EffectTest exposing (suite)

import Effect
import Expect
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "Effect"
        [ describe "encodeForm"
            [ test "joins percent-encoded pairs with ampersands" <|
                \() ->
                    Effect.encodeForm [ ( "grant_type", "refresh_token" ), ( "refresh_token", "a b&c=d" ) ]
                        |> Expect.equal "grant_type=refresh_token&refresh_token=a%20b%26c%3Dd"
            , test "is empty without pairs" <|
                \() ->
                    Effect.encodeForm []
                        |> Expect.equal ""
            ]
        ]
