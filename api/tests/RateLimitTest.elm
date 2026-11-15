module RateLimitTest exposing (suite)

import Data.ClientAddress as ClientAddress exposing (ClientAddress)
import Data.Email as Email exposing (Email)
import Dict
import Expect
import RateLimit
import Test exposing (Test, describe, test)
import Time


address : ClientAddress
address =
    ClientAddress.fromHeaders (Dict.fromList [ ( "x-real-ip", "203.0.113.7" ) ])


elsewhere : ClientAddress
elsewhere =
    ClientAddress.fromHeaders (Dict.fromList [ ( "x-real-ip", "198.51.100.4" ) ])


at : Int -> Time.Posix
at millis =
    Time.millisToPosix (1794700800000 + millis)


fresh : Result Int RateLimit.Limits
fresh =
    Ok RateLimit.empty


withAda : (Email -> Expect.Expectation) -> Expect.Expectation
withAda toExpectation =
    case Email.parse "ada@example.com" of
        Ok ada ->
            toExpectation ada

        Err problem ->
            Expect.fail problem


hits : Int -> Int -> List RateLimit.Policy -> Result Int RateLimit.Limits -> Result Int RateLimit.Limits
hits times millis policies limits =
    List.foldl (\_ -> Result.andThen (RateLimit.check (at millis) policies)) limits (List.range 1 times)


suite : Test
suite =
    describe "RateLimit"
        [ test "limits health checks to 10 a minute per address" <|
            \() ->
                RateLimit.healthChecks address
                    |> RateLimit.describe
                    |> Expect.equal { key = "health:ip:203.0.113.7", limit = 10, windowSeconds = 60 }
        , test "limits magic links to 5 in 15 minutes per address" <|
            \() ->
                RateLimit.magicLinksByAddress address
                    |> RateLimit.describe
                    |> Expect.equal { key = "magic-link:ip:203.0.113.7", limit = 5, windowSeconds = 900 }
        , test "limits magic links to 3 an hour per email" <|
            \() ->
                Email.parse "ada@example.com"
                    |> Result.map (RateLimit.magicLinksByEmail >> RateLimit.describe)
                    |> Expect.equal (Ok { key = "magic-link:email:ada@example.com", limit = 3, windowSeconds = 3600 })
        , test "limits passkey challenges to 10 in 15 minutes per address" <|
            \() ->
                RateLimit.passkeyChallenges address
                    |> RateLimit.describe
                    |> Expect.equal { key = "passkey:ip:203.0.113.7", limit = 10, windowSeconds = 900 }
        , test "limits every request to 30 a minute per address" <|
            \() ->
                RateLimit.requests address
                    |> RateLimit.describe
                    |> Expect.equal { key = "requests:ip:203.0.113.7", limit = 30, windowSeconds = 60 }
        , describe "check"
            [ test "allows a policy's limit at once" <|
                \() ->
                    fresh
                        |> hits 30 0 [ RateLimit.requests address ]
                        |> Result.map (\_ -> ())
                        |> Expect.equal (Ok ())
            , test "refuses one more with the seconds until it may come" <|
                \() ->
                    fresh
                        |> hits 31 0 [ RateLimit.requests address ]
                        |> Expect.equal (Err 2)
            , test "allows one more once its interval has passed" <|
                \() ->
                    fresh
                        |> hits 30 0 [ RateLimit.requests address ]
                        |> hits 1 2000 [ RateLimit.requests address ]
                        |> Result.map (\_ -> ())
                        |> Expect.equal (Ok ())
            , test "allows only one more once its interval has passed" <|
                \() ->
                    fresh
                        |> hits 30 0 [ RateLimit.requests address ]
                        |> hits 2 2000 [ RateLimit.requests address ]
                        |> Expect.equal (Err 2)
            , test "rounds a wait up to whole seconds" <|
                \() ->
                    fresh
                        |> hits 30 0 [ RateLimit.requests address ]
                        |> hits 1 500 [ RateLimit.requests address ]
                        |> Expect.equal (Err 2)
            , test "waits for the slowest of several policies, and counts none" <|
                \() ->
                    withAda
                        (\ada ->
                            fresh
                                |> hits 10 0 [ RateLimit.healthChecks address ]
                                |> hits 3 0 [ RateLimit.magicLinksByEmail ada ]
                                |> hits 1 0 [ RateLimit.healthChecks address, RateLimit.magicLinksByEmail ada ]
                                |> Expect.equal (Err 1200)
                        )
            , test "keeps each key apart" <|
                \() ->
                    fresh
                        |> hits 30 0 [ RateLimit.requests address ]
                        |> hits 30 0 [ RateLimit.requests elsewhere ]
                        |> Result.map (\_ -> ())
                        |> Expect.equal (Ok ())
            ]
        , describe "sweep"
            [ test "forgets keys whose wait is over" <|
                \() ->
                    fresh
                        |> hits 30 0 [ RateLimit.requests address ]
                        |> Result.map (RateLimit.sweep (at 61000) >> RateLimit.size)
                        |> Expect.equal (Ok 0)
            , test "keeps keys that still have a wait" <|
                \() ->
                    fresh
                        |> hits 30 0 [ RateLimit.requests address ]
                        |> Result.map (RateLimit.sweep (at 30000) >> RateLimit.size)
                        |> Expect.equal (Ok 1)
            , test "sweeps at most once a minute" <|
                \() ->
                    fresh
                        |> Result.map (RateLimit.sweep (at 0))
                        |> hits 1 0 [ RateLimit.requests address ]
                        |> Result.map (RateLimit.sweep (at 30000))
                        |> Result.map (\limits -> ( RateLimit.size limits, RateLimit.size (RateLimit.sweep (at 60000) limits) ))
                        |> Expect.equal (Ok ( 1, 0 ))
            ]
        ]
