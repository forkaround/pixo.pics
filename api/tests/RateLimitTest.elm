module RateLimitTest exposing (suite)

import Data.ClientAddress as ClientAddress exposing (ClientAddress)
import Data.Email as Email
import Dict
import Expect
import RateLimit
import Test exposing (Test, describe, test)


address : ClientAddress
address =
    ClientAddress.fromHeaders (Dict.fromList [ ( "x-real-ip", "203.0.113.7" ) ])


suite : Test
suite =
    describe "RateLimit"
        [ test "limits health checks to 60 a minute per address" <|
            \() ->
                RateLimit.healthChecks address
                    |> RateLimit.describe
                    |> Expect.equal { key = "health:ip:203.0.113.7", limit = 60, windowSeconds = 60 }
        , test "limits magic links to 10 per address" <|
            \() ->
                RateLimit.magicLinksByAddress address
                    |> RateLimit.describe
                    |> Expect.equal { key = "magic-link:ip:203.0.113.7", limit = 10, windowSeconds = 900 }
        , test "limits magic links to 3 per email" <|
            \() ->
                Email.parse "ada@example.com"
                    |> Result.map (RateLimit.magicLinksByEmail >> RateLimit.describe)
                    |> Expect.equal (Ok { key = "magic-link:email:ada@example.com", limit = 3, windowSeconds = 900 })
        , test "limits passkey challenges to 20 per address" <|
            \() ->
                RateLimit.passkeyChallenges address
                    |> RateLimit.describe
                    |> Expect.equal { key = "passkey:ip:203.0.113.7", limit = 20, windowSeconds = 900 }
        , test "waits for the slowest of several limits" <|
            \() ->
                RateLimit.retryAfter
                    [ RateLimit.Limited { retryAfter = 5 }
                    , RateLimit.Allowed { remaining = 1 }
                    , RateLimit.Limited { retryAfter = 40 }
                    ]
                    |> Expect.equal (Just 40)
        , test "waits for nothing when every limit allows" <|
            \() ->
                RateLimit.retryAfter [ RateLimit.Allowed { remaining = 1 } ]
                    |> Expect.equal Nothing
        ]
