module RateLimit exposing (Outcome(..), Policy, describe, healthChecks, magicLinksByAddress, magicLinksByEmail, passkeyChallenges, retryAfter)

import Data.ClientAddress as ClientAddress exposing (ClientAddress)
import Data.Email as Email exposing (Email)


type Policy
    = Policy { key : String, limit : Int, windowSeconds : Int }


type Outcome
    = Allowed { remaining : Int }
    | Limited { retryAfter : Int }


describe : Policy -> { key : String, limit : Int, windowSeconds : Int }
describe (Policy policy) =
    policy


healthChecks : ClientAddress -> Policy
healthChecks address =
    Policy { key = "health:ip:" ++ ClientAddress.toString address, limit = 60, windowSeconds = 60 }


magicLinksByAddress : ClientAddress -> Policy
magicLinksByAddress address =
    Policy { key = "magic-link:ip:" ++ ClientAddress.toString address, limit = 10, windowSeconds = 900 }


magicLinksByEmail : Email -> Policy
magicLinksByEmail email =
    Policy { key = "magic-link:email:" ++ Email.toString email, limit = 3, windowSeconds = 900 }


passkeyChallenges : ClientAddress -> Policy
passkeyChallenges address =
    Policy { key = "passkey:ip:" ++ ClientAddress.toString address, limit = 20, windowSeconds = 900 }


retryAfter : List Outcome -> Maybe Int
retryAfter outcomes =
    outcomes
        |> List.filterMap
            (\outcome ->
                case outcome of
                    Allowed _ ->
                        Nothing

                    Limited limited ->
                        Just limited.retryAfter
            )
        |> List.maximum
