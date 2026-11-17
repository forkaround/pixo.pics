module RateLimit exposing (Limits, Policy, check, checkouts, describe, empty, healthChecks, magicLinksByAddress, magicLinksByEmail, passkeyChallenges, portals, publishes, reports, requests, size, sweep, syncStarts, syncTokens, webhooks)

import Data.ClientAddress as ClientAddress exposing (ClientAddress)
import Data.Email as Email exposing (Email)
import Dict exposing (Dict)
import Time


type Policy
    = Policy { key : String, limit : Int, windowSeconds : Int }


type Limits
    = Limits { due : Dict String Int, sweptAt : Int }


describe : Policy -> { key : String, limit : Int, windowSeconds : Int }
describe (Policy policy) =
    policy


requests : ClientAddress -> Policy
requests address =
    Policy { key = "requests:ip:" ++ ClientAddress.toString address, limit = 30, windowSeconds = 60 }


healthChecks : ClientAddress -> Policy
healthChecks address =
    Policy { key = "health:ip:" ++ ClientAddress.toString address, limit = 10, windowSeconds = 60 }


webhooks : ClientAddress -> Policy
webhooks address =
    Policy { key = "webhooks:ip:" ++ ClientAddress.toString address, limit = 600, windowSeconds = 60 }


magicLinksByAddress : ClientAddress -> Policy
magicLinksByAddress address =
    Policy { key = "magic-link:ip:" ++ ClientAddress.toString address, limit = 5, windowSeconds = 900 }


magicLinksByEmail : Email -> Policy
magicLinksByEmail email =
    Policy { key = "magic-link:email:" ++ Email.toString email, limit = 3, windowSeconds = 3600 }


passkeyChallenges : ClientAddress -> Policy
passkeyChallenges address =
    Policy { key = "passkey:ip:" ++ ClientAddress.toString address, limit = 10, windowSeconds = 900 }


checkouts : ClientAddress -> Policy
checkouts address =
    Policy { key = "checkout:ip:" ++ ClientAddress.toString address, limit = 10, windowSeconds = 900 }


portals : ClientAddress -> Policy
portals address =
    Policy { key = "portal:ip:" ++ ClientAddress.toString address, limit = 10, windowSeconds = 900 }


publishes : ClientAddress -> Policy
publishes address =
    Policy { key = "publish:ip:" ++ ClientAddress.toString address, limit = 30, windowSeconds = 3600 }


reports : ClientAddress -> Policy
reports address =
    Policy { key = "report:ip:" ++ ClientAddress.toString address, limit = 10, windowSeconds = 3600 }


syncStarts : ClientAddress -> Policy
syncStarts address =
    Policy { key = "sync-start:ip:" ++ ClientAddress.toString address, limit = 10, windowSeconds = 900 }


syncTokens : ClientAddress -> Policy
syncTokens address =
    Policy { key = "sync-token:ip:" ++ ClientAddress.toString address, limit = 120, windowSeconds = 3600 }


empty : Limits
empty =
    Limits { due = Dict.empty, sweptAt = 0 }


check : Time.Posix -> List Policy -> Limits -> Result Int Limits
check time policies (Limits limits) =
    let
        ( waits, counted ) =
            List.foldl (count (Time.posixToMillis time)) ( [], limits.due ) policies
    in
    case List.maximum waits of
        Just wait ->
            Err (max 1 ((wait + 999) // 1000))

        Nothing ->
            Ok (Limits { limits | due = counted })


count : Int -> Policy -> ( List Int, Dict String Int ) -> ( List Int, Dict String Int )
count now (Policy policy) ( waits, due ) =
    let
        interval =
            max 1 ((policy.windowSeconds * 1000) // policy.limit)

        burst =
            interval * policy.limit

        next =
            max now (Maybe.withDefault now (Dict.get policy.key due)) + interval
    in
    if next - now <= burst then
        ( waits, Dict.insert policy.key next due )

    else
        ( (next - burst - now) :: waits, due )


sweep : Time.Posix -> Limits -> Limits
sweep time (Limits limits) =
    let
        now =
            Time.posixToMillis time
    in
    if now - limits.sweptAt < 60000 then
        Limits limits

    else
        Limits { due = Dict.filter (\_ until -> until > now) limits.due, sweptAt = now }


size : Limits -> Int
size (Limits limits) =
    Dict.size limits.due
