module Db exposing (RateLimit(..), connect, ping, rateLimit, signedIn)

import Json.Decode as Decode exposing (Decoder)
import Neon


connect : { a | databaseUrl : Maybe String, neonEndpoint : Maybe String } -> Result String Neon.Connection
connect config =
    case config.databaseUrl of
        Nothing ->
            Err "DATABASE_URL (a URL) and API_DATABASE_PASSWORD must both be set."

        Just url ->
            Neon.connect url
                |> Result.map
                    (\connection ->
                        config.neonEndpoint
                            |> Maybe.map (\endpoint -> Neon.withEndpoint endpoint connection)
                            |> Maybe.withDefault connection
                    )


ping : Neon.Query ()
ping =
    Neon.one
        ("SELECT rolsuper OR rolbypassrls OR EXISTS ("
            ++ "SELECT 1 FROM pg_tables WHERE schemaname = 'public' AND pg_has_role(current_user, tableowner, 'USAGE')"
            ++ ") AS exempt FROM pg_roles WHERE rolname = current_user"
        )
        []
        (Decode.field "exempt" Decode.bool
            |> Decode.andThen
                (\exempt ->
                    if exempt then
                        Decode.fail "The database role is exempt from row-level security."

                    else
                        Decode.succeed ()
                )
        )


signedIn : String -> (Maybe String -> a) -> Neon.Transaction a
signedIn token toResult =
    Neon.transaction toResult
        |> Neon.with
            (Neon.one "SELECT authenticate($1) AS user_id"
                [ Neon.text token ]
                (Decode.field "user_id" (Decode.nullable Decode.string))
            )


type RateLimit
    = Allowed { remaining : Int }
    | Limited { retryAfter : Int }


rateLimit : { key : String, limit : Int, windowSeconds : Int } -> Neon.Query RateLimit
rateLimit { key, limit, windowSeconds } =
    Neon.one "SELECT allowed, remaining, retry_after FROM rate_limit($1, $2, $3)"
        [ Neon.text key, Neon.int limit, Neon.int windowSeconds ]
        rateLimitDecoder


rateLimitDecoder : Decoder RateLimit
rateLimitDecoder =
    Decode.field "allowed" Decode.bool
        |> Decode.andThen
            (\allowed ->
                if allowed then
                    Decode.field "remaining" Decode.int
                        |> Decode.map (\remaining -> Allowed { remaining = remaining })

                else
                    Decode.field "retry_after" Decode.int
                        |> Decode.map (\retryAfter -> Limited { retryAfter = retryAfter })
            )
