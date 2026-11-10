module Db exposing (Consumed(..), Link(..), RateLimit(..), account, connect, consumeMagicLink, createSignInLink, createSignUpLink, magicLink, ping, rateLimit, signOut, signedIn)

import Api
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


type Link
    = SignUpLink String
    | SignInLink String
    | UsernameTaken
    | NoAccount


createSignInLink : { email : String, ttlSeconds : Int } -> Neon.Query Link
createSignInLink { email, ttlSeconds } =
    Neon.one "SELECT outcome, token FROM create_sign_in_link($1, $2)"
        [ Neon.text email, Neon.int ttlSeconds ]
        linkDecoder


createSignUpLink : { email : String, username : String, ttlSeconds : Int } -> Neon.Query Link
createSignUpLink { email, username, ttlSeconds } =
    Neon.one "SELECT outcome, token FROM create_sign_up_link($1, $2, $3)"
        [ Neon.text email, Neon.text username, Neon.int ttlSeconds ]
        linkDecoder


linkDecoder : Decoder Link
linkDecoder =
    Decode.field "outcome" Decode.string
        |> Decode.andThen
            (\outcome ->
                case outcome of
                    "sign-up" ->
                        Decode.map SignUpLink (Decode.field "token" Decode.string)

                    "sign-in" ->
                        Decode.map SignInLink (Decode.field "token" Decode.string)

                    "username-taken" ->
                        Decode.succeed UsernameTaken

                    "no-account" ->
                        Decode.succeed NoAccount

                    _ ->
                        Decode.fail ("Unknown link outcome: " ++ outcome)
            )


magicLink : String -> Neon.Query (Maybe Api.Verification)
magicLink token =
    Neon.selectOne "SELECT email, username, new_account FROM magic_link($1)"
        [ Neon.text token ]
        (Decode.map3 Api.Verification
            (Decode.field "email" Decode.string)
            (Decode.field "username" Decode.string)
            (Decode.field "new_account" Decode.bool)
        )


type Consumed
    = Verified { sessionToken : String, account : Api.Account }
    | InvalidLink
    | UsernameTakenMeanwhile


consumeMagicLink : { token : String, sessionSeconds : Int } -> Neon.Query Consumed
consumeMagicLink { token, sessionSeconds } =
    Neon.one "SELECT outcome, session_token, email, username FROM consume_magic_link($1, $2)"
        [ Neon.text token, Neon.int sessionSeconds ]
        consumedDecoder


consumedDecoder : Decoder Consumed
consumedDecoder =
    Decode.field "outcome" Decode.string
        |> Decode.andThen
            (\outcome ->
                case outcome of
                    "verified" ->
                        Decode.map3
                            (\sessionToken email username ->
                                Verified { sessionToken = sessionToken, account = { email = email, username = username } }
                            )
                            (Decode.field "session_token" Decode.string)
                            (Decode.field "email" Decode.string)
                            (Decode.field "username" Decode.string)

                    "invalid" ->
                        Decode.succeed InvalidLink

                    "username-taken" ->
                        Decode.succeed UsernameTakenMeanwhile

                    _ ->
                        Decode.fail ("Unknown verification outcome: " ++ outcome)
            )


account : String -> Neon.Transaction (Maybe Api.Account)
account token =
    signedIn token (\_ found -> found)
        |> Neon.with
            (Neon.selectOne "SELECT email, username FROM users"
                []
                (Decode.map2 Api.Account
                    (Decode.field "email" Decode.string)
                    (Decode.field "username" Decode.string)
                )
            )


signOut : String -> Neon.Query ()
signOut token =
    Neon.execute "SELECT sign_out($1)" [ Neon.text token ]
        |> Neon.map (\_ -> ())
