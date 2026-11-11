module Db exposing (Added(..), Ceremony, Consumed(..), PasskeySignIn(..), RateLimit(..), Removed(..), SignInChallenge(..), SignInLink(..), SignUpLink(..), account, addPasskey, connect, consumeMagicLink, createSignInLink, createSignUpLink, magicLink, passkeyPublicKey, passkeys, ping, rateLimit, removePasskey, signInChallenge, signInWithPasskey, signOut, signedIn, startPasskey)

import Api
import Data.Email as Email exposing (Email)
import Data.LinkToken as LinkToken exposing (LinkToken)
import Data.SessionToken as SessionToken exposing (SessionToken)
import Data.Username as Username exposing (Username)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Services.Neon as Neon
import WebAuthn


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


signedIn : SessionToken -> (Maybe String -> a) -> Neon.Transaction a
signedIn token toResult =
    Neon.transaction toResult
        |> Neon.with
            (Neon.one "SELECT authenticate($1) AS user_id"
                [ Neon.text (SessionToken.toString token) ]
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


type SignUpLink
    = SignUpLink LinkToken
    | SignInInstead LinkToken
    | UsernameTaken


type SignInLink
    = SignInLink LinkToken
    | NoAccount


createSignInLink : { email : Email, ttlSeconds : Int } -> Neon.Query SignInLink
createSignInLink { email, ttlSeconds } =
    Neon.one "SELECT outcome, token FROM create_sign_in_link($1, $2)"
        [ Neon.text (Email.toString email), Neon.int ttlSeconds ]
        signInLinkDecoder


createSignUpLink : { email : Email, username : Username, ttlSeconds : Int } -> Neon.Query SignUpLink
createSignUpLink { email, username, ttlSeconds } =
    Neon.one "SELECT outcome, token FROM create_sign_up_link($1, $2, $3)"
        [ Neon.text (Email.toString email), Neon.text (Username.toString username), Neon.int ttlSeconds ]
        signUpLinkDecoder


signUpLinkDecoder : Decoder SignUpLink
signUpLinkDecoder =
    Decode.field "outcome" Decode.string
        |> Decode.andThen
            (\outcome ->
                case outcome of
                    "sign-up" ->
                        Decode.map SignUpLink (Decode.field "token" LinkToken.decoder)

                    "sign-in" ->
                        Decode.map SignInInstead (Decode.field "token" LinkToken.decoder)

                    "username-taken" ->
                        Decode.succeed UsernameTaken

                    _ ->
                        Decode.fail ("Unknown sign-up link outcome: " ++ outcome)
            )


signInLinkDecoder : Decoder SignInLink
signInLinkDecoder =
    Decode.field "outcome" Decode.string
        |> Decode.andThen
            (\outcome ->
                case outcome of
                    "sign-in" ->
                        Decode.map SignInLink (Decode.field "token" LinkToken.decoder)

                    "no-account" ->
                        Decode.succeed NoAccount

                    _ ->
                        Decode.fail ("Unknown sign-in link outcome: " ++ outcome)
            )


magicLink : LinkToken -> Neon.Query (Maybe Api.Verification)
magicLink token =
    Neon.selectOne "SELECT email, username, new_account FROM magic_link($1)"
        [ Neon.text (LinkToken.toString token) ]
        (Decode.map3 Api.Verification
            (Decode.field "email" Email.decoder)
            (Decode.field "username" Username.decoder)
            (Decode.field "new_account" Decode.bool)
        )


type Consumed
    = Verified { sessionToken : SessionToken, account : Api.Account }
    | InvalidLink
    | UsernameTakenMeanwhile


consumeMagicLink : { token : LinkToken, sessionSeconds : Int } -> Neon.Query Consumed
consumeMagicLink { token, sessionSeconds } =
    Neon.one "SELECT outcome, session_token, email, username FROM consume_magic_link($1, $2)"
        [ Neon.text (LinkToken.toString token), Neon.int sessionSeconds ]
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
                            (Decode.field "session_token" SessionToken.decoder)
                            (Decode.field "email" Email.decoder)
                            (Decode.field "username" Username.decoder)

                    "invalid" ->
                        Decode.succeed InvalidLink

                    "username-taken" ->
                        Decode.succeed UsernameTakenMeanwhile

                    _ ->
                        Decode.fail ("Unknown verification outcome: " ++ outcome)
            )


account : SessionToken -> Neon.Transaction (Maybe Api.Account)
account token =
    signedIn token (\_ found -> found)
        |> Neon.with ownAccount


ownAccount : Neon.Query (Maybe Api.Account)
ownAccount =
    Neon.selectOne "SELECT email, username FROM users"
        []
        (Decode.map2 Api.Account
            (Decode.field "email" Email.decoder)
            (Decode.field "username" Username.decoder)
        )


passkeys : SessionToken -> Neon.Transaction (Maybe (List Api.Passkey))
passkeys token =
    signedIn token (\userId found -> Maybe.map (\_ -> found) userId)
        |> Neon.with
            (Neon.select ("SELECT credential_id, name, " ++ createdAt ++ " FROM passkeys ORDER BY passkeys.created_at")
                []
                passkeyDecoder
            )


passkeyDecoder : Decoder Api.Passkey
passkeyDecoder =
    Decode.map3 Api.Passkey
        (Decode.field "credential_id" Decode.string)
        (Decode.field "name" (Decode.nullable Decode.string))
        (Decode.field "created_at" Decode.string)


createdAt : String
createdAt =
    "to_char(created_at AT TIME ZONE 'UTC', 'YYYY-MM-DD\"T\"HH24:MI:SS\"Z\"') AS created_at"


type alias Ceremony =
    { challenge : String
    , userId : String
    , account : Api.Account
    , passkeys : List String
    }


startPasskey : { token : SessionToken, ttlSeconds : Int } -> Neon.Transaction (Maybe Ceremony)
startPasskey { token, ttlSeconds } =
    signedIn token
        (\userId _ challenge found credentialIds ->
            Maybe.map3
                (\id made owner ->
                    { challenge = made
                    , userId = id
                    , account = owner
                    , passkeys = credentialIds
                    }
                )
                userId
                challenge
                found
        )
        |> Neon.with (Neon.execute "DELETE FROM webauthn_challenges WHERE ceremony = 'registration'" [])
        |> Neon.with
            (Neon.selectOne
                ("INSERT INTO webauthn_challenges (challenge, ceremony, user_id, expires_at) "
                    ++ "SELECT translate(rtrim(encode(uuid_send(gen_random_uuid()) || uuid_send(gen_random_uuid()), 'base64'), '='), '+/', '-_'), "
                    ++ "'registration', NULLIF(current_setting('app.user_id', true), '')::uuid, now() + $1::int * interval '1 second' "
                    ++ "WHERE NULLIF(current_setting('app.user_id', true), '') IS NOT NULL "
                    ++ "RETURNING challenge"
                )
                [ Neon.int ttlSeconds ]
                (Decode.field "challenge" Decode.string)
            )
        |> Neon.with ownAccount
        |> Neon.with
            (Neon.select "SELECT credential_id FROM passkeys ORDER BY created_at"
                []
                (Decode.field "credential_id" Decode.string)
            )


type Added
    = Added Api.Passkey Api.Account
    | NotAdded


addPasskey : { token : SessionToken, registration : WebAuthn.Registration, name : Maybe String } -> Neon.Transaction (Maybe Added)
addPasskey { token, registration, name } =
    signedIn token
        (\userId added found ->
            Maybe.map2 (\_ owner -> Maybe.withDefault NotAdded (Maybe.map (\passkey -> Added passkey owner) added))
                userId
                found
        )
        |> Neon.with
            (Neon.selectOne
                ("WITH used AS ("
                    ++ "DELETE FROM webauthn_challenges "
                    ++ "WHERE challenge = $1 AND ceremony = 'registration' AND expires_at > now() "
                    ++ "RETURNING user_id"
                    ++ ") "
                    ++ "INSERT INTO passkeys (credential_id, user_id, public_key, sign_count, transports, backup_eligible, backed_up, name) "
                    ++ "SELECT $2::text, used.user_id, $3::text, $4::bigint, ARRAY(SELECT jsonb_array_elements_text($5::jsonb)), $6::boolean, $7::boolean, $8::text "
                    ++ "FROM used "
                    ++ "ON CONFLICT (credential_id) DO NOTHING "
                    ++ "RETURNING credential_id, name, "
                    ++ createdAt
                )
                [ Neon.text registration.challenge
                , Neon.text registration.credentialId
                , Neon.text registration.publicKey
                , Neon.int registration.signCount
                , Neon.json (Encode.list Encode.string registration.transports)
                , Neon.bool registration.backupEligible
                , Neon.bool registration.backedUp
                , Neon.nullable Neon.text name
                ]
                passkeyDecoder
            )
        |> Neon.with ownAccount


type Removed
    = Removed { name : Maybe String, account : Api.Account }
    | NotRemoved


removePasskey : { token : SessionToken, id : String } -> Neon.Transaction (Maybe Removed)
removePasskey { token, id } =
    signedIn token
        (\userId removed found ->
            Maybe.map2
                (\_ owner ->
                    removed
                        |> Maybe.map (\name -> Removed { name = name, account = owner })
                        |> Maybe.withDefault NotRemoved
                )
                userId
                found
        )
        |> Neon.with
            (Neon.selectOne "DELETE FROM passkeys WHERE credential_id = $1 RETURNING name"
                [ Neon.text id ]
                (Decode.field "name" (Decode.nullable Decode.string))
            )
        |> Neon.with ownAccount


type SignInChallenge
    = SignInChallenge String
    | TooManyChallenges { retryAfter : Int }


signInChallenge : { key : String, limit : Int, windowSeconds : Int, ttlSeconds : Int } -> Neon.Query SignInChallenge
signInChallenge { key, limit, windowSeconds, ttlSeconds } =
    Neon.one
        ("SELECT l.retry_after, CASE WHEN l.allowed THEN create_authentication_challenge($4) END AS challenge "
            ++ "FROM rate_limit($1, $2, $3) AS l"
        )
        [ Neon.text key, Neon.int limit, Neon.int windowSeconds, Neon.int ttlSeconds ]
        (Decode.field "challenge" (Decode.nullable Decode.string)
            |> Decode.andThen
                (\challenge ->
                    case challenge of
                        Just made ->
                            Decode.succeed (SignInChallenge made)

                        Nothing ->
                            Decode.field "retry_after" Decode.int
                                |> Decode.map (\retryAfter -> TooManyChallenges { retryAfter = retryAfter })
                )
        )


passkeyPublicKey : String -> Neon.Query (Maybe String)
passkeyPublicKey credentialId =
    Neon.one "SELECT passkey_public_key($1) AS public_key"
        [ Neon.text credentialId ]
        (Decode.field "public_key" (Decode.nullable Decode.string))


type PasskeySignIn
    = PasskeySignedIn { sessionToken : SessionToken, account : Api.Account }
    | ChallengeSpent
    | PasskeyStale


signInWithPasskey :
    { challenge : String, credentialId : String, signCount : Int, backedUp : Bool, sessionSeconds : Int }
    -> Neon.Query PasskeySignIn
signInWithPasskey { challenge, credentialId, signCount, backedUp, sessionSeconds } =
    Neon.one "SELECT outcome, session_token, email, username FROM sign_in_with_passkey($1, $2, $3, $4, $5)"
        [ Neon.text challenge
        , Neon.text credentialId
        , Neon.int signCount
        , Neon.bool backedUp
        , Neon.int sessionSeconds
        ]
        (Decode.field "outcome" Decode.string
            |> Decode.andThen
                (\outcome ->
                    case outcome of
                        "signed-in" ->
                            Decode.map3
                                (\sessionToken email username ->
                                    PasskeySignedIn
                                        { sessionToken = sessionToken
                                        , account = { email = email, username = username }
                                        }
                                )
                                (Decode.field "session_token" SessionToken.decoder)
                                (Decode.field "email" Email.decoder)
                                (Decode.field "username" Username.decoder)

                        "no-challenge" ->
                            Decode.succeed ChallengeSpent

                        "no-passkey" ->
                            Decode.succeed PasskeyStale

                        _ ->
                            Decode.fail ("Unknown passkey sign-in outcome: " ++ outcome)
                )
        )


signOut : SessionToken -> Neon.Query ()
signOut token =
    Neon.execute "SELECT sign_out($1)" [ Neon.text (SessionToken.toString token) ]
        |> Neon.map (\_ -> ())
