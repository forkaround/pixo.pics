module Db exposing (Added(..), Asked(..), Ceremony, Consumed(..), Deletion(..), PasskeySignIn(..), PictureRow, ProfileRow, Removed(..), SignInLink(..), SignUpLink(..), Subscription, Unconfirmed(..), addPasskey, applySubscription, askPicture, beforeDeletion, checkoutState, connect, consumeMagicLink, createSignInLink, createSignUpLink, deleteAccount, dropPicture, feed, feedTags, magicLink, markPublished, me, ownPicture, passkeyPublicKey, passkeys, ping, profile, removePasskey, signInChallenge, signInWithPasskey, signOut, signedIn, startPasskey, subscriptions, unconfirmed)

import Api
import Data.Challenge as Challenge exposing (Challenge)
import Data.CredentialId as CredentialId exposing (CredentialId)
import Data.Email as Email exposing (Email)
import Data.LinkToken as LinkToken exposing (LinkToken)
import Data.PictureId as PictureId exposing (PictureId)
import Data.Publication exposing (Publication)
import Data.SessionToken as SessionToken exposing (SessionToken)
import Data.Tag as Tag exposing (Tag)
import Data.UserId as UserId exposing (UserId)
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


signedIn : SessionToken -> (Maybe UserId -> a) -> Neon.Transaction a
signedIn token toResult =
    Neon.transaction toResult
        |> Neon.with
            (Neon.one "SELECT authenticate($1) AS user_id"
                [ Neon.text (SessionToken.toString token) ]
                (Decode.field "user_id" (Decode.nullable UserId.decoder))
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


me : SessionToken -> Neon.Transaction (Maybe ( Api.Account, List Subscription ))
me token =
    signedIn token (\_ found rows -> Maybe.map (\owner -> ( owner, rows )) found)
        |> Neon.with ownAccount
        |> Neon.with subscriptionRows


type alias Subscription =
    { status : String
    , scheduledChange : Maybe String
    , scheduledChangeAt : Maybe String
    , periodEndsAt : Maybe String
    , customerId : String
    , subscriptionId : String
    }


subscriptions : SessionToken -> Neon.Transaction (Maybe (List Subscription))
subscriptions token =
    signedIn token (\userId found -> Maybe.map (\_ -> found) userId)
        |> Neon.with subscriptionRows


subscriptionRows : Neon.Query (List Subscription)
subscriptionRows =
    Neon.select
        ("SELECT status, scheduled_change, "
            ++ utc "scheduled_change_at"
            ++ " AS scheduled_change_at, "
            ++ utc "current_period_ends_at"
            ++ " AS current_period_ends_at, paddle_customer_id, paddle_subscription_id "
            ++ "FROM subscriptions "
            ++ "ORDER BY (status IN ('active', 'trialing', 'past_due')) DESC, paddle_updated_at DESC"
        )
        []
        (Decode.map6 Subscription
            (Decode.field "status" Decode.string)
            (Decode.field "scheduled_change" (Decode.nullable Decode.string))
            (Decode.field "scheduled_change_at" (Decode.nullable Decode.string))
            (Decode.field "current_period_ends_at" (Decode.nullable Decode.string))
            (Decode.field "paddle_customer_id" Decode.string)
            (Decode.field "paddle_subscription_id" Decode.string)
        )


utc : String -> String
utc column =
    "to_char(" ++ column ++ " AT TIME ZONE 'UTC', 'YYYY-MM-DD\"T\"HH24:MI:SS\"Z\"')"


checkoutState : SessionToken -> Neon.Transaction (Maybe { userId : UserId, pro : Bool, customerId : Maybe String })
checkoutState token =
    signedIn token
        (\userId state ->
            Maybe.map (\id -> { userId = id, pro = state.pro, customerId = state.customerId }) userId
        )
        |> Neon.with
            (Neon.one
                ("SELECT is_pro(NULLIF(current_setting('app.user_id', true), '')::uuid) AS pro, "
                    ++ "(SELECT paddle_customer_id FROM subscriptions ORDER BY paddle_updated_at DESC LIMIT 1) AS customer_id"
                )
                []
                (Decode.map2 (\pro customerId -> { pro = pro, customerId = customerId })
                    (Decode.field "pro" Decode.bool)
                    (Decode.field "customer_id" (Decode.nullable Decode.string))
                )
            )


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
        (Decode.field "credential_id" CredentialId.decoder)
        (Decode.field "name" (Decode.nullable Decode.string))
        (Decode.field "created_at" Decode.string)


createdAt : String
createdAt =
    "to_char(created_at AT TIME ZONE 'UTC', 'YYYY-MM-DD\"T\"HH24:MI:SS\"Z\"') AS created_at"


type alias Ceremony =
    { challenge : Challenge
    , userId : UserId
    , account : Api.Account
    , passkeys : List CredentialId
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
                (Decode.field "challenge" Challenge.decoder)
            )
        |> Neon.with ownAccount
        |> Neon.with
            (Neon.select "SELECT credential_id FROM passkeys ORDER BY created_at"
                []
                (Decode.field "credential_id" CredentialId.decoder)
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
                [ Neon.text (Challenge.toString registration.challenge)
                , Neon.text (CredentialId.toString registration.credentialId)
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


removePasskey : { token : SessionToken, id : CredentialId } -> Neon.Transaction (Maybe Removed)
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
                [ Neon.text (CredentialId.toString id) ]
                (Decode.field "name" (Decode.nullable Decode.string))
            )
        |> Neon.with ownAccount


signInChallenge : { ttlSeconds : Int } -> Neon.Query Challenge
signInChallenge { ttlSeconds } =
    Neon.one "SELECT create_authentication_challenge($1) AS challenge"
        [ Neon.int ttlSeconds ]
        (Decode.field "challenge" Challenge.decoder)


passkeyPublicKey : CredentialId -> Neon.Query (Maybe String)
passkeyPublicKey credentialId =
    Neon.one "SELECT passkey_public_key($1) AS public_key"
        [ Neon.text (CredentialId.toString credentialId) ]
        (Decode.field "public_key" (Decode.nullable Decode.string))


type PasskeySignIn
    = PasskeySignedIn { sessionToken : SessionToken, account : Api.Account }
    | ChallengeSpent
    | PasskeyStale


signInWithPasskey :
    { challenge : Challenge, credentialId : CredentialId, signCount : Int, backedUp : Bool, sessionSeconds : Int }
    -> Neon.Query PasskeySignIn
signInWithPasskey { challenge, credentialId, signCount, backedUp, sessionSeconds } =
    Neon.one "SELECT outcome, session_token, email, username FROM sign_in_with_passkey($1, $2, $3, $4, $5)"
        [ Neon.text (Challenge.toString challenge)
        , Neon.text (CredentialId.toString credentialId)
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


type Deletion
    = Deleted
    | Renewing
    | NotSignedIn


deleteAccount : SessionToken -> Neon.Transaction Deletion
deleteAccount token =
    signedIn token (\_ outcome -> outcome)
        |> Neon.with
            (Neon.one
                ("SELECT CASE "
                    ++ "WHEN EXISTS (SELECT 1 FROM subscriptions WHERE status IN ('active', 'trialing', 'past_due') "
                    ++ "AND scheduled_change IS DISTINCT FROM 'cancel') THEN 'renewing' "
                    ++ "WHEN delete_account($1) THEN 'deleted' "
                    ++ "ELSE 'signed-out' END AS outcome"
                )
                [ Neon.text (SessionToken.toString token) ]
                (Decode.field "outcome" Decode.string
                    |> Decode.andThen
                        (\outcome ->
                            case outcome of
                                "deleted" ->
                                    Decode.succeed Deleted

                                "renewing" ->
                                    Decode.succeed Renewing

                                "signed-out" ->
                                    Decode.succeed NotSignedIn

                                _ ->
                                    Decode.fail ("Unknown deletion outcome: " ++ outcome)
                        )
                )
            )


applySubscription :
    { id : String
    , userId : Maybe UserId
    , customerId : String
    , status : String
    , priceId : String
    , periodEnds : Maybe String
    , scheduledChange : Maybe String
    , scheduledChangeAt : Maybe String
    , updatedAt : String
    }
    -> Neon.Query String
applySubscription subscription =
    Neon.one "SELECT apply_paddle_subscription($1, $2::uuid, $3, $4, $5, $6::timestamptz, $7, $8::timestamptz, $9::timestamptz) AS outcome"
        [ Neon.text subscription.id
        , Neon.nullable (UserId.toString >> Neon.text) subscription.userId
        , Neon.text subscription.customerId
        , Neon.text subscription.status
        , Neon.text subscription.priceId
        , Neon.nullable Neon.text subscription.periodEnds
        , Neon.nullable Neon.text subscription.scheduledChange
        , Neon.nullable Neon.text subscription.scheduledChangeAt
        , Neon.text subscription.updatedAt
        ]
        (Decode.field "outcome" Decode.string)


type Asked
    = AskedFor PictureId
    | NotPro
    | TooManyUnfinished


askPicture : SessionToken -> Publication -> Neon.Transaction (Maybe Asked)
askPicture token publication =
    signedIn token (\userId asked -> Maybe.map (\_ -> asked) userId)
        |> Neon.with
            (Neon.one
                ("WITH me AS ("
                    ++ "SELECT NULLIF(current_setting('app.user_id', true), '')::uuid AS id"
                    ++ "), state AS ("
                    ++ "SELECT me.id, is_pro(me.id) AS pro, "
                    ++ "(SELECT count(*) FROM published_pictures AS p "
                    ++ "WHERE p.published_at IS NULL AND p.created_at > now() - interval '1 hour') AS unfinished "
                    ++ "FROM me"
                    ++ "), added AS ("
                    ++ "INSERT INTO published_pictures (user_id, prompt, width, height, steps, seed, model, bytes, tags) "
                    ++ "SELECT state.id, $1::text, $2::integer, $3::integer, $4::integer, $5::bigint, $6::text, $7::integer, "
                    ++ "ARRAY(SELECT t.tag FROM jsonb_array_elements_text($8::jsonb) WITH ORDINALITY AS t(tag, n) ORDER BY t.n) "
                    ++ "FROM state "
                    ++ "WHERE state.pro AND state.unfinished < 3 "
                    ++ "RETURNING id"
                    ++ ") "
                    ++ "SELECT (SELECT pro FROM state) AS pro, (SELECT id FROM added) AS id"
                )
                [ Neon.text publication.prompt
                , Neon.int publication.width
                , Neon.int publication.height
                , Neon.int publication.steps
                , Neon.int publication.seed
                , Neon.text publication.model
                , Neon.int publication.bytes
                , Neon.json (Encode.list Tag.encode publication.tags)
                ]
                (Decode.map2
                    (\pro id ->
                        case id of
                            Just added ->
                                AskedFor added

                            Nothing ->
                                if pro then
                                    TooManyUnfinished

                                else
                                    NotPro
                    )
                    (Decode.field "pro" (Decode.nullable Decode.bool) |> Decode.map (Maybe.withDefault False))
                    (Decode.field "id" (Decode.nullable PictureId.decoder))
                )
            )


type Unconfirmed
    = Waiting Int
    | AlreadyPublished String
    | Missing


publishedAt : String
publishedAt =
    "to_char(published_at AT TIME ZONE 'UTC', 'YYYY-MM-DD\"T\"HH24:MI:SS\"Z\"') AS published_at"


unconfirmed : SessionToken -> PictureId -> Neon.Transaction (Maybe Unconfirmed)
unconfirmed token id =
    signedIn token (\userId found -> Maybe.map (\_ -> Maybe.withDefault Missing found) userId)
        |> Neon.with
            (Neon.selectOne ("SELECT bytes, " ++ publishedAt ++ " FROM published_pictures WHERE id = $1")
                [ Neon.text (PictureId.toString id) ]
                (Decode.map2
                    (\bytes at ->
                        case at of
                            Just when ->
                                AlreadyPublished when

                            Nothing ->
                                Waiting bytes
                    )
                    (Decode.field "bytes" Decode.int)
                    (Decode.field "published_at" (Decode.nullable Decode.string))
                )
            )


markPublished : SessionToken -> PictureId -> Neon.Transaction (Maybe String)
markPublished token id =
    signedIn token (\_ marked -> marked)
        |> Neon.with
            (Neon.selectOne
                ("UPDATE published_pictures SET published_at = now() WHERE id = $1 AND published_at IS NULL RETURNING " ++ publishedAt)
                [ Neon.text (PictureId.toString id) ]
                (Decode.field "published_at" Decode.string)
            )


dropPicture : SessionToken -> PictureId -> Neon.Transaction Bool
dropPicture token id =
    signedIn token (\_ dropped -> dropped /= Nothing)
        |> Neon.with
            (Neon.selectOne "DELETE FROM published_pictures WHERE id = $1 RETURNING id"
                [ Neon.text (PictureId.toString id) ]
                (Decode.field "id" PictureId.decoder)
            )


ownPicture : SessionToken -> PictureId -> Neon.Transaction (Maybe Bool)
ownPicture token id =
    signedIn token (\userId found -> Maybe.map (\_ -> found) userId)
        |> Neon.with
            (Neon.one "SELECT EXISTS (SELECT 1 FROM published_pictures WHERE id = $1) AS found"
                [ Neon.text (PictureId.toString id) ]
                (Decode.field "found" Decode.bool)
            )


beforeDeletion : SessionToken -> Neon.Transaction (Maybe { renewing : Bool, pictures : List PictureId })
beforeDeletion token =
    signedIn token (\userId state -> Maybe.map (\_ -> state) userId)
        |> Neon.with
            (Neon.one
                ("SELECT EXISTS ("
                    ++ "SELECT 1 FROM subscriptions "
                    ++ "WHERE status IN ('active', 'trialing', 'past_due') "
                    ++ "AND scheduled_change IS DISTINCT FROM 'cancel'"
                    ++ ") AS renewing, "
                    ++ "COALESCE((SELECT json_agg(id) FROM published_pictures), '[]'::json) AS pictures"
                )
                []
                (Decode.map2 (\renewing pictures -> { renewing = renewing, pictures = pictures })
                    (Decode.field "renewing" Decode.bool)
                    (Decode.field "pictures" (Decode.list PictureId.decoder))
                )
            )


type alias PictureRow =
    { id : PictureId
    , username : Username
    , prompt : String
    , width : Int
    , height : Int
    , steps : Int
    , seed : Int
    , model : String
    , tags : List Tag
    , publishedAt : String
    }


type alias ProfileRow =
    { username : Username
    , pictures : List PictureRow
    }


feed : Maybe Tag -> Maybe PictureId -> Int -> Neon.Query (List PictureRow)
feed tag before limit =
    Neon.select "SELECT * FROM feed_pictures($1, $2::uuid, $3)"
        [ Neon.nullable (Tag.toString >> Neon.text) tag
        , Neon.nullable (PictureId.toString >> Neon.text) before
        , Neon.int limit
        ]
        (Decode.field "username" Username.decoder
            |> Decode.andThen (pictureRow seedAsText)
        )


feedTags : Int -> Neon.Query (List Api.TagCount)
feedTags limit =
    Neon.select "SELECT tag, pictures FROM feed_tags($1)"
        [ Neon.int limit ]
        (Decode.map2 (\tag pictures -> Maybe.map2 Api.TagCount tag (String.toInt pictures))
            (Decode.field "tag" (Decode.map Tag.fromString Decode.string))
            (Decode.field "pictures" Decode.string)
        )
        |> Neon.map (List.filterMap identity)


profile : Username -> Maybe PictureId -> Int -> Neon.Query (Maybe ProfileRow)
profile name before limit =
    Neon.selectOne "SELECT username, pictures FROM profile($1, $2::uuid, $3)"
        [ Neon.text (Username.toString name)
        , Neon.nullable (PictureId.toString >> Neon.text) before
        , Neon.int limit
        ]
        (Decode.field "username" Username.decoder
            |> Decode.andThen
                (\username ->
                    Decode.map (ProfileRow username)
                        (Decode.field "pictures" (Decode.list (pictureRow Decode.int username)))
                )
        )


seedAsText : Decoder Int
seedAsText =
    Decode.string
        |> Decode.andThen
            (\text ->
                case String.toInt text of
                    Just seed ->
                        Decode.succeed seed

                    Nothing ->
                        Decode.fail "Expected a seed."
            )


pictureRow : Decoder Int -> Username -> Decoder PictureRow
pictureRow seed username =
    Decode.succeed PictureRow
        |> andMap (Decode.field "id" PictureId.decoder)
        |> andMap (Decode.succeed username)
        |> andMap (Decode.field "prompt" Decode.string)
        |> andMap (Decode.field "width" Decode.int)
        |> andMap (Decode.field "height" Decode.int)
        |> andMap (Decode.field "steps" Decode.int)
        |> andMap (Decode.field "seed" seed)
        |> andMap (Decode.field "model" Decode.string)
        |> andMap (Decode.field "tags" (Decode.map (List.filterMap Tag.fromString) (Decode.list Decode.string)))
        |> andMap (Decode.field "published_at" Decode.string)


andMap : Decoder a -> Decoder (a -> b) -> Decoder b
andMap =
    Decode.map2 (|>)
