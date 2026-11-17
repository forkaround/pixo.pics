module Services.OAuth exposing (Client, Config, Problem(..), Tokens, authorizeUrl, clientFor, config, exchange, refresh, revoke)

import Api exposing (SyncProvider(..))
import Effect exposing (Effect)
import Http
import Json.Decode as Decode exposing (Decoder)
import Url.Builder


type alias Client =
    { id : String
    , secret : String
    }


type alias Config =
    { key : String
    , dropbox : Maybe Client
    , googleDrive : Maybe Client
    }


type alias Tokens =
    { accessToken : String
    , expiresIn : Int
    , refreshToken : Maybe String
    }


type Problem
    = Revoked
    | Failed String


config :
    { a
        | dropboxClientId : Maybe String
        , dropboxClientSecret : Maybe String
        , googleClientId : Maybe String
        , googleClientSecret : Maybe String
        , syncTokenKey : Maybe String
    }
    -> Result String (Maybe Config)
config flags =
    let
        variables =
            [ flags.dropboxClientId, flags.dropboxClientSecret, flags.googleClientId, flags.googleClientSecret, flags.syncTokenKey ]

        pair id secret =
            case ( id, secret ) of
                ( Just i, Just s ) ->
                    Ok (Just { id = i, secret = s })

                ( Nothing, Nothing ) ->
                    Ok Nothing

                _ ->
                    Err "DROPBOX_CLIENT_ID and DROPBOX_CLIENT_SECRET, and GOOGLE_CLIENT_ID and GOOGLE_CLIENT_SECRET, must each be set together or not at all."
    in
    if List.all ((==) Nothing) variables then
        Ok Nothing

    else
        case ( flags.syncTokenKey, pair flags.dropboxClientId flags.dropboxClientSecret, pair flags.googleClientId flags.googleClientSecret ) of
            ( _, Err problem, _ ) ->
                Err problem

            ( _, _, Err problem ) ->
                Err problem

            ( Nothing, _, _ ) ->
                Err "SYNC_TOKEN_KEY must be set, with at least 32 characters, when a Dropbox or Google client is."

            ( Just key, Ok dropbox, Ok googleDrive ) ->
                if String.length key < 32 then
                    Err "SYNC_TOKEN_KEY must have at least 32 characters."

                else if dropbox == Nothing && googleDrive == Nothing then
                    Err "SYNC_TOKEN_KEY is set, but neither DROPBOX_CLIENT_ID and DROPBOX_CLIENT_SECRET nor GOOGLE_CLIENT_ID and GOOGLE_CLIENT_SECRET are."

                else
                    Ok (Just { key = key, dropbox = dropbox, googleDrive = googleDrive })


clientFor : SyncProvider -> Config -> Maybe Client
clientFor provider settings =
    case provider of
        Dropbox ->
            settings.dropbox

        GoogleDrive ->
            settings.googleDrive


authorizeUrl : SyncProvider -> Client -> { redirectUri : String, state : String, challenge : String } -> String
authorizeUrl provider client wanted =
    let
        common =
            [ Url.Builder.string "client_id" client.id
            , Url.Builder.string "response_type" "code"
            , Url.Builder.string "redirect_uri" wanted.redirectUri
            , Url.Builder.string "state" wanted.state
            , Url.Builder.string "code_challenge" wanted.challenge
            , Url.Builder.string "code_challenge_method" "S256"
            ]
    in
    case provider of
        Dropbox ->
            Url.Builder.crossOrigin "https://www.dropbox.com"
                [ "oauth2", "authorize" ]
                (common ++ [ Url.Builder.string "token_access_type" "offline" ])

        GoogleDrive ->
            Url.Builder.crossOrigin "https://accounts.google.com"
                [ "o", "oauth2", "v2", "auth" ]
                (common
                    ++ [ Url.Builder.string "scope" "https://www.googleapis.com/auth/drive.file"
                       , Url.Builder.string "access_type" "offline"
                       , Url.Builder.string "prompt" "consent"
                       , Url.Builder.string "include_granted_scopes" "true"
                       ]
                )


tokenUrl : SyncProvider -> String
tokenUrl provider =
    case provider of
        Dropbox ->
            "https://api.dropboxapi.com/oauth2/token"

        GoogleDrive ->
            "https://oauth2.googleapis.com/token"


timeout : Float
timeout =
    10000


exchange : SyncProvider -> Client -> { code : String, verifier : String, redirectUri : String } -> Effect (Result Problem Tokens)
exchange provider client wanted =
    token provider
        [ ( "code", wanted.code )
        , ( "grant_type", "authorization_code" )
        , ( "redirect_uri", wanted.redirectUri )
        , ( "code_verifier", wanted.verifier )
        , ( "client_id", client.id )
        , ( "client_secret", client.secret )
        ]


refresh : SyncProvider -> Client -> String -> Effect (Result Problem Tokens)
refresh provider client refreshToken =
    token provider
        [ ( "grant_type", "refresh_token" )
        , ( "refresh_token", refreshToken )
        , ( "client_id", client.id )
        , ( "client_secret", client.secret )
        ]


token : SyncProvider -> List ( String, String ) -> Effect (Result Problem Tokens)
token provider fields =
    Effect.Http
        { method = "POST"
        , headers = [ ( "accept", "application/json" ) ]
        , url = tokenUrl provider
        , body = Effect.FormBody fields
        , timeout = Just timeout
        , onResponse = tokensOutcome provider
        }


revoke : SyncProvider -> Client -> { refreshToken : String, accessToken : Maybe String } -> Effect (Result Problem ())
revoke provider _ wanted =
    case ( provider, wanted.accessToken ) of
        ( Dropbox, Just accessToken ) ->
            Effect.Http
                { method = "POST"
                , headers = [ ( "authorization", "Bearer " ++ accessToken ) ]
                , url = "https://api.dropboxapi.com/2/auth/token/revoke"
                , body = Effect.EmptyBody
                , timeout = Just timeout
                , onResponse = revokeOutcome provider
                }

        ( Dropbox, Nothing ) ->
            Effect.Succeed (Ok ())

        ( GoogleDrive, _ ) ->
            Effect.Http
                { method = "POST"
                , headers = []
                , url = "https://oauth2.googleapis.com/revoke"
                , body = Effect.FormBody [ ( "token", wanted.refreshToken ) ]
                , timeout = Just timeout
                , onResponse = revokeOutcome provider
                }


tokensOutcome : SyncProvider -> Http.Response String -> Result Problem Tokens
tokensOutcome provider response =
    case response of
        Http.GoodStatus_ _ body ->
            Decode.decodeString tokensDecoder body
                |> Result.mapError (\_ -> Failed (Api.providerName provider ++ "'s answer was not what we expected."))

        _ ->
            Err (failure provider response)


revokeOutcome : SyncProvider -> Http.Response String -> Result Problem ()
revokeOutcome provider response =
    case response of
        Http.GoodStatus_ _ _ ->
            Ok ()

        _ ->
            Err (failure provider response)


tokensDecoder : Decoder Tokens
tokensDecoder =
    Decode.map3 Tokens
        (Decode.field "access_token" Decode.string)
        (Decode.field "expires_in" Decode.int)
        (Decode.maybe (Decode.field "refresh_token" Decode.string))


failure : SyncProvider -> Http.Response String -> Problem
failure provider response =
    let
        name =
            Api.providerName provider
    in
    case response of
        Http.BadStatus_ metadata body ->
            case Decode.decodeString (Decode.field "error" Decode.string) body of
                Ok "invalid_grant" ->
                    if metadata.statusCode == 400 then
                        Revoked

                    else
                        Failed (name ++ " answered " ++ String.fromInt metadata.statusCode ++ " (invalid_grant).")

                Ok code ->
                    Failed (name ++ " answered " ++ String.fromInt metadata.statusCode ++ " (" ++ String.left 100 code ++ ").")

                Err _ ->
                    Failed (name ++ " answered " ++ String.fromInt metadata.statusCode ++ ".")

        Http.GoodStatus_ _ _ ->
            Failed (name ++ " answered something unexpected.")

        Http.NetworkError_ ->
            Failed ("Could not reach " ++ name ++ ".")

        Http.Timeout_ ->
            Failed (name ++ " took too long to answer.")

        Http.BadUrl_ _ ->
            Failed ("Bad " ++ name ++ " endpoint.")
