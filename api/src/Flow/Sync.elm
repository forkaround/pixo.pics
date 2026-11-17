module Flow.Sync exposing (Msg, finish, start, stop, token, update)

import Api exposing (SyncProvider)
import Data.Origin as Origin
import Data.SessionToken exposing (SessionToken)
import Db
import Effect exposing (Effect(..), log, respond, transact)
import InteropDefinitions as Interop
import Json.Decode as Decode
import Path
import Pkce
import RateLimit
import Reply exposing (Reply(..), withSession)
import Request exposing (Request)
import Response
import Server exposing (Server)
import Services.OAuth as OAuth


type Msg
    = Started Interop.Resolver SyncProvider (Maybe (Maybe { state : String, verifier : String }))
    | Finishing Interop.Resolver SessionToken String (Maybe (Maybe { provider : SyncProvider, verifier : String }))
    | Exchanged Interop.Resolver SessionToken SyncProvider (Result OAuth.Problem OAuth.Tokens)
    | Saved Interop.Resolver SyncProvider (Maybe String)
    | Found Interop.Resolver SessionToken SyncProvider (Maybe (Maybe { pro : Bool, refreshToken : String }))
    | Refreshed Interop.Resolver SessionToken SyncProvider (Result OAuth.Problem OAuth.Tokens)
    | Rotated Interop.Resolver SyncProvider OAuth.Tokens (Maybe String)
    | Gone Interop.Resolver SyncProvider (Maybe (Maybe String))
    | Dropped Interop.Resolver SyncProvider (Maybe (Maybe String))
    | RefreshedToRevoke Interop.Resolver SyncProvider String (Result OAuth.Problem OAuth.Tokens)
    | Revoked Interop.Resolver SyncProvider (Result OAuth.Problem ())


start : SyncProvider -> Server -> Request -> Reply Msg
start provider server request =
    configured server
        provider
        (\_ _ ->
            Limit [ RateLimit.syncStarts (Request.clientAddress request) ]
                (withSession request
                    (\sessionToken resolver ->
                        transact server.db resolver "Sync start failed" (Db.startSync sessionToken provider) (Started resolver provider)
                    )
                )
        )


finish : Server -> Request -> Reply Msg
finish server request =
    case server.sync of
        Nothing ->
            Now Response.unavailable

        Just _ ->
            case Decode.decodeString (Api.bodyDecoder Api.syncFinish) request.body of
                Err _ ->
                    Now (Response.error 400 "Expected a JSON object with a state and a code.")

                Ok body ->
                    Limit [ RateLimit.syncStarts (Request.clientAddress request) ]
                        (withSession request
                            (\sessionToken resolver ->
                                transact server.db resolver "Sync finish failed" (Db.finishSync sessionToken body.state) (Finishing resolver sessionToken body.code)
                            )
                        )


token : SyncProvider -> Server -> Request -> Reply Msg
token provider server request =
    configured server
        provider
        (\config _ ->
            Limit [ RateLimit.syncTokens (Request.clientAddress request) ]
                (withSession request
                    (\sessionToken resolver ->
                        transact server.db resolver "Sync token lookup failed" (Db.syncRefreshToken sessionToken provider config.key) (Found resolver sessionToken provider)
                    )
                )
        )


stop : SyncProvider -> Server -> Request -> Reply Msg
stop provider server request =
    configured server
        provider
        (\config _ ->
            withSession request
                (\sessionToken resolver ->
                    transact server.db resolver "Sync disconnect failed" (Db.dropSync sessionToken provider config.key) (Dropped resolver provider)
                )
        )


configured : Server -> SyncProvider -> (OAuth.Config -> OAuth.Client -> Reply Msg) -> Reply Msg
configured server provider toReply =
    case server.sync of
        Nothing ->
            Now Response.unavailable

        Just config ->
            case OAuth.clientFor provider config of
                Nothing ->
                    Now notSetUp

                Just client ->
                    toReply config client


notSetUp : Response.Response
notSetUp =
    Response.error 404 "This provider is not set up."


redirectUri : Server -> String
redirectUri server =
    Origin.href server.origin (Path.Sync { code = Nothing, state = Nothing, error = Nothing })


update : Server -> Msg -> Effect Msg
update server msg =
    case server.sync of
        Nothing ->
            respond (resolverOf msg) Response.unavailable

        Just config ->
            updateConfigured server config msg


resolverOf : Msg -> Interop.Resolver
resolverOf msg =
    case msg of
        Started resolver _ _ ->
            resolver

        Finishing resolver _ _ _ ->
            resolver

        Exchanged resolver _ _ _ ->
            resolver

        Saved resolver _ _ ->
            resolver

        Found resolver _ _ _ ->
            resolver

        Refreshed resolver _ _ _ ->
            resolver

        Rotated resolver _ _ _ ->
            resolver

        Gone resolver _ _ ->
            resolver

        Dropped resolver _ _ ->
            resolver

        RefreshedToRevoke resolver _ _ _ ->
            resolver

        Revoked resolver _ _ ->
            resolver


updateConfigured : Server -> OAuth.Config -> Msg -> Effect Msg
updateConfigured server config msg =
    case msg of
        Started resolver _ Nothing ->
            respond resolver Response.notSignedIn

        Started resolver _ (Just Nothing) ->
            respond resolver (Response.error 403 "Sync needs Pro.")

        Started resolver provider (Just (Just attempt)) ->
            case OAuth.clientFor provider config of
                Nothing ->
                    respond resolver notSetUp

                Just client ->
                    respond resolver
                        (Response.json 200
                            (Api.encodeResponse (Api.syncStart provider)
                                { url =
                                    OAuth.authorizeUrl provider
                                        client
                                        { redirectUri = redirectUri server
                                        , state = attempt.state
                                        , challenge = Pkce.challenge attempt.verifier
                                        }
                                }
                            )
                        )

        Finishing resolver _ _ Nothing ->
            respond resolver Response.notSignedIn

        Finishing resolver _ _ (Just Nothing) ->
            respond resolver (Response.error 400 "This connection link expired. Try again.")

        Finishing resolver sessionToken code (Just (Just attempt)) ->
            case OAuth.clientFor attempt.provider config of
                Nothing ->
                    respond resolver notSetUp

                Just client ->
                    Effect.map (Exchanged resolver sessionToken attempt.provider)
                        (OAuth.exchange attempt.provider
                            client
                            { code = code
                            , verifier = attempt.verifier
                            , redirectUri = redirectUri server
                            }
                        )

        Exchanged resolver sessionToken provider (Ok tokens) ->
            case tokens.refreshToken of
                Just refreshToken ->
                    transact server.db
                        resolver
                        "Sync save failed"
                        (Db.saveSync sessionToken { provider = provider, refreshToken = refreshToken, key = config.key })
                        (Saved resolver provider)

                Nothing ->
                    Batch
                        [ respond resolver (Response.error 502 (Api.providerName provider ++ " gave no lasting access. Try again."))
                        , log Interop.Warn (Api.providerName provider ++ " answered the code exchange without lasting access.")
                        ]

        Exchanged resolver _ provider (Err problem) ->
            Batch
                [ respond resolver (Response.error 502 ("Couldn't connect to " ++ Api.providerName provider ++ "."))
                , log Interop.Error (Api.providerName provider ++ " code request failed: " ++ reasonOf problem)
                ]

        Saved resolver _ Nothing ->
            respond resolver Response.notSignedIn

        Saved resolver provider (Just connectedAt) ->
            respond resolver
                (Response.json 200
                    (Api.encodeResponse Api.syncFinish { provider = provider, connectedAt = connectedAt })
                )

        Found resolver _ _ Nothing ->
            respond resolver Response.notSignedIn

        Found resolver _ _ (Just Nothing) ->
            respond resolver (Response.error 404 "Not connected.")

        Found resolver sessionToken provider (Just (Just row)) ->
            if not row.pro then
                respond resolver (Response.error 403 "Sync needs Pro.")

            else
                case OAuth.clientFor provider config of
                    Nothing ->
                        respond resolver notSetUp

                    Just client ->
                        Effect.map (Refreshed resolver sessionToken provider) (OAuth.refresh provider client row.refreshToken)

        Refreshed resolver sessionToken provider (Ok tokens) ->
            case tokens.refreshToken of
                Just replacement ->
                    transact server.db
                        resolver
                        "Sync save failed"
                        (Db.saveSync sessionToken { provider = provider, refreshToken = replacement, key = config.key })
                        (Rotated resolver provider tokens)

                Nothing ->
                    respond resolver (accessTokenAnswer provider tokens)

        Refreshed resolver sessionToken provider (Err OAuth.Revoked) ->
            transact server.db
                resolver
                "Sync disconnect failed"
                (Db.dropSync sessionToken provider config.key)
                (Gone resolver provider)

        Refreshed resolver _ provider (Err (OAuth.Failed reason)) ->
            Batch
                [ respond resolver (Response.error 502 ("Couldn't reach " ++ Api.providerName provider ++ "."))
                , log Interop.Error (Api.providerName provider ++ " token request failed: " ++ reason)
                ]

        Rotated resolver provider tokens _ ->
            respond resolver (accessTokenAnswer provider tokens)

        Gone resolver provider _ ->
            respond resolver (Response.error 409 (Api.providerName provider ++ " access was removed. Connect again."))

        Dropped resolver _ Nothing ->
            respond resolver Response.notSignedIn

        Dropped resolver _ (Just Nothing) ->
            respond resolver stopped

        Dropped resolver provider (Just (Just refreshToken)) ->
            case OAuth.clientFor provider config of
                Nothing ->
                    respond resolver stopped

                Just client ->
                    case provider of
                        Api.Dropbox ->
                            Effect.map (RefreshedToRevoke resolver provider refreshToken) (OAuth.refresh provider client refreshToken)

                        Api.GoogleDrive ->
                            Effect.map (Revoked resolver provider)
                                (OAuth.revoke provider client { refreshToken = refreshToken, accessToken = Nothing })

        RefreshedToRevoke resolver provider refreshToken (Ok tokens) ->
            case OAuth.clientFor provider config of
                Nothing ->
                    respond resolver stopped

                Just client ->
                    Effect.map (Revoked resolver provider)
                        (OAuth.revoke provider client { refreshToken = refreshToken, accessToken = Just tokens.accessToken })

        RefreshedToRevoke resolver provider _ (Err problem) ->
            Batch
                [ respond resolver stopped
                , log Interop.Warn (Api.providerName provider ++ " could not be revoked: " ++ reasonOf problem)
                ]

        Revoked resolver _ (Ok ()) ->
            respond resolver stopped

        Revoked resolver provider (Err problem) ->
            Batch
                [ respond resolver stopped
                , log Interop.Warn (Api.providerName provider ++ " could not be revoked: " ++ reasonOf problem)
                ]


accessTokenAnswer : SyncProvider -> OAuth.Tokens -> Response.Response
accessTokenAnswer provider tokens =
    Response.json 200
        (Api.encodeResponse (Api.syncToken provider) { accessToken = tokens.accessToken, expiresIn = tokens.expiresIn })


stopped : Response.Response
stopped =
    Response.json 200 (Api.encodeResponse (Api.syncStop Api.Dropbox) ())


reasonOf : OAuth.Problem -> String
reasonOf problem =
    case problem of
        OAuth.Revoked ->
            "the grant was refused."

        OAuth.Failed reason ->
            reason
