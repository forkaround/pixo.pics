module Flow.Passkeys exposing (Msg, add, list, options, remove, update)

import Aaguid
import Api
import Data.CredentialId exposing (CredentialId)
import Data.Email as Email exposing (Email)
import Data.Origin as Origin
import Data.UserId as UserId
import Data.Username as Username
import Db
import Effect exposing (Effect(..), log, respond, transact)
import InteropDefinitions as Interop
import Json.Decode as Decode
import Path
import Reply exposing (Reply(..), withSession)
import Request exposing (Request)
import Response exposing (Response)
import Server exposing (Server)
import Services.Mailer as Mailer
import Services.Mailer.Layout as Layout
import Services.Mailer.PasskeyAdded
import Services.Mailer.PasskeyRemoved
import WebAuthn


type Msg
    = Listed Interop.Resolver (Maybe (List Api.Passkey))
    | Started Interop.Resolver (Maybe Db.Ceremony)
    | Added Interop.Resolver (Maybe Db.Added)
    | Removed Interop.Resolver CredentialId (Maybe Db.Removed)
    | Notified Interop.Resolver Response Mailer.Outcome


list : Server -> Request -> Reply Msg
list server request =
    withSession
        request
        (\token resolver ->
            transact server.db resolver "Passkey list failed" (Db.passkeys token) (Listed resolver)
        )


options : Server -> Request -> Reply Msg
options server request =
    withSession
        request
        (\token resolver ->
            transact server.db
                resolver
                "Passkey failed at its challenge"
                (Db.startPasskey { token = token, ttlSeconds = WebAuthn.challengeSeconds })
                (Started resolver)
        )


add : Server -> Request -> Reply Msg
add server request =
    withSession
        request
        (\token resolver ->
            case
                Decode.decodeString (Api.bodyDecoder Api.addPasskey) request.body
                    |> Result.mapError (\_ -> Nothing)
                    |> Result.andThen
                        (WebAuthn.register server.origin
                            >> Result.mapError Just
                        )
            of
                Ok registration ->
                    transact server.db
                        resolver
                        "Passkey failed at its credential"
                        (Db.addPasskey
                            { token = token
                            , registration = registration
                            , name = Aaguid.name registration.aaguid
                            }
                        )
                        (Added resolver)

                Err Nothing ->
                    respond resolver
                        (Response.error 400 "Expected a JSON object with a clientDataJSON, an attestationObject and transports.")

                Err (Just problem) ->
                    Batch
                        [ respond resolver (Response.error 422 "This passkey could not be verified. Try again.")
                        , log Interop.Warn ("Passkey refused: " ++ String.left 200 problem)
                        ]
        )


remove : CredentialId -> Server -> Request -> Reply Msg
remove id server request =
    withSession
        request
        (\token resolver ->
            transact server.db
                resolver
                "Passkey removal failed"
                (Db.removePasskey { token = token, id = id })
                (Removed resolver id)
        )


update : Server -> Msg -> Effect Msg
update server msg =
    case msg of
        Listed resolver (Just found) ->
            respond resolver (Response.json 200 (Api.encodeResponse Api.passkeys found))

        Listed resolver Nothing ->
            respond resolver Response.notSignedIn

        Started resolver (Just ceremony) ->
            respond resolver
                (Response.json 200
                    (Api.encodeResponse Api.passkeyOptions
                        { challenge = ceremony.challenge
                        , rp = { id = Origin.rpId server.origin, name = "pixo" }
                        , user =
                            { id = UserId.toString ceremony.userId
                            , name = Email.toString ceremony.account.email
                            , displayName = Username.toString ceremony.account.username
                            }
                        , algorithms = WebAuthn.algorithms
                        , excludeCredentials = ceremony.passkeys
                        }
                    )
                )

        Started resolver Nothing ->
            respond resolver Response.notSignedIn

        Added resolver (Just (Db.Added passkey account)) ->
            notify server
                resolver
                (Response.json 201 (Api.encodeResponse Api.addPasskey passkey))
                { to = account.email, tag = "passkey-added" }
                (Services.Mailer.PasskeyAdded.email
                    { url = Origin.href server.origin Path.Dashboard
                    , username = account.username
                    , passkey = passkey.name
                    }
                )

        Added resolver (Just Db.NotAdded) ->
            respond resolver (Response.error 409 "This passkey request expired or was used already. Try again.")

        Added resolver Nothing ->
            respond resolver Response.notSignedIn

        Removed resolver id (Just (Db.Removed { name, account })) ->
            notify server
                resolver
                (Response.json 200 (Api.encodeResponse (Api.deletePasskey id) ()))
                { to = account.email, tag = "passkey-removed" }
                (Services.Mailer.PasskeyRemoved.email
                    { url = Origin.href server.origin Path.Dashboard
                    , username = account.username
                    , passkey = name
                    }
                )

        Removed resolver _ (Just Db.NotRemoved) ->
            respond resolver (Response.error 404 "This account has no such passkey.")

        Removed resolver _ Nothing ->
            respond resolver Response.notSignedIn

        Notified resolver response (Ok _) ->
            respond resolver response

        Notified resolver response (Err problem) ->
            Batch
                [ respond resolver response
                , log Interop.Error ("Passkey notice failed at its email: " ++ problem)
                ]


notify : Server -> Interop.Resolver -> Response -> { to : Email, tag : String } -> Layout.Message -> Effect Msg
notify server resolver response envelope email =
    Mailer.send server.mailer envelope email (Notified resolver response)
