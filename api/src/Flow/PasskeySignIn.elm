module Flow.PasskeySignIn exposing (Msg, authenticate, options, signatureChecked, update)

import Api
import Data.CredentialId as CredentialId
import Data.Origin as Origin
import Db
import Effect exposing (Effect(..), log, query, respond)
import InteropDefinitions as Interop
import Json.Decode as Decode
import RateLimit
import Reply exposing (Reply(..))
import Request exposing (Request)
import Response
import Server exposing (Server)
import SessionCookie
import WebAuthn


type Msg
    = Started Interop.Resolver Db.SignInChallenge
    | KeyFound Interop.Resolver { signed : String, signature : String, signIn : Interop.SignIn } (Maybe String)
    | SignedIn Interop.Resolver Db.PasskeySignIn


options : Server -> Request -> Reply Msg
options server request =
    Later
        (\resolver ->
            query server.db
                resolver
                "Passkey sign-in failed at its challenge"
                (Db.signInChallenge
                    { policy = RateLimit.passkeyChallenges (Request.clientAddress request)
                    , ttlSeconds = WebAuthn.challengeSeconds
                    }
                )
                (Started resolver)
        )


authenticate : Server -> Request -> Reply Msg
authenticate server request =
    case Decode.decodeString (Api.bodyDecoder Api.passkeySignIn) request.body of
        Ok assertion ->
            Later
                (\resolver ->
                    case ( WebAuthn.authenticate server.origin assertion, CredentialId.fromString assertion.id ) of
                        ( Ok authentication, Just credentialId ) ->
                            query server.db
                                resolver
                                "Passkey sign-in failed at its key"
                                (Db.passkeyPublicKey credentialId)
                                (KeyFound resolver
                                    { signed = authentication.signed
                                    , signature = assertion.signature
                                    , signIn =
                                        { challenge = authentication.challenge
                                        , credentialId = credentialId
                                        , signCount = authentication.signCount
                                        , backedUp = authentication.backedUp
                                        }
                                    }
                                )

                        ( Err problem, _ ) ->
                            refuse resolver problem

                        ( Ok _, Nothing ) ->
                            refuse resolver "The assertion names no credential."
                )

        Err _ ->
            Now (Response.error 400 "Expected a JSON object with an id, a clientDataJSON, an authenticatorData and a signature.")


signatureChecked : Server -> Interop.Resolver -> Interop.SignIn -> Bool -> Effect Msg
signatureChecked server resolver signIn valid =
    if valid then
        query server.db
            resolver
            "Passkey sign-in failed at its session"
            (Db.signInWithPasskey
                { challenge = signIn.challenge
                , credentialId = signIn.credentialId
                , signCount = signIn.signCount
                , backedUp = signIn.backedUp
                , sessionSeconds = SessionCookie.lifetimeSeconds
                }
            )
            (SignedIn resolver)

    else
        refuse resolver "The signature does not match the passkey."


update : Server -> Msg -> Effect Msg
update server msg =
    case msg of
        Started resolver (Db.SignInChallenge challenge) ->
            respond resolver
                (Response.json 200 (Api.encodeResponse Api.passkeySignInOptions { challenge = challenge, rpId = Origin.rpId server.origin }))

        Started resolver (Db.TooManyChallenges limited) ->
            respond resolver (Response.tooManyRequests limited.retryAfter)

        KeyFound resolver assertion (Just publicKey) ->
            case
                WebAuthn.key publicKey
                    |> Result.andThen
                        (\key ->
                            WebAuthn.signature key assertion.signature
                                |> Result.map (Tuple.pair key)
                        )
            of
                Ok ( key, signature ) ->
                    SendToJs
                        (Interop.VerifySignature resolver
                            { key = key
                            , signed = assertion.signed
                            , signature = signature
                            , signIn = assertion.signIn
                            }
                        )

                Err problem ->
                    refuse resolver problem

        KeyFound resolver _ Nothing ->
            refuse resolver "No passkey has this credential id."

        SignedIn resolver (Db.PasskeySignedIn { sessionToken, account }) ->
            respond resolver
                (Response.json 200 (Api.encodeResponse Api.passkeySignIn account)
                    |> SessionCookie.set server.origin sessionToken
                )

        SignedIn resolver Db.ChallengeSpent ->
            refuse resolver "The challenge is expired, used or unknown."

        SignedIn resolver Db.PasskeyStale ->
            refuse resolver "The passkey is gone, or its counter did not move forward."


refuse : Interop.Resolver -> String -> Effect Msg
refuse resolver problem =
    Batch
        [ respond resolver (Response.error 401 "This passkey could not be verified. Try again, or sign in with an emailed link.")
        , log Interop.Warn ("Passkey sign-in refused: " ++ String.left 200 problem)
        ]
