module Flow.MagicLink exposing (Msg, askSignIn, askSignUp, update, verification, verify)

import Api
import Data.Email as Email exposing (Email)
import Data.LinkToken exposing (LinkToken)
import Data.Origin as Origin
import Data.SignUp as SignUp
import Db
import Effect exposing (Effect(..), log, query, respond, transact)
import InteropDefinitions as Interop
import Json.Decode as Decode
import Path
import RateLimit
import Reply exposing (Reply(..))
import Request exposing (Request)
import Response exposing (Response)
import Server exposing (Server)
import Services.Mailer as Mailer
import Services.Mailer.Layout as Layout
import Services.Mailer.SignIn
import Services.Mailer.SignUp
import Services.Neon as Neon
import SessionCookie


type Msg
    = LinkLimited Interop.Resolver Ask ( RateLimit.Outcome, RateLimit.Outcome )
    | SignUpLinkCreated Interop.Resolver SignUp.SignUp Db.SignUpLink
    | SignInLinkCreated Interop.Resolver Email Db.SignInLink
    | LinkSent Interop.Resolver Mailer.Outcome
    | VerificationFound Interop.Resolver (Maybe Api.Verification)
    | VerificationConsumed Interop.Resolver Db.Consumed


type Ask
    = SignUpAsked SignUp.SignUp
    | SignInAsked Email


askedBy : Ask -> Email
askedBy ask =
    case ask of
        SignUpAsked signUp ->
            signUp.email

        SignInAsked email ->
            email


askSignUp : Server -> Request -> Reply Msg
askSignUp server request =
    askForLink server
        request
        (Decode.decodeString (Api.bodyDecoder Api.signUp) request.body
            |> Result.mapError (\_ -> Response.error 400 "Expected a JSON object with an email and a username.")
            |> Result.andThen (SignUp.parse >> Result.mapError (Response.error 422))
            |> Result.map SignUpAsked
        )


askSignIn : Server -> Request -> Reply Msg
askSignIn server request =
    askForLink server
        request
        (Decode.decodeString (Api.bodyDecoder Api.signIn) request.body
            |> Result.mapError (\_ -> Response.error 400 "Expected a JSON object with an email.")
            |> Result.andThen (.email >> Email.parse >> Result.mapError (Response.error 422))
            |> Result.map SignInAsked
        )


verification : Server -> Request -> Reply Msg
verification server request =
    withLink
        request
        (\token resolver ->
            query server.db resolver "Verification failed at its link" (Db.magicLink token) (VerificationFound resolver)
        )


verify : Server -> Request -> Reply Msg
verify server request =
    withLink
        request
        (\token resolver ->
            query server.db
                resolver
                "Verification failed at its session"
                (Db.consumeMagicLink { token = token, sessionSeconds = SessionCookie.lifetimeSeconds })
                (VerificationConsumed resolver)
        )


update : Server -> Msg -> Effect Msg
update server msg =
    case msg of
        LinkLimited resolver ask ( byAddress, byEmail ) ->
            case RateLimit.retryAfter [ byAddress, byEmail ] of
                Just seconds ->
                    respond resolver (Response.tooManyRequests seconds)

                Nothing ->
                    case ask of
                        SignUpAsked signUp ->
                            query server.db
                                resolver
                                "Magic link failed at its link"
                                (Db.createSignUpLink
                                    { email = signUp.email
                                    , username = signUp.username
                                    , ttlSeconds = linkMinutes * 60
                                    }
                                )
                                (SignUpLinkCreated resolver signUp)

                        SignInAsked email ->
                            query server.db
                                resolver
                                "Magic link failed at its link"
                                (Db.createSignInLink { email = email, ttlSeconds = linkMinutes * 60 })
                                (SignInLinkCreated resolver email)

        SignUpLinkCreated resolver signUp link ->
            case link of
                Db.UsernameTaken ->
                    respond resolver (Response.error 409 "That username is taken.")

                Db.SignUpLink token ->
                    sendLink server
                        resolver
                        { to = signUp.email, tag = "sign-up" }
                        (Services.Mailer.SignUp.email
                            { url = Origin.href server.origin (Path.Verify (Just token))
                            , username = signUp.username
                            , expiresInMinutes = linkMinutes
                            }
                        )

                Db.SignInInstead token ->
                    sendLink server resolver { to = signUp.email, tag = "sign-in" } (signInEmail server token)

        SignInLinkCreated resolver email link ->
            case link of
                Db.NoAccount ->
                    respond resolver Response.accepted

                Db.SignInLink token ->
                    sendLink server resolver { to = email, tag = "sign-in" } (signInEmail server token)

        LinkSent resolver (Ok _) ->
            respond resolver Response.accepted

        LinkSent resolver (Err problem) ->
            Batch
                [ respond resolver (Response.error 502 "The email could not be sent. Try again.")
                , log Interop.Error ("Magic link failed at its email: " ++ problem)
                ]

        VerificationFound resolver (Just found) ->
            respond resolver (Response.json 200 (Api.encodeResponse Api.verification found))

        VerificationFound resolver Nothing ->
            respond resolver Response.invalidLink

        VerificationConsumed resolver (Db.Verified { sessionToken, account }) ->
            respond resolver
                (Response.json 200 (Api.encodeResponse Api.verify account)
                    |> SessionCookie.set server.origin sessionToken
                )

        VerificationConsumed resolver Db.InvalidLink ->
            respond resolver Response.invalidLink

        VerificationConsumed resolver Db.UsernameTakenMeanwhile ->
            respond resolver (Response.error 409 "That username was taken in the meantime. Sign up again with another.")


sendLink : Server -> Interop.Resolver -> { to : Email, tag : String } -> Layout.Message -> Effect Msg
sendLink server resolver envelope message =
    Mailer.send server.mailer envelope message (LinkSent resolver)


signInEmail : Server -> LinkToken -> Layout.Message
signInEmail server token =
    Services.Mailer.SignIn.email
        { url = Origin.href server.origin (Path.Verify (Just token))
        , expiresInMinutes = linkMinutes
        }


askForLink : Server -> Request -> Result Response Ask -> Reply Msg
askForLink server request parsed =
    case parsed of
        Ok ask ->
            Later
                (\resolver ->
                    transact server.db
                        resolver
                        "Magic link failed at its rate limits"
                        (Neon.transaction Tuple.pair
                            |> Neon.with
                                (Db.rateLimit (RateLimit.magicLinksByAddress (Request.clientAddress request)))
                            |> Neon.with
                                (Db.rateLimit (RateLimit.magicLinksByEmail (askedBy ask)))
                        )
                        (LinkLimited resolver ask)
                )

        Err response ->
            Now response


withLink : Request -> (LinkToken -> Interop.Resolver -> Effect Msg) -> Reply Msg
withLink request toEffect =
    case Request.linkToken request of
        Just token ->
            Later (toEffect token)

        Nothing ->
            Now
                (Response.error 401 "Send the link's token as a bearer token."
                    |> Response.withHeader "www-authenticate" "Bearer"
                )


linkMinutes : Int
linkMinutes =
    15
