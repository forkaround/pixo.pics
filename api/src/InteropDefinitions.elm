module InteropDefinitions exposing (Flags, FromElm(..), Level(..), Resolver, SignIn, ToElm(..), interop)

import Data.Challenge as Challenge exposing (Challenge)
import Data.CredentialId as CredentialId exposing (CredentialId)
import Json.Encode as Encode
import Request exposing (Request)
import Response exposing (Response)
import TsJson.Decode as TsDecode exposing (Decoder)
import TsJson.Encode as TsEncode exposing (Encoder)
import WebAuthn


interop :
    { toElm : Decoder ToElm
    , fromElm : Encoder FromElm
    , flags : Decoder Flags
    }
interop =
    { toElm = toElm
    , fromElm = fromElm
    , flags = flags
    }


type ToElm
    = GotRequest Resolver Request
    | SignatureChecked Resolver SignIn Bool


type FromElm
    = Respond Resolver Response
    | Log Level String
    | VerifySignature Resolver { key : WebAuthn.Key, signed : String, signature : String, signIn : SignIn }


type alias SignIn =
    { challenge : Challenge
    , credentialId : CredentialId
    , signCount : Int
    , backedUp : Bool
    }


type Level
    = Info
    | Warn
    | Error


type alias Flags =
    { databaseUrl : Maybe String
    , neonEndpoint : Maybe String
    , brevoApiKey : Maybe String
    , emailFrom : Maybe String
    , origin : String
    , releaseId : Maybe String
    , releaseUrl : Maybe String
    , region : Maybe String
    }


type Resolver
    = Resolver Encode.Value



-- TO ELM


toElm : Decoder ToElm
toElm =
    TsDecode.discriminatedUnion "tag"
        [ ( "Request"
          , TsDecode.map2 GotRequest
                (TsDecode.field "resolve" resolverDecoder)
                (TsDecode.field "request" requestDecoder)
          )
        , ( "SignatureChecked"
          , TsDecode.map3 SignatureChecked
                (TsDecode.field "resolve" resolverDecoder)
                (TsDecode.field "signIn"
                    (TsDecode.map4 SignIn
                        (TsDecode.field "challenge" challengeDecoder)
                        (TsDecode.field "credentialId" credentialIdDecoder)
                        (TsDecode.field "signCount" TsDecode.int)
                        (TsDecode.field "backedUp" TsDecode.bool)
                    )
                )
                (TsDecode.field "valid" TsDecode.bool)
          )
        ]


challengeDecoder : Decoder Challenge
challengeDecoder =
    TsDecode.andThen
        (TsDecode.andThenInit
            (\value ->
                case Challenge.fromString value of
                    Just challenge ->
                        TsDecode.succeed challenge

                    Nothing ->
                        TsDecode.fail "Expected a challenge, not an empty string."
            )
        )
        TsDecode.string


credentialIdDecoder : Decoder CredentialId
credentialIdDecoder =
    TsDecode.andThen
        (TsDecode.andThenInit
            (\value ->
                case CredentialId.fromString value of
                    Just id ->
                        TsDecode.succeed id

                    Nothing ->
                        TsDecode.fail "Expected a credential id, not an empty string."
            )
        )
        TsDecode.string


resolverDecoder : Decoder Resolver
resolverDecoder =
    TsDecode.map Resolver TsDecode.value


requestDecoder : Decoder Request
requestDecoder =
    TsDecode.map5 Request
        (TsDecode.field "method" TsDecode.string)
        (TsDecode.field "path" (TsDecode.list TsDecode.string))
        (TsDecode.field "query" (TsDecode.dict TsDecode.string))
        (TsDecode.field "headers" (TsDecode.dict TsDecode.string))
        (TsDecode.field "body" TsDecode.string)



-- FROM ELM


fromElm : Encoder FromElm
fromElm =
    TsEncode.union
        (\respond log verifySignature value ->
            case value of
                Respond resolver response ->
                    respond ( resolver, response )

                Log level message ->
                    log ( level, message )

                VerifySignature resolver check ->
                    verifySignature ( resolver, check )
        )
        |> TsEncode.variantObject "Respond"
            [ TsEncode.required "resolve" Tuple.first resolverEncoder
            , TsEncode.required "status" (Tuple.second >> .status) TsEncode.int
            , TsEncode.required "headers"
                (Tuple.second >> .headers)
                (TsEncode.list (TsEncode.tuple TsEncode.string TsEncode.string))
            , TsEncode.required "body" (Tuple.second >> .body) TsEncode.string
            ]
        |> TsEncode.variantObject "Log"
            [ TsEncode.required "level" Tuple.first levelEncoder
            , TsEncode.required "message" Tuple.second TsEncode.string
            ]
        |> TsEncode.variantObject "VerifySignature"
            [ TsEncode.required "resolve" Tuple.first resolverEncoder
            , TsEncode.required "key" (Tuple.second >> .key) keyEncoder
            , TsEncode.required "signed" (Tuple.second >> .signed) TsEncode.string
            , TsEncode.required "signature" (Tuple.second >> .signature) TsEncode.string
            , TsEncode.required "signIn"
                (Tuple.second >> .signIn)
                (TsEncode.object
                    [ TsEncode.required "challenge" (.challenge >> Challenge.toString) TsEncode.string
                    , TsEncode.required "credentialId" (.credentialId >> CredentialId.toString) TsEncode.string
                    , TsEncode.required "signCount" .signCount TsEncode.int
                    , TsEncode.required "backedUp" .backedUp TsEncode.bool
                    ]
                )
            ]
        |> TsEncode.buildUnion


keyEncoder : Encoder WebAuthn.Key
keyEncoder =
    TsEncode.union
        (\es256 edDsa rs256 value ->
            case value of
                WebAuthn.Es256 point ->
                    es256 point

                WebAuthn.EdDsa point ->
                    edDsa point

                WebAuthn.Rs256 modulus ->
                    rs256 modulus
        )
        |> TsEncode.variantObject "ES256"
            [ TsEncode.required "x" .x TsEncode.string
            , TsEncode.required "y" .y TsEncode.string
            ]
        |> TsEncode.variantObject "EdDSA"
            [ TsEncode.required "x" .x TsEncode.string ]
        |> TsEncode.variantObject "RS256"
            [ TsEncode.required "n" .n TsEncode.string
            , TsEncode.required "e" .e TsEncode.string
            ]
        |> TsEncode.buildUnion


levelEncoder : Encoder Level
levelEncoder =
    TsEncode.union
        (\info warn error value ->
            case value of
                Info ->
                    info

                Warn ->
                    warn

                Error ->
                    error
        )
        |> TsEncode.variantLiteral (Encode.string "info")
        |> TsEncode.variantLiteral (Encode.string "warn")
        |> TsEncode.variantLiteral (Encode.string "error")
        |> TsEncode.buildUnion


resolverEncoder : Encoder Resolver
resolverEncoder =
    TsEncode.map (\(Resolver fn) -> fn) TsEncode.value



-- FLAGS


flags : Decoder Flags
flags =
    TsDecode.map8 Flags
        (TsDecode.field "databaseUrl" (TsDecode.nullable TsDecode.string))
        (TsDecode.field "neonEndpoint" (TsDecode.nullable TsDecode.string))
        (TsDecode.field "brevoApiKey" (TsDecode.nullable TsDecode.string))
        (TsDecode.field "emailFrom" (TsDecode.nullable TsDecode.string))
        (TsDecode.field "origin" TsDecode.string)
        (TsDecode.field "releaseId" (TsDecode.nullable TsDecode.string))
        (TsDecode.field "releaseUrl" (TsDecode.nullable TsDecode.string))
        (TsDecode.field "region" (TsDecode.nullable TsDecode.string))
