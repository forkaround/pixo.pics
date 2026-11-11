module WebAuthn exposing (Authentication, Key(..), Registration, algorithms, authenticate, challengeSeconds, key, register, signature)

import Base64
import Bitwise
import Bytes exposing (Bytes, Endianness(..))
import Bytes.Decode as Binary
import Bytes.Encode
import Cbor exposing (CborItem(..))
import Cbor.Decode
import Cbor.Encode
import Data.Challenge as Challenge exposing (Challenge)
import Data.CredentialId as CredentialId exposing (CredentialId)
import Data.Origin as Origin exposing (Origin)
import Json.Decode as Decode exposing (Decoder)
import SHA256


type alias Registration =
    { challenge : Challenge
    , credentialId : CredentialId
    , publicKey : String
    , aaguid : String
    , signCount : Int
    , backupEligible : Bool
    , backedUp : Bool
    , transports : List String
    }


type alias Authentication =
    { challenge : Challenge
    , signCount : Int
    , backedUp : Bool
    , signed : String
    }


type Key
    = Es256 { x : String, y : String }
    | EdDsa { x : String }
    | Rs256 { n : String, e : String }


algorithms : List Int
algorithms =
    [ -8, -7, -257 ]


challengeSeconds : Int
challengeSeconds =
    300


knownTransports : List String
knownTransports =
    [ "ble", "hybrid", "internal", "nfc", "smart-card", "usb" ]


register :
    Origin
    -> { a | clientDataJSON : String, attestationObject : String, transports : List String }
    -> Result String Registration
register expected { clientDataJSON, attestationObject, transports } =
    Result.map2
        (\challenge credential ->
            { challenge = challenge
            , credentialId = credential.id
            , publicKey = credential.publicKey
            , aaguid = credential.aaguid
            , signCount = credential.signCount
            , backupEligible = credential.backupEligible
            , backedUp = credential.backedUp
            , transports = List.filter (\transport -> List.member transport knownTransports) transports
            }
        )
        (challengeOf "webauthn.create" (Origin.toString expected) clientDataJSON)
        (fromBase64Url attestationObject
            |> Maybe.andThen (Cbor.Decode.decode Cbor.Decode.any)
            |> Maybe.andThen authenticatorDataOf
            |> Result.fromMaybe "The attestation object is not CBOR with authenticator data."
            |> Result.andThen (credentialOf (Origin.rpId expected))
        )


authenticate :
    Origin
    -> { a | clientDataJSON : String, authenticatorData : String }
    -> Result String Authentication
authenticate expected { clientDataJSON, authenticatorData } =
    Result.map2
        (\challenge assertion ->
            { challenge = challenge
            , signCount = assertion.signCount
            , backedUp = assertion.backedUp
            , signed = assertion.signed
            }
        )
        (challengeOf "webauthn.get" (Origin.toString expected) clientDataJSON)
        (Maybe.map2 Tuple.pair (fromBase64Url authenticatorData) (fromBase64Url clientDataJSON)
            |> Result.fromMaybe "The authenticator data is not base64url."
            |> Result.andThen (assertionOf (Origin.rpId expected))
        )



-- CLIENT DATA


type alias ClientData =
    { ceremony : String
    , challenge : String
    , origin : String
    , crossOrigin : Bool
    }


challengeOf : String -> String -> String -> Result String Challenge
challengeOf ceremony origin clientDataJSON =
    fromBase64Url clientDataJSON
        |> Maybe.andThen (\bytes -> Binary.decode (Binary.string (Bytes.width bytes)) bytes)
        |> Maybe.andThen (Decode.decodeString clientDataDecoder >> Result.toMaybe)
        |> Result.fromMaybe "The client data is not the JSON of a ceremony."
        |> Result.andThen
            (\clientData ->
                if clientData.ceremony /= ceremony then
                    Err ("The client data is for " ++ clientData.ceremony ++ ", not for " ++ ceremony ++ ".")

                else if clientData.origin /= origin then
                    Err ("The client data comes from " ++ clientData.origin ++ ", not from " ++ origin ++ ".")

                else if clientData.crossOrigin then
                    Err "The passkey was used in a frame of another origin."

                else
                    Challenge.fromString clientData.challenge
                        |> Result.fromMaybe "The client data has no challenge."
            )


clientDataDecoder : Decoder ClientData
clientDataDecoder =
    Decode.map4 ClientData
        (Decode.field "type" Decode.string)
        (Decode.field "challenge" Decode.string)
        (Decode.field "origin" Decode.string)
        (Decode.maybe (Decode.field "crossOrigin" Decode.bool)
            |> Decode.map (Maybe.withDefault False)
        )



-- AUTHENTICATOR DATA


type alias Header =
    { rpIdHash : Bytes
    , flags : Int
    , signCount : Int
    }


type alias Credential =
    { id : CredentialId
    , publicKey : String
    , aaguid : String
    , signCount : Int
    , backupEligible : Bool
    , backedUp : Bool
    }


type alias Attested =
    { header : Header
    , aaguid : List Int
    , credentialId : Bytes
    , rest : Bytes
    }


headerDecoder : Binary.Decoder Header
headerDecoder =
    Binary.map3 Header
        (Binary.bytes 32)
        Binary.unsignedInt8
        (Binary.unsignedInt32 BE)


flag : Int -> Header -> Bool
flag bit header =
    Bitwise.and bit header.flags /= 0


checked : String -> Header -> Result String Header
checked rp header =
    if Base64.fromBytes header.rpIdHash /= Base64.fromBytes (SHA256.toBytes (SHA256.fromString rp)) then
        Err ("The passkey is not for " ++ rp ++ ".")

    else if not (flag 0x01 header) then
        Err "The user was not present."

    else if flag 0x10 header && not (flag 0x08 header) then
        Err "The passkey is backed up but not eligible for backup."

    else
        Ok header


assertionOf : String -> ( Bytes, Bytes ) -> Result String { signCount : Int, backedUp : Bool, signed : String }
assertionOf rp ( authenticatorData, clientData ) =
    Binary.decode headerDecoder authenticatorData
        |> Result.fromMaybe "The authenticator data is too short."
        |> Result.andThen (checked rp)
        |> Result.andThen
            (\header ->
                Bytes.Encode.sequence
                    [ Bytes.Encode.bytes authenticatorData
                    , Bytes.Encode.bytes (SHA256.toBytes (SHA256.fromBytes clientData))
                    ]
                    |> Bytes.Encode.encode
                    |> toBase64Url
                    |> Result.fromMaybe "The signed data could not be written."
                    |> Result.map
                        (\signed ->
                            { signCount = header.signCount
                            , backedUp = flag 0x10 header
                            , signed = signed
                            }
                        )
            )


authenticatorDataOf : CborItem -> Maybe Bytes
authenticatorDataOf attestation =
    case attestation of
        CborMap entries ->
            entries
                |> List.filterMap
                    (\entry ->
                        case entry of
                            ( CborString "authData", CborBytes bytes ) ->
                                Just bytes

                            _ ->
                                Nothing
                    )
                |> List.head

        _ ->
            Nothing


credentialOf : String -> Bytes -> Result String Credential
credentialOf rp bytes =
    Binary.decode (attestedDecoder (Bytes.width bytes)) bytes
        |> Result.fromMaybe "The authenticator data holds no credential."
        |> Result.andThen
            (\attested ->
                checked rp attested.header
                    |> Result.andThen
                        (\header ->
                            if flag 0x40 header then
                                Result.map2
                                    (\id publicKey ->
                                        { id = id
                                        , publicKey = publicKey
                                        , aaguid = uuid attested.aaguid
                                        , signCount = header.signCount
                                        , backupEligible = flag 0x08 header
                                        , backedUp = flag 0x10 header
                                        }
                                    )
                                    (toBase64Url attested.credentialId
                                        |> Maybe.andThen CredentialId.fromString
                                        |> Result.fromMaybe "The credential id could not be read."
                                    )
                                    (publicKeyOf attested.rest)

                            else
                                Err "The authenticator data holds no credential."
                        )
            )


attestedDecoder : Int -> Binary.Decoder Attested
attestedDecoder width =
    Binary.map3 (\header aaguid credentialId -> ( header, aaguid, credentialId ))
        headerDecoder
        (byteList 16)
        (Binary.unsignedInt16 BE
            |> Binary.andThen
                (\length ->
                    if length >= 1 && length <= 1023 then
                        Binary.bytes length

                    else
                        Binary.fail
                )
        )
        |> Binary.andThen
            (\( header, aaguid, credentialId ) ->
                let
                    remaining =
                        width - 55 - Bytes.width credentialId
                in
                if remaining >= 1 then
                    Binary.bytes remaining
                        |> Binary.map (Attested header aaguid credentialId)

                else
                    Binary.fail
            )


uuid : List Int -> String
uuid bytes =
    let
        hex =
            String.concat (List.map hexByte bytes)
    in
    String.join "-"
        [ String.slice 0 8 hex
        , String.slice 8 12 hex
        , String.slice 12 16 hex
        , String.slice 16 20 hex
        , String.slice 20 32 hex
        ]


hexByte : Int -> String
hexByte byte =
    hexDigit (byte // 16) ++ hexDigit (modBy 16 byte)


hexDigit : Int -> String
hexDigit digit =
    String.slice digit (digit + 1) "0123456789abcdef"


byteList : Int -> Binary.Decoder (List Int)
byteList count =
    Binary.loop ( count, [] )
        (\( remaining, bytes ) ->
            if remaining <= 0 then
                Binary.succeed (Binary.Done (List.reverse bytes))

            else
                Binary.map (\byte -> Binary.Loop ( remaining - 1, byte :: bytes )) Binary.unsignedInt8
        )



-- PUBLIC KEYS


publicKeyOf : Bytes -> Result String String
publicKeyOf bytes =
    case Cbor.Decode.decode Cbor.Decode.any bytes of
        Just (CborMap entries) ->
            case intAt 3 entries of
                Just algorithm ->
                    if not (List.member algorithm algorithms) then
                        Err ("The public key uses the algorithm " ++ String.fromInt algorithm ++ ", which is not accepted.")

                    else if keyOf entries == Nothing then
                        Err "The public key lacks a part its algorithm needs."

                    else
                        Cbor.Encode.encode (Cbor.Encode.any (CborMap entries))
                            |> toBase64Url
                            |> Result.fromMaybe "The public key could not be read."

                Nothing ->
                    Err "The public key names no algorithm."

        _ ->
            Err "The public key is not a COSE key."


key : String -> Result String Key
key publicKey =
    case fromBase64Url publicKey |> Maybe.andThen (Cbor.Decode.decode Cbor.Decode.any) of
        Just (CborMap entries) ->
            keyOf entries
                |> Result.fromMaybe "The stored public key is not one of the accepted kinds."

        _ ->
            Err "The stored public key is not a COSE key."


keyOf : List ( CborItem, CborItem ) -> Maybe Key
keyOf entries =
    let
        algorithm =
            intAt 3 entries

        curve =
            intAt -1 entries
    in
    if algorithm == Just -7 && curve == Just 1 then
        Maybe.map2 (\x y -> Es256 { x = x, y = y }) (bytesAt -2 entries) (bytesAt -3 entries)

    else if algorithm == Just -8 && curve == Just 6 then
        Maybe.map (\x -> EdDsa { x = x }) (bytesAt -2 entries)

    else if algorithm == Just -257 then
        Maybe.map2 (\n e -> Rs256 { n = n, e = e }) (bytesAt -1 entries) (bytesAt -2 entries)

    else
        Nothing


intAt : Int -> List ( CborItem, CborItem ) -> Maybe Int
intAt label entries =
    entries
        |> List.filterMap
            (\entry ->
                case entry of
                    ( CborInt32 found, CborInt32 value ) ->
                        if found == label then
                            Just value

                        else
                            Nothing

                    _ ->
                        Nothing
            )
        |> List.head


bytesAt : Int -> List ( CborItem, CborItem ) -> Maybe String
bytesAt label entries =
    entries
        |> List.filterMap
            (\entry ->
                case entry of
                    ( CborInt32 found, CborBytes value ) ->
                        if found == label then
                            toBase64Url value

                        else
                            Nothing

                    _ ->
                        Nothing
            )
        |> List.head



-- SIGNATURES


signature : Key -> String -> Result String String
signature publicKey encoded =
    case publicKey of
        Es256 _ ->
            fromBase64Url encoded
                |> Maybe.andThen (Binary.decode derDecoder)
                |> Maybe.andThen
                    (\( r, s ) ->
                        Maybe.map2 (++) (fixed r) (fixed s)
                    )
                |> Maybe.map (List.map Bytes.Encode.unsignedInt8 >> Bytes.Encode.sequence >> Bytes.Encode.encode)
                |> Maybe.andThen toBase64Url
                |> Result.fromMaybe "The signature is not a DER pair of 32-byte integers."

        _ ->
            fromBase64Url encoded
                |> Maybe.andThen toBase64Url
                |> Result.fromMaybe "The signature is not base64url."


derDecoder : Binary.Decoder ( List Int, List Int )
derDecoder =
    tagged 0x30
        |> Binary.andThen (\_ -> Binary.map2 Tuple.pair derInteger derInteger)


derInteger : Binary.Decoder (List Int)
derInteger =
    tagged 0x02
        |> Binary.andThen
            (\length ->
                if length >= 1 && length <= 33 then
                    byteList length

                else
                    Binary.fail
            )


tagged : Int -> Binary.Decoder Int
tagged expected =
    Binary.unsignedInt8
        |> Binary.andThen
            (\found ->
                if found == expected then
                    Binary.unsignedInt8

                else
                    Binary.fail
            )


fixed : List Int -> Maybe (List Int)
fixed bytes =
    let
        significant =
            withoutLeadingZeros bytes

        length =
            List.length significant
    in
    if length <= 32 then
        Just (List.repeat (32 - length) 0 ++ significant)

    else
        Nothing


withoutLeadingZeros : List Int -> List Int
withoutLeadingZeros bytes =
    case bytes of
        0 :: rest ->
            withoutLeadingZeros rest

        _ ->
            bytes



-- BASE64URL


fromBase64Url : String -> Maybe Bytes
fromBase64Url value =
    let
        standard =
            value
                |> String.replace "-" "+"
                |> String.replace "_" "/"
    in
    Base64.toBytes (standard ++ String.repeat (modBy 4 (4 - modBy 4 (String.length standard))) "=")


toBase64Url : Bytes -> Maybe String
toBase64Url bytes =
    Base64.fromBytes bytes
        |> Maybe.map
            (String.replace "+" "-"
                >> String.replace "/" "_"
                >> String.replace "=" ""
            )
