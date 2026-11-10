module WebAuthnTest exposing (suite)

import Expect
import Test exposing (Test, describe, test)
import WebAuthn


suite : Test
suite =
    describe "WebAuthn"
        [ describe "register"
            [ test "reads the challenge, the credential, its flags and the transports it knows" <|
                \() ->
                    register clientData synced
                        |> Expect.equal
                            (Ok
                                { challenge = "Y2hhbGxlbmdl"
                                , credentialId = credentialId
                                , publicKey = publicKey
                                , aaguid = "00000000-0000-0000-0000-000000000000"
                                , signCount = 0
                                , backupEligible = True
                                , backedUp = True
                                , transports = [ "internal", "hybrid" ]
                                }
                            )
            , test "reads a passkey that stays on its device, and its counter" <|
                \() ->
                    register clientData deviceBound
                        |> Result.map (\{ signCount, backupEligible, backedUp } -> ( signCount, backupEligible, backedUp ))
                        |> Expect.equal (Ok ( 7, False, False ))
            , test "reads which authenticator made the passkey" <|
                \() ->
                    register clientData apple
                        |> Result.map .aaguid
                        |> Expect.equal (Ok "fbfc3007-154e-4ecc-8c0b-6e020557d7bd")
            , test "leaves out the extensions after the public key" <|
                \() ->
                    register clientData withExtensions
                        |> Result.map .publicKey
                        |> Expect.equal (Ok publicKey)
            , test "refuses the client data of a sign-in" <|
                \() ->
                    register clientDataGet synced
                        |> Expect.equal (Err "The client data is for webauthn.get, not for webauthn.create.")
            , test "refuses a passkey created on another site" <|
                \() ->
                    register clientDataElsewhere synced
                        |> Expect.equal (Err "The client data comes from https://evil.example, not from https://pixo.pics.")
            , test "refuses a passkey created in a frame of another origin" <|
                \() ->
                    register clientDataFramed synced
                        |> Expect.equal (Err "The passkey was used in a frame of another origin.")
            , test "refuses client data that is not JSON" <|
                \() ->
                    register "bm90IGpzb24" synced
                        |> Expect.equal (Err "The client data is not the JSON of a ceremony.")
            , test "refuses a passkey created for another relying party" <|
                \() ->
                    WebAuthn.register { origin = "https://pixo.pics", rpId = "example.com" }
                        { clientDataJSON = clientData, attestationObject = synced, transports = [] }
                        |> Expect.equal (Err "The passkey is not for example.com.")
            , test "refuses a passkey created without the user" <|
                \() ->
                    register clientData absent
                        |> Expect.equal (Err "The user was not present.")
            , test "refuses a backup that its passkey is not eligible for" <|
                \() ->
                    register clientData backedUpNotEligible
                        |> Expect.equal (Err "The passkey is backed up but not eligible for backup.")
            , test "refuses an algorithm it did not ask for" <|
                \() ->
                    register clientData unknownAlgorithm
                        |> Expect.equal (Err "The public key uses the algorithm -65535, which is not accepted.")
            , test "refuses a key on a curve its algorithm does not use" <|
                \() ->
                    register clientData otherCurve
                        |> Expect.equal (Err "The public key lacks a part its algorithm needs.")
            , test "refuses authenticator data without a credential" <|
                \() ->
                    register clientData noCredential
                        |> Expect.equal (Err "The authenticator data holds no credential.")
            , test "refuses an attestation object that is not CBOR" <|
                \() ->
                    register clientData "bm90IGNib3I"
                        |> Expect.equal (Err "The attestation object is not CBOR with authenticator data.")
            ]
        , describe "authenticate"
            [ test "reads the challenge and the flags, and joins what the passkey signed" <|
                \() ->
                    authenticate clientDataGet "hc4asGtQ0lfqei0FVSxvDEzvgNF-FdmbKJXm3Ic-1uwdAAAAAA"
                        |> Expect.equal
                            (Ok
                                { challenge = "Y2hhbGxlbmdl"
                                , signCount = 0
                                , backedUp = True
                                , signed = "hc4asGtQ0lfqei0FVSxvDEzvgNF-FdmbKJXm3Ic-1uwdAAAAAGGrdV6kson9YunnR9B0osE5k1x-xRe64NBdqS2heNa4"
                                }
                            )
            , test "reads the counter of a passkey that stays on its device" <|
                \() ->
                    authenticate clientDataGet "hc4asGtQ0lfqei0FVSxvDEzvgNF-FdmbKJXm3Ic-1uwFAAAABw"
                        |> Result.map (\{ signCount, backedUp } -> ( signCount, backedUp ))
                        |> Expect.equal (Ok ( 7, False ))
            , test "refuses the client data of a registration" <|
                \() ->
                    authenticate clientData "hc4asGtQ0lfqei0FVSxvDEzvgNF-FdmbKJXm3Ic-1uwdAAAAAA"
                        |> Expect.equal (Err "The client data is for webauthn.create, not for webauthn.get.")
            , test "refuses a passkey used for another relying party" <|
                \() ->
                    authenticate clientDataGet "o3mm9u6vuaVeN4wRgDTidR5oL6ufLTCrE9ISVYbOGUcdAAAAAA"
                        |> Expect.equal (Err "The passkey is not for pixo.pics.")
            , test "refuses a passkey used without the user" <|
                \() ->
                    authenticate clientDataGet "hc4asGtQ0lfqei0FVSxvDEzvgNF-FdmbKJXm3Ic-1uwcAAAAAA"
                        |> Expect.equal (Err "The user was not present.")
            , test "refuses authenticator data that stops short" <|
                \() ->
                    authenticate clientDataGet "hc4asGtQ0lfqei0FVSxvDEzvgNF"
                        |> Expect.equal (Err "The authenticator data is too short.")
            ]
        , describe "key"
            [ test "reads the point of an ES256 key" <|
                \() ->
                    WebAuthn.key publicKey
                        |> Expect.equal
                            (Ok
                                (WebAuthn.Es256
                                    { x = "AQIDBAUGBwgJCgsMDQ4PEBESExQVFhcYGRobHB0eHyA"
                                    , y = "__79_Pv6-fj39vX08_Lx8O_u7ezr6uno5-bl5OPi4eA"
                                    }
                                )
                            )
            , test "reads the point of an EdDSA key" <|
                \() ->
                    WebAuthn.key "pAEBAycgBiFYIAECAwQFBgcICQoLDA0ODxAREhMUFRYXGBkaGxwdHh8g"
                        |> Expect.equal (Ok (WebAuthn.EdDsa { x = "AQIDBAUGBwgJCgsMDQ4PEBESExQVFhcYGRobHB0eHyA" }))
            , test "reads the modulus and the exponent of an RS256 key" <|
                \() ->
                    WebAuthn.key "pAEDAzkBACBZAQCAgYKDhIWGh4iJiouMjY6PkJGSk5SVlpeYmZqbnJ2en6ChoqOkpaanqKmqq6ytrq-wsbKztLW2t7i5uru8vb6_wMHCw8TFxsfIycrLzM3Oz9DR0tPU1dbX2Nna29zd3t_g4eLj5OXm5-jp6uvs7e7v8PHy8_T19vf4-fr7_P3-_wABAgMEBQYHCAkKCwwNDg8QERITFBUWFxgZGhscHR4fICEiIyQlJicoKSorLC0uLzAxMjM0NTY3ODk6Ozw9Pj9AQUJDREVGR0hJSktMTU5PUFFSU1RVVldYWVpbXF1eX2BhYmNkZWZnaGlqa2xtbm9wcXJzdHV2d3h5ent8fX5_IUMBAAE"
                        |> Expect.equal
                            (Ok
                                (WebAuthn.Rs256
                                    { n = "gIGCg4SFhoeIiYqLjI2Oj5CRkpOUlZaXmJmam5ydnp-goaKjpKWmp6ipqqusra6vsLGys7S1tre4ubq7vL2-v8DBwsPExcbHyMnKy8zNzs_Q0dLT1NXW19jZ2tvc3d7f4OHi4-Tl5ufo6err7O3u7_Dx8vP09fb3-Pn6-_z9_v8AAQIDBAUGBwgJCgsMDQ4PEBESExQVFhcYGRobHB0eHyAhIiMkJSYnKCkqKywtLi8wMTIzNDU2Nzg5Ojs8PT4_QEFCQ0RFRkdISUpLTE1OT1BRUlNUVVZXWFlaW1xdXl9gYWJjZGVmZ2hpamtsbW5vcHFyc3R1dnd4eXp7fH1-fw"
                                    , e = "AQAB"
                                    }
                                )
                            )
            , test "refuses what is not a COSE key" <|
                \() ->
                    WebAuthn.key "bm90IGNib3I"
                        |> Expect.equal (Err "The stored public key is not a COSE key.")
            ]
        , describe "signature"
            [ test "turns the DER pair of an ES256 signature into two 32-byte integers" <|
                \() ->
                    WebAuthn.signature (WebAuthn.Es256 { x = "", y = "" })
                        "MEQCIQCAgYKDhIWGh4iJiouMjY6PkJGSk5SVlpeYmZqbnJ2enwIfEBESExQVFhcYGRobHB0eHyAhIiMkJSYnKCkqKywtLg"
                        |> Expect.equal
                            (Ok "gIGCg4SFhoeIiYqLjI2Oj5CRkpOUlZaXmJmam5ydnp8AEBESExQVFhcYGRobHB0eHyAhIiMkJSYnKCkqKywtLg")
            , test "refuses an ES256 signature that is not DER" <|
                \() ->
                    WebAuthn.signature (WebAuthn.Es256 { x = "", y = "" })
                        "AQIDBAUGBwgJCgsMDQ4PEBESExQVFhcYGRobHB0eHyAhIiMkJSYnKCkqKywtLi8wMTIzNDU2Nzg5Ojs8PT4_QA"
                        |> Expect.equal (Err "The signature is not a DER pair of 32-byte integers.")
            , test "leaves an EdDSA signature as it is" <|
                \() ->
                    WebAuthn.signature (WebAuthn.EdDsa { x = "" })
                        "AQIDBAUGBwgJCgsMDQ4PEBESExQVFhcYGRobHB0eHyAhIiMkJSYnKCkqKywtLi8wMTIzNDU2Nzg5Ojs8PT4_QA"
                        |> Expect.equal
                            (Ok "AQIDBAUGBwgJCgsMDQ4PEBESExQVFhcYGRobHB0eHyAhIiMkJSYnKCkqKywtLi8wMTIzNDU2Nzg5Ojs8PT4_QA")
            ]
        , describe "rpId"
            [ test "is the host of the origin" <|
                \() ->
                    WebAuthn.rpId "https://pixo.pics"
                        |> Expect.equal (Just "pixo.pics")
            , test "leaves out the port" <|
                \() ->
                    WebAuthn.rpId "http://localhost:5173"
                        |> Expect.equal (Just "localhost")
            ]
        ]


register : String -> String -> Result String WebAuthn.Registration
register clientDataJSON attestationObject =
    WebAuthn.register { origin = "https://pixo.pics", rpId = "pixo.pics" }
        { clientDataJSON = clientDataJSON
        , attestationObject = attestationObject
        , transports = [ "internal", "teleport", "hybrid" ]
        }


authenticate : String -> String -> Result String WebAuthn.Authentication
authenticate clientDataJSON authenticatorData =
    WebAuthn.authenticate { origin = "https://pixo.pics", rpId = "pixo.pics" }
        { clientDataJSON = clientDataJSON, authenticatorData = authenticatorData }


apple : String
apple =
    "o2NmbXRkbm9uZWdhdHRTdG10oGhhdXRoRGF0YViUhc4asGtQ0lfqei0FVSxvDEzvgNF-FdmbKJXm3Ic-1uxdAAAAAPv8MAcVTk7MjAtuAgVX170AEKChoqOkpaanqKmqq6ytrq-lAQIDJiABIVggAQIDBAUGBwgJCgsMDQ4PEBESExQVFhcYGRobHB0eHyAiWCD__v38-_r5-Pf29fTz8vHw7-7t7Ovq6ejn5uXk4-Lh4A"


otherCurve : String
otherCurve =
    "o2NmbXRkbm9uZWdhdHRTdG10oGhhdXRoRGF0YViUhc4asGtQ0lfqei0FVSxvDEzvgNF-FdmbKJXm3Ic-1uxdAAAAAAAAAAAAAAAAAAAAAAAAAAAAEKChoqOkpaanqKmqq6ytrq-lAQIDJiACIVggAQIDBAUGBwgJCgsMDQ4PEBESExQVFhcYGRobHB0eHyAiWCD__v38-_r5-Pf29fTz8vHw7-7t7Ovq6ejn5uXk4-Lh4A"


credentialId : String
credentialId =
    "oKGio6SlpqeoqaqrrK2urw"


publicKey : String
publicKey =
    "pQECAyYgASFYIAECAwQFBgcICQoLDA0ODxAREhMUFRYXGBkaGxwdHh8gIlgg__79_Pv6-fj39vX08_Lx8O_u7ezr6uno5-bl5OPi4eA"


clientData : String
clientData =
    "eyJ0eXBlIjoid2ViYXV0aG4uY3JlYXRlIiwiY2hhbGxlbmdlIjoiWTJoaGJHeGxibWRsIiwib3JpZ2luIjoiaHR0cHM6Ly9waXhvLnBpY3MiLCJjcm9zc09yaWdpbiI6ZmFsc2V9"


clientDataGet : String
clientDataGet =
    "eyJ0eXBlIjoid2ViYXV0aG4uZ2V0IiwiY2hhbGxlbmdlIjoiWTJoaGJHeGxibWRsIiwib3JpZ2luIjoiaHR0cHM6Ly9waXhvLnBpY3MifQ"


clientDataElsewhere : String
clientDataElsewhere =
    "eyJ0eXBlIjoid2ViYXV0aG4uY3JlYXRlIiwiY2hhbGxlbmdlIjoiWTJoaGJHeGxibWRsIiwib3JpZ2luIjoiaHR0cHM6Ly9ldmlsLmV4YW1wbGUifQ"


clientDataFramed : String
clientDataFramed =
    "eyJ0eXBlIjoid2ViYXV0aG4uY3JlYXRlIiwiY2hhbGxlbmdlIjoiWTJoaGJHeGxibWRsIiwib3JpZ2luIjoiaHR0cHM6Ly9waXhvLnBpY3MiLCJjcm9zc09yaWdpbiI6dHJ1ZX0"


synced : String
synced =
    "o2NmbXRkbm9uZWdhdHRTdG10oGhhdXRoRGF0YViUhc4asGtQ0lfqei0FVSxvDEzvgNF-FdmbKJXm3Ic-1uxdAAAAAAAAAAAAAAAAAAAAAAAAAAAAEKChoqOkpaanqKmqq6ytrq-lAQIDJiABIVggAQIDBAUGBwgJCgsMDQ4PEBESExQVFhcYGRobHB0eHyAiWCD__v38-_r5-Pf29fTz8vHw7-7t7Ovq6ejn5uXk4-Lh4A"


deviceBound : String
deviceBound =
    "o2NmbXRkbm9uZWdhdHRTdG10oGhhdXRoRGF0YViUhc4asGtQ0lfqei0FVSxvDEzvgNF-FdmbKJXm3Ic-1uxFAAAABwAAAAAAAAAAAAAAAAAAAAAAEKChoqOkpaanqKmqq6ytrq-lAQIDJiABIVggAQIDBAUGBwgJCgsMDQ4PEBESExQVFhcYGRobHB0eHyAiWCD__v38-_r5-Pf29fTz8vHw7-7t7Ovq6ejn5uXk4-Lh4A"


withExtensions : String
withExtensions =
    "o2NmbXRkbm9uZWdhdHRTdG10oGhhdXRoRGF0YViihc4asGtQ0lfqei0FVSxvDEzvgNF-FdmbKJXm3Ic-1uzFAAAAAAAAAAAAAAAAAAAAAAAAAAAAEKChoqOkpaanqKmqq6ytrq-lAQIDJiABIVggAQIDBAUGBwgJCgsMDQ4PEBESExQVFhcYGRobHB0eHyAiWCD__v38-_r5-Pf29fTz8vHw7-7t7Ovq6ejn5uXk4-Lh4KFrY3JlZFByb3RlY3QC"


absent : String
absent =
    "o2NmbXRkbm9uZWdhdHRTdG10oGhhdXRoRGF0YViUhc4asGtQ0lfqei0FVSxvDEzvgNF-FdmbKJXm3Ic-1uxcAAAAAAAAAAAAAAAAAAAAAAAAAAAAEKChoqOkpaanqKmqq6ytrq-lAQIDJiABIVggAQIDBAUGBwgJCgsMDQ4PEBESExQVFhcYGRobHB0eHyAiWCD__v38-_r5-Pf29fTz8vHw7-7t7Ovq6ejn5uXk4-Lh4A"


backedUpNotEligible : String
backedUpNotEligible =
    "o2NmbXRkbm9uZWdhdHRTdG10oGhhdXRoRGF0YViUhc4asGtQ0lfqei0FVSxvDEzvgNF-FdmbKJXm3Ic-1uxVAAAAAAAAAAAAAAAAAAAAAAAAAAAAEKChoqOkpaanqKmqq6ytrq-lAQIDJiABIVggAQIDBAUGBwgJCgsMDQ4PEBESExQVFhcYGRobHB0eHyAiWCD__v38-_r5-Pf29fTz8vHw7-7t7Ovq6ejn5uXk4-Lh4A"


unknownAlgorithm : String
unknownAlgorithm =
    "o2NmbXRkbm9uZWdhdHRTdG10oGhhdXRoRGF0YViWhc4asGtQ0lfqei0FVSxvDEzvgNF-FdmbKJXm3Ic-1uxdAAAAAAAAAAAAAAAAAAAAAAAAAAAAEKChoqOkpaanqKmqq6ytrq-lAQIDOf_-IAEhWCABAgMEBQYHCAkKCwwNDg8QERITFBUWFxgZGhscHR4fICJYIP_-_fz7-vn49_b19PPy8fDv7u3s6-rp6Ofm5eTj4uHg"


noCredential : String
noCredential =
    "o2NmbXRkbm9uZWdhdHRTdG10oGhhdXRoRGF0YVglhc4asGtQ0lfqei0FVSxvDEzvgNF-FdmbKJXm3Ic-1uwFAAAAAA"
