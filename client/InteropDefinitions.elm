module InteropDefinitions exposing (Flags, FromElm(..), Theme(..), ToElm(..), interop, themeToString)

import Api
import TsJson.Decode as TsDecode exposing (Decoder)
import TsJson.Encode as TsEncode exposing (Encoder)


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


type Theme
    = Light
    | Dark
    | System


type FromElm
    = WriteToLocalStorage { key : String, value : String }
    | CreatePasskey Api.PasskeyOptions
    | GetPasskey Api.SignInOptions


type ToElm
    = SystemThemeChanged Bool
    | PasskeyCreated Api.Attestation
    | PasskeyUsed Api.Assertion
    | PasskeyFailed String


type alias Flags =
    { theme : Maybe Theme
    , systemDark : Bool
    }


themeToString : Theme -> String
themeToString theme =
    case theme of
        Light ->
            "light"

        Dark ->
            "dark"

        System ->
            "system"


fromElm : Encoder FromElm
fromElm =
    TsEncode.union
        (\writeToLocalStorage createPasskey getPasskey value ->
            case value of
                WriteToLocalStorage entry ->
                    writeToLocalStorage entry

                CreatePasskey options ->
                    createPasskey options

                GetPasskey options ->
                    getPasskey options
        )
        |> TsEncode.variantTagged "WriteToLocalStorage"
            (TsEncode.object
                [ TsEncode.required "key" .key TsEncode.string
                , TsEncode.required "value" .value TsEncode.string
                ]
            )
        |> TsEncode.variantTagged "CreatePasskey"
            (TsEncode.object
                [ TsEncode.required "challenge" .challenge TsEncode.string
                , TsEncode.required "rp"
                    .rp
                    (TsEncode.object
                        [ TsEncode.required "id" .id TsEncode.string
                        , TsEncode.required "name" .name TsEncode.string
                        ]
                    )
                , TsEncode.required "user"
                    .user
                    (TsEncode.object
                        [ TsEncode.required "id" .id TsEncode.string
                        , TsEncode.required "name" .name TsEncode.string
                        , TsEncode.required "displayName" .displayName TsEncode.string
                        ]
                    )
                , TsEncode.required "algorithms" .algorithms (TsEncode.list TsEncode.int)
                , TsEncode.required "excludeCredentials" .excludeCredentials (TsEncode.list TsEncode.string)
                ]
            )
        |> TsEncode.variantTagged "GetPasskey"
            (TsEncode.object
                [ TsEncode.required "challenge" .challenge TsEncode.string
                , TsEncode.required "rpId" .rpId TsEncode.string
                ]
            )
        |> TsEncode.buildUnion


toElm : Decoder ToElm
toElm =
    TsDecode.discriminatedUnion "tag"
        [ ( "SystemThemeChanged", TsDecode.map SystemThemeChanged (TsDecode.field "dark" TsDecode.bool) )
        , ( "PasskeyCreated"
          , TsDecode.map PasskeyCreated
                (TsDecode.map3 Api.Attestation
                    (TsDecode.field "clientDataJSON" TsDecode.string)
                    (TsDecode.field "attestationObject" TsDecode.string)
                    (TsDecode.field "transports" (TsDecode.list TsDecode.string))
                )
          )
        , ( "PasskeyUsed"
          , TsDecode.map PasskeyUsed
                (TsDecode.map4 Api.Assertion
                    (TsDecode.field "id" TsDecode.string)
                    (TsDecode.field "clientDataJSON" TsDecode.string)
                    (TsDecode.field "authenticatorData" TsDecode.string)
                    (TsDecode.field "signature" TsDecode.string)
                )
          )
        , ( "PasskeyFailed", TsDecode.map PasskeyFailed (TsDecode.field "error" TsDecode.string) )
        ]


themeDecoder : Decoder Theme
themeDecoder =
    TsDecode.stringUnion [ ( "light", Light ), ( "dark", Dark ), ( "system", System ) ]


flags : Decoder Flags
flags =
    TsDecode.map2 Flags
        (TsDecode.field "theme" (TsDecode.nullable themeDecoder))
        (TsDecode.field "systemDark" TsDecode.bool)
