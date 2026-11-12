module InteropDefinitions exposing (Flags, FromElm(..), Theme(..), ToElm(..), interop, themeToString)

import Api
import Data.Challenge as Challenge
import Data.CredentialId as CredentialId
import Data.Dimension as Dimension
import Data.Prompt as Prompt
import Data.Seed as Seed
import Data.Steps as Steps
import Pipeline
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
    | RunPipeline Pipeline.Command


type ToElm
    = SystemThemeChanged Bool
    | PasskeyCreated Api.Attestation
    | PasskeyUsed Api.Assertion
    | PasskeyFailed String
    | PipelineEvent Pipeline.Event


type alias Flags =
    { theme : Maybe Theme
    , systemDark : Bool
    , entropy : Int
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
        (\writeToLocalStorage createPasskey getPasskey checkWebGpu loadModel generateImage cancelPipeline releaseModel clearModelCache value ->
            case value of
                WriteToLocalStorage entry ->
                    writeToLocalStorage entry

                CreatePasskey options ->
                    createPasskey options

                GetPasskey options ->
                    getPasskey options

                RunPipeline Pipeline.Check ->
                    checkWebGpu

                RunPipeline (Pipeline.Load options) ->
                    loadModel options

                RunPipeline (Pipeline.Generate job) ->
                    generateImage job

                RunPipeline Pipeline.Cancel ->
                    cancelPipeline

                RunPipeline Pipeline.Release ->
                    releaseModel

                RunPipeline Pipeline.ClearCache ->
                    clearModelCache
        )
        |> TsEncode.variantTagged "WriteToLocalStorage"
            (TsEncode.object
                [ TsEncode.required "key" .key TsEncode.string
                , TsEncode.required "value" .value TsEncode.string
                ]
            )
        |> TsEncode.variantTagged "CreatePasskey"
            (TsEncode.object
                [ TsEncode.required "challenge" (.challenge >> Challenge.toString) TsEncode.string
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
                , TsEncode.required "excludeCredentials" (.excludeCredentials >> List.map CredentialId.toString) (TsEncode.list TsEncode.string)
                ]
            )
        |> TsEncode.variantTagged "GetPasskey"
            (TsEncode.object
                [ TsEncode.required "challenge" (.challenge >> Challenge.toString) TsEncode.string
                , TsEncode.required "rpId" .rpId TsEncode.string
                ]
            )
        |> TsEncode.variant0 "CheckWebGpu"
        |> TsEncode.variantTagged "LoadModel"
            (TsEncode.object
                [ TsEncode.required "model" (.weights >> Pipeline.weightsId) TsEncode.string
                , TsEncode.required "cache" .keep TsEncode.bool
                , TsEncode.required "force" .fresh TsEncode.bool
                ]
            )
        |> TsEncode.variantTagged "GenerateImage"
            (TsEncode.object
                [ TsEncode.required "prompt" (.prompt >> Prompt.toString) TsEncode.string
                , TsEncode.required "width" (.width >> Dimension.toInt) TsEncode.int
                , TsEncode.required "height" (.height >> Dimension.toInt) TsEncode.int
                , TsEncode.required "numInferenceSteps" (.steps >> Steps.toInt) TsEncode.int
                , TsEncode.required "seed" (.seed >> Seed.toInt) TsEncode.int
                , TsEncode.required "guidanceScale" (always Pipeline.guidance) TsEncode.float
                , TsEncode.required "frameBudget" (always Pipeline.frameBudget) TsEncode.int
                ]
            )
        |> TsEncode.variant0 "CancelPipeline"
        |> TsEncode.variant0 "ReleaseModel"
        |> TsEncode.variant0 "ClearModelCache"
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
        , ( "WebGpuChecked"
          , TsDecode.map (Pipeline.SupportChecked >> PipelineEvent) (TsDecode.field "supported" TsDecode.bool)
          )
        , ( "ModelProgress"
          , TsDecode.map (Pipeline.Downloaded >> PipelineEvent)
                (TsDecode.map4 Pipeline.Download
                    (TsDecode.field "component" componentDecoder)
                    (TsDecode.field "loaded" TsDecode.int)
                    (TsDecode.field "total" TsDecode.int)
                    (TsDecode.field "fromCache" TsDecode.bool)
                )
          )
        , ( "ModelLoaded", TsDecode.succeed (PipelineEvent Pipeline.ModelLoaded) )
        , ( "GenerationStep"
          , TsDecode.map (Pipeline.StepFinished >> PipelineEvent) (TsDecode.field "step" TsDecode.int)
          )
        , ( "ImageGenerated"
          , TsDecode.map (Pipeline.PictureMade >> PipelineEvent) (TsDecode.field "url" TsDecode.string)
          )
        , ( "ModelReleased", TsDecode.succeed (PipelineEvent Pipeline.ModelReleased) )
        , ( "ModelCacheCleared", TsDecode.succeed (PipelineEvent Pipeline.CacheCleared) )
        , ( "PipelineFailed"
          , TsDecode.map (Pipeline.Failed >> PipelineEvent)
                (TsDecode.map2 Pipeline.Failure
                    (TsDecode.field "name" TsDecode.string)
                    (TsDecode.field "message" TsDecode.string)
                )
          )
        ]


componentDecoder : Decoder Pipeline.Component
componentDecoder =
    TsDecode.stringUnion
        [ ( "text_encoder", Pipeline.TextEncoder )
        , ( "transformer", Pipeline.Transformer )
        , ( "vae", Pipeline.Vae )
        ]


themeDecoder : Decoder Theme
themeDecoder =
    TsDecode.stringUnion [ ( "light", Light ), ( "dark", Dark ), ( "system", System ) ]


flags : Decoder Flags
flags =
    TsDecode.map3 Flags
        (TsDecode.field "theme" (TsDecode.nullable themeDecoder))
        (TsDecode.field "systemDark" TsDecode.bool)
        (TsDecode.field "entropy" TsDecode.int)
