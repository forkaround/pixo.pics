module InteropDefinitions exposing (Flags, FromElm(..), Theme(..), ToElm(..), interop, themeToString)

import Api
import Data.Challenge as Challenge
import Data.CredentialId as CredentialId
import Data.Dimension as Dimension
import Data.Prompt as Prompt
import Data.Seed as Seed
import Data.Steps as Steps
import Dict
import Idb
import Library
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
    | RunLibrary Library.Command


type ToElm
    = SystemThemeChanged Bool
    | PasskeyCreated Api.Attestation
    | PasskeyUsed Api.Assertion
    | PasskeyFailed String
    | PipelineEvent Pipeline.Event
    | LibraryEvent Library.Event


type alias Flags =
    { theme : Maybe Theme
    , systemDark : Bool
    , entropy : Int
    , models : Maybe String
    , canShare : Bool
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
        (\writeToLocalStorage createPasskey getPasskey checkWebGpu loadModel generateImage cancelPipeline releaseModel clearModelCache idbGetAll idbAdd idbDelete idbRelease sharePicture copyPicture openTab value ->
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

                RunLibrary (Library.Database (Idb.GetAll request)) ->
                    idbGetAll request

                RunLibrary (Library.Database (Idb.Add request)) ->
                    idbAdd request

                RunLibrary (Library.Database (Idb.Delete request)) ->
                    idbDelete request

                RunLibrary (Library.Database (Idb.Release urls)) ->
                    idbRelease urls

                RunLibrary (Library.Share picture) ->
                    sharePicture picture

                RunLibrary (Library.Copy picture) ->
                    copyPicture picture

                RunLibrary (Library.Open url) ->
                    openTab url
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
                [ TsEncode.required "model" Pipeline.weightsId TsEncode.string
                , TsEncode.required "cache" (always True) TsEncode.bool
                , TsEncode.required "force" (always False) TsEncode.bool
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
        |> TsEncode.variantTagged "IdbGetAll"
            (TsEncode.object
                [ TsEncode.required "database" .database databaseEncoder
                , TsEncode.required "store" .store TsEncode.string
                , TsEncode.required "label" .label TsEncode.string
                ]
            )
        |> TsEncode.variantTagged "IdbAdd"
            (TsEncode.object
                [ TsEncode.required "database" .database databaseEncoder
                , TsEncode.required "store" .store TsEncode.string
                , TsEncode.required "row" (.row >> Dict.fromList) (TsEncode.dict identity TsEncode.value)
                , TsEncode.required "blobs"
                    .blobs
                    (TsEncode.list
                        (TsEncode.object
                            [ TsEncode.required "field" Tuple.first TsEncode.string
                            , TsEncode.required "url" Tuple.second TsEncode.string
                            ]
                        )
                    )
                , TsEncode.required "label" .label TsEncode.string
                ]
            )
        |> TsEncode.variantTagged "IdbDelete"
            (TsEncode.object
                [ TsEncode.required "database" .database databaseEncoder
                , TsEncode.required "store" .store TsEncode.string
                , TsEncode.required "key" .key TsEncode.value
                , TsEncode.required "label" .label TsEncode.string
                ]
            )
        |> TsEncode.variantTagged "IdbRelease"
            (TsEncode.object [ TsEncode.required "urls" identity (TsEncode.list TsEncode.string) ])
        |> TsEncode.variantTagged "SharePicture"
            (TsEncode.object
                [ TsEncode.required "url" .url TsEncode.string
                , TsEncode.required "name" (.job >> Library.fileName) TsEncode.string
                , TsEncode.required "text" (.job >> .prompt >> Prompt.toString) TsEncode.string
                ]
            )
        |> TsEncode.variantTagged "CopyPicture"
            (TsEncode.object [ TsEncode.required "url" .url TsEncode.string ])
        |> TsEncode.variantTagged "OpenTab"
            (TsEncode.object [ TsEncode.required "url" identity TsEncode.string ])
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
        , ( "IdbRows"
          , TsDecode.map2 (\label rows -> answered (Idb.Rows { label = label, rows = rows }))
                (TsDecode.field "label" TsDecode.string)
                (TsDecode.field "rows" (TsDecode.list TsDecode.value))
          )
        , ( "IdbAdded"
          , TsDecode.map2 (\label row -> answered (Idb.Added { label = label, row = row }))
                (TsDecode.field "label" TsDecode.string)
                (TsDecode.field "row" TsDecode.value)
          )
        , ( "IdbDeleted"
          , TsDecode.map2 (\label key -> answered (Idb.Deleted { label = label, key = key }))
                (TsDecode.field "label" TsDecode.string)
                (TsDecode.field "key" TsDecode.value)
          )
        , ( "IdbFailed"
          , TsDecode.map2 (\label message -> answered (Idb.Failed { label = label, message = message }))
                (TsDecode.field "label" TsDecode.string)
                (TsDecode.field "message" TsDecode.string)
          )
        , ( "PictureCopied", TsDecode.succeed (LibraryEvent Library.Copied) )
        , ( "ShareFailed"
          , TsDecode.map (Library.ShareFailed >> LibraryEvent) (TsDecode.field "name" TsDecode.string)
          )
        ]


answered : Idb.Event -> ToElm
answered =
    Library.Answered >> LibraryEvent


databaseEncoder : Encoder Idb.Database
databaseEncoder =
    TsEncode.object
        [ TsEncode.required "name" .name TsEncode.string
        , TsEncode.required "version" .version TsEncode.int
        , TsEncode.required "stores"
            .stores
            (TsEncode.list
                (TsEncode.object
                    [ TsEncode.required "name" .name TsEncode.string
                    , TsEncode.required "keyPath" .keyPath TsEncode.string
                    , TsEncode.required "autoIncrement" .autoIncrement TsEncode.bool
                    ]
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
    TsDecode.map5 Flags
        (TsDecode.field "theme" (TsDecode.nullable themeDecoder))
        (TsDecode.field "systemDark" TsDecode.bool)
        (TsDecode.field "entropy" TsDecode.int)
        (TsDecode.field "models" (TsDecode.nullable TsDecode.string))
        (TsDecode.field "canShare" TsDecode.bool)
