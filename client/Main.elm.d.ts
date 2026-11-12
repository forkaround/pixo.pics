export type JsonObject = { [Key in string]?: JsonValue };
export type JsonArray = JsonValue[];

/**
Matches any valid JSON value.
Source: https://github.com/sindresorhus/type-fest/blob/master/source/basic.d.ts
*/
export type JsonValue =
  | string
  | number
  | boolean
  | null
  | JsonObject
  | JsonArray;

export interface ElmApp {
  ports: {
    interopFromElm: PortFromElm<FromElm>;
    interopToElm: PortToElm<ToElm>;
    [key: string]: UnknownPort;
  };
}

export type FromElm = { data : { url : string }; tag : "OpenTab" } | { data : { url : string }; tag : "CopyPicture" } | { data : { name : string; text : string; url : string }; tag : "SharePicture" } | { data : { id : number; url : string }; tag : "DeletePicture" } | { data : { height : number; model : string; prompt : string; seed : number; source : string; steps : number; width : number }; tag : "SavePicture" } | { tag : "LoadLibrary" } | { tag : "ClearModelCache" } | { tag : "ReleaseModel" } | { tag : "CancelPipeline" } | { data : { frameBudget : number; guidanceScale : number; height : number; numInferenceSteps : number; prompt : string; seed : number; width : number }; tag : "GenerateImage" } | { data : { cache : boolean; force : boolean; model : string }; tag : "LoadModel" } | { tag : "CheckWebGpu" } | { data : { challenge : string; rpId : string }; tag : "GetPasskey" } | { data : { algorithms : number[]; challenge : string; excludeCredentials : string[]; rp : { id : string; name : string }; user : { displayName : string; id : string; name : string } }; tag : "CreatePasskey" } | { data : { key : string; value : string }; tag : "WriteToLocalStorage" };

export type ToElm = { dark : boolean; tag : "SystemThemeChanged" } | { attestationObject : string; clientDataJSON : string; tag : "PasskeyCreated"; transports : string[] } | ({ tag : "PasskeyUsed" } & ({ id : string } & { clientDataJSON : string } & { authenticatorData : string } & { signature : string })) | { error : string; tag : "PasskeyFailed" } | { supported : boolean; tag : "WebGpuChecked" } | ({ tag : "ModelProgress" } & ({ component : "text_encoder" | "transformer" | "vae" } & { loaded : number } & { total : number } & { fromCache : boolean })) | { tag : "ModelLoaded" } | { step : number; tag : "GenerationStep" } | { tag : "ImageGenerated"; url : string } | { tag : "ModelReleased" } | { tag : "ModelCacheCleared" } | { message : string; name : string; tag : "PipelineFailed" } | { pictures : ({ id : number } & { url : string } & { prompt : string } & { width : number } & { height : number } & { steps : number } & { seed : number } & { model : string })[]; tag : "LibraryLoaded" } | { picture : ({ id : number } & { url : string } & { prompt : string } & { width : number } & { height : number } & { steps : number } & { seed : number } & { model : string }); source : string; tag : "PictureSaved" } | { id : number; tag : "PictureDeleted" } | { message : string; tag : "LibraryFailed" } | { tag : "PictureCopied" } | { name : string; tag : "ShareFailed" };

export type Flags = ({ theme : ("light" | "dark" | "system") | null } & { systemDark : boolean } & { entropy : number } & { models : string | null } & { canShare : boolean });

export namespace Main {
  function init(options: { node?: HTMLElement | null; flags: Flags }): ElmApp;
}

export as namespace Elm;

export { Elm };

export type UnknownPort = PortFromElm<unknown> | PortToElm<unknown> | undefined;

export type PortFromElm<Data> = {
  subscribe(callback: (fromElm: Data) => void): void;
  unsubscribe(callback: (fromElm: Data) => void): void;
};

export type PortToElm<Data> = { send(data: Data): void };
