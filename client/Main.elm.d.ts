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

export type FromElm = { data : { model : string }; tag : "RemoveStylist" } | { tag : "StopWriting" } | { data : { maxTokens : number; messages : { content : string; role : "assistant" | "user" | "system" }[]; model : string; temperature : number; thinking : boolean; topP : number }; tag : "WritePrompt" } | { data : { model : string }; tag : "LoadStylist" } | { data : { url : string }; tag : "OpenTab" } | { data : { url : string }; tag : "CopyPicture" } | { data : { name : string; text : string; url : string }; tag : "SharePicture" } | { data : { urls : string[] }; tag : "IdbRelease" } | { data : { database : { name : string; stores : { autoIncrement : boolean; keyPath : string; name : string }[]; version : number }; key : JsonValue; label : string; store : string }; tag : "IdbDelete" } | { data : { blobs : { field : string; url : string }[]; database : { name : string; stores : { autoIncrement : boolean; keyPath : string; name : string }[]; version : number }; label : string; row : { [key: string]: JsonValue }; store : string }; tag : "IdbAdd" } | { data : { database : { name : string; stores : { autoIncrement : boolean; keyPath : string; name : string }[]; version : number }; label : string; store : string }; tag : "IdbGetAll" } | { tag : "ClearModelCache" } | { tag : "ReleaseModel" } | { tag : "CancelPipeline" } | { data : { frameBudget : number; guidanceScale : number; height : number; numInferenceSteps : number; prompt : string; seed : number; width : number }; tag : "GenerateImage" } | { data : { cache : boolean; force : boolean; model : string }; tag : "LoadModel" } | { tag : "CheckWebGpu" } | { data : { challenge : string; rpId : string }; tag : "GetPasskey" } | { data : { algorithms : number[]; challenge : string; excludeCredentials : string[]; rp : { id : string; name : string }; user : { displayName : string; id : string; name : string } }; tag : "CreatePasskey" } | { data : { key : string; value : string }; tag : "WriteToLocalStorage" };

export type ToElm = { dark : boolean; tag : "SystemThemeChanged" } | { attestationObject : string; clientDataJSON : string; tag : "PasskeyCreated"; transports : string[] } | ({ tag : "PasskeyUsed" } & ({ id : string } & { clientDataJSON : string } & { authenticatorData : string } & { signature : string })) | { error : string; tag : "PasskeyFailed" } | { supported : boolean; tag : "WebGpuChecked" } | ({ tag : "ModelProgress" } & ({ component : "text_encoder" | "transformer" | "vae" } & { loaded : number } & { total : number } & { fromCache : boolean })) | { tag : "ModelLoaded" } | { step : number; tag : "GenerationStep" } | { tag : "ImageGenerated"; url : string } | { tag : "ModelReleased" } | { tag : "ModelCacheCleared" } | { message : string; name : string; tag : "PipelineFailed" } | { label : string; rows : JsonValue[]; tag : "IdbRows" } | { label : string; row : JsonValue; tag : "IdbAdded" } | { key : JsonValue; label : string; tag : "IdbDeleted" } | { label : string; message : string; tag : "IdbFailed" } | { progress : number; tag : "StylistProgress" } | { tag : "StylistLoaded" } | { tag : "PromptStreamed"; text : string } | { tag : "PromptWritten"; text : string } | { tag : "StylistRemoved" } | { message : string; name : string; tag : "StylistFailed" } | { tag : "PictureCopied" } | { name : string; tag : "ShareFailed" };

export type Flags = ({ theme : ("light" | "dark" | "system") | null } & { systemDark : boolean } & { entropy : number } & { models : string | null } & { stylist : string | null } & { canShare : boolean } & { sidebar : ("expanded" | "collapsed") | null });

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
