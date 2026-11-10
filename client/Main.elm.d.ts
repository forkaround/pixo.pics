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

export type FromElm = { data : { challenge : string; rpId : string }; tag : "GetPasskey" } | { data : { algorithms : number[]; challenge : string; excludeCredentials : string[]; rp : { id : string; name : string }; user : { displayName : string; id : string; name : string } }; tag : "CreatePasskey" } | { data : { key : string; value : string }; tag : "WriteToLocalStorage" };

export type ToElm = { dark : boolean; tag : "SystemThemeChanged" } | { attestationObject : string; clientDataJSON : string; tag : "PasskeyCreated"; transports : string[] } | ({ tag : "PasskeyUsed" } & ({ id : string } & { clientDataJSON : string } & { authenticatorData : string } & { signature : string })) | { error : string; tag : "PasskeyFailed" };

export type Flags = { systemDark : boolean; theme : ("light" | "dark" | "system") | null };

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
