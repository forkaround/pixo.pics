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

export type FromElm = { key : { e : string; n : string; tag : "RS256" } | { tag : "EdDSA"; x : string } | { tag : "ES256"; x : string; y : string }; resolve : JsonValue; signIn : { backedUp : boolean; challenge : string; credentialId : string; signCount : number }; signature : string; signed : string; tag : "VerifySignature" } | { level : "error" | "warn" | "info"; message : string; tag : "Log" } | { body : string; headers : [ string, string ][]; resolve : JsonValue; status : number; tag : "Respond" };

export type ToElm = { request : ({ method : string } & { path : string[] } & { query : { [key: string]: string } } & { headers : { [key: string]: string } } & { body : string }); resolve : JsonValue; tag : "Request" } | { resolve : JsonValue; signIn : ({ challenge : string } & { credentialId : string } & { signCount : number } & { backedUp : boolean }); tag : "SignatureChecked"; valid : boolean };

export type Flags = ({ databaseUrl : string | null } & { neonEndpoint : string | null } & { brevoApiKey : string | null } & { emailFrom : string | null } & { origin : string } & { releaseId : string | null } & { releaseUrl : string | null } & { region : string | null });

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
