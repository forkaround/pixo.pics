import "./UI/elmcn-dialog";
import { Elm, type Flags, type FromElm, type ToElm } from "./Main.elm";
import { idb, release } from "./idb";
import type { Command, Reply } from "./pipeline.worker";
import type { Command as StylistCommand, Reply as StylistReply } from "./stylist.worker";
import { canShare, copyPicture, sharePicture } from "./share";

type PasskeyOptions = Extract<FromElm, { tag: "CreatePasskey" }>["data"];
type SignInOptions = Extract<FromElm, { tag: "GetPasskey" }>["data"];

//-- FLAGS

const media = window.matchMedia("(prefers-color-scheme: dark)");
const stored = (key: string) => {
  try {
    return localStorage.getItem(key);
  } catch {
    return null;
  }
};

const fromBase64Url = (value: string) =>
  Uint8Array.from(atob(value.replace(/-/g, "+").replace(/_/g, "/")), (char) => char.charCodeAt(0));

const toBase64Url = (buffer: ArrayBuffer) =>
  btoa(String.fromCharCode(...new Uint8Array(buffer)))
    .replace(/\+/g, "-")
    .replace(/\//g, "_")
    .replace(/=+$/, "");

const createPasskey = async (options: PasskeyOptions): Promise<ToElm> => {
  try {
    const credential = await navigator.credentials.create({
      publicKey: {
        challenge: fromBase64Url(options.challenge),
        rp: options.rp,
        user: { ...options.user, id: new TextEncoder().encode(options.user.id) },
        pubKeyCredParams: options.algorithms.map((alg) => ({ type: "public-key", alg })),
        excludeCredentials: options.excludeCredentials.map((id) => ({
          type: "public-key",
          id: fromBase64Url(id),
        })),
        authenticatorSelection: { residentKey: "required", userVerification: "preferred" },
        attestation: "none",
      },
    });
    if (
      !(credential instanceof PublicKeyCredential) ||
      !(credential.response instanceof AuthenticatorAttestationResponse)
    ) {
      return { tag: "PasskeyFailed", error: "NotSupportedError" };
    }
    return {
      tag: "PasskeyCreated",
      clientDataJSON: toBase64Url(credential.response.clientDataJSON),
      attestationObject: toBase64Url(credential.response.attestationObject),
      transports: credential.response.getTransports(),
    };
  } catch (error) {
    return {
      tag: "PasskeyFailed",
      error: error instanceof Error ? error.name : "NotSupportedError",
    };
  }
};

const getPasskey = async (options: SignInOptions): Promise<ToElm> => {
  try {
    const credential = await navigator.credentials.get({
      publicKey: {
        challenge: fromBase64Url(options.challenge),
        rpId: options.rpId,
        userVerification: "preferred",
      },
    });
    if (
      !(credential instanceof PublicKeyCredential) ||
      !(credential.response instanceof AuthenticatorAssertionResponse)
    ) {
      return { tag: "PasskeyFailed", error: "NotSupportedError" };
    }
    return {
      tag: "PasskeyUsed",
      id: toBase64Url(credential.rawId),
      clientDataJSON: toBase64Url(credential.response.clientDataJSON),
      authenticatorData: toBase64Url(credential.response.authenticatorData),
      signature: toBase64Url(credential.response.signature),
    };
  } catch (error) {
    return {
      tag: "PasskeyFailed",
      error: error instanceof Error ? error.name : "NotSupportedError",
    };
  }
};

//-- INIT

const app = Elm.Main.init({
  flags: {
    theme: stored("theme") as Flags["theme"],
    systemDark: media.matches,
    entropy: crypto.getRandomValues(new Uint32Array(1))[0] ?? 0,
    models: stored("models"),
    stylist: stored("stylist"),
    canShare: canShare(),
    sidebar: stored("sidebar") as Flags["sidebar"],
    webGpu: "gpu" in navigator,
  },
});

const send = (msg: ToElm) => app.ports.interopToElm.send(msg);

//-- IMAGE PIPELINE PORTS

const worker = new Worker(new URL("./pipeline.worker.ts", import.meta.url), { type: "module" });
const pipeline = (cmd: Command) => worker.postMessage(cmd);

worker.addEventListener("error", (e) =>
  send({
    tag: "PipelineFailed",
    name: "Error",
    message: e.message || "Worker unable to start",
  }),
);

worker.addEventListener("message", ({ data: msg }: MessageEvent<Reply>) => {
  switch (msg.tag) {
    case "ImageGenerated":
      return send({ tag: "ImageGenerated", url: URL.createObjectURL(msg.picture) });

    default:
      return send(msg);
  }
});

//-- PROMPT WRITER PORTS

const startStylist = () => {
  const started = new Worker(new URL("./stylist.worker.ts", import.meta.url), { type: "module" });
  started.addEventListener("error", (e) =>
    send({
      tag: "StylistFailed",
      name: "Error",
      message: e.message || "Worker unable to start",
    }),
  );
  started.addEventListener("message", ({ data: msg }: MessageEvent<StylistReply>) => send(msg));
  return started;
};

let stylistWorker: Worker | null = null;
const stylist = (cmd: StylistCommand) => (stylistWorker ??= startStylist()).postMessage(cmd);

//-- THEME PORTS

media.addEventListener("change", (event) =>
  app.ports.interopToElm.send({
    tag: "SystemThemeChanged",
    dark: event.matches,
  }),
);

//-- SUBSCRIBE

app.ports.interopFromElm.subscribe((msg) => {
  switch (msg.tag) {
    case "WriteToLocalStorage":
      try {
        return localStorage.setItem(msg.data.key, msg.data.value);
      } catch {
        return;
      }

    case "CreatePasskey":
      return void createPasskey(msg.data).then((result) => app.ports.interopToElm.send(result));

    case "GetPasskey":
      return void getPasskey(msg.data).then((result) => app.ports.interopToElm.send(result));

    case "CheckWebGpu":
    case "LoadModel":
    case "GenerateImage":
    case "CancelPipeline":
    case "ReleaseModel":
    case "ClearModelCache":
      return void pipeline(msg);

    case "LoadStylist":
    case "WritePrompt":
    case "StopWriting":
    case "RemoveStylist":
      return void stylist(msg);

    case "IdbGetAll":
    case "IdbAdd":
    case "IdbDelete":
      return void idb(msg).then(send);

    case "IdbRelease":
      return release(msg.data.urls);

    case "SharePicture":
      return void sharePicture(msg.data).then((reply) => reply && send(reply));

    case "CopyPicture":
      return void copyPicture(msg.data).then(send);

    case "OpenTab":
      return void window.open(msg.data.url, "_blank", "noopener,noreferrer");

    default:
      return msg satisfies never;
  }
});
