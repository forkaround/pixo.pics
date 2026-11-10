import { Elm, type Flags, type FromElm, type ToElm } from "./Main.elm";

type PasskeyOptions = Extract<FromElm, { tag: "CreatePasskey" }>["data"];
type SignInOptions = Extract<FromElm, { tag: "GetPasskey" }>["data"];

//-- FLAGS

const media = window.matchMedia("(prefers-color-scheme: dark)");
const getStoredTheme = () => {
  try {
    return (localStorage.getItem("theme") as Flags["theme"]) ?? null;
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
    theme: getStoredTheme(),
    systemDark: media.matches,
  },
});

//-- PORTS:SEND

media.addEventListener("change", (event) =>
  app.ports.interopToElm.send({
    tag: "SystemThemeChanged",
    dark: event.matches,
  }),
);

//-- PORTS:SUBSCRIBE

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

    default:
      return msg satisfies never;
  }
});
