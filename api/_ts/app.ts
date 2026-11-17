import "./xhr";
import { Elm, type FromElm, type JsonValue } from "../src/Main.elm";

//-- RESOLVERS

type Resolve = (response: Response) => void;
const toResolver = (resolve: Resolve) => resolve as unknown as JsonValue;
const fromResolver = (resolver: JsonValue) => resolver as unknown as Resolve;

//-- SIGNATURES

type Key = Extract<FromElm, { tag: "VerifySignature" }>["key"];

const fromBase64Url = (value: string) =>
  Uint8Array.from(atob(value.replace(/-/g, "+").replace(/_/g, "/")), (char) => char.charCodeAt(0));

const importKey = (key: Key) => {
  switch (key.tag) {
    case "ES256":
      return crypto.subtle.importKey(
        "jwk",
        { kty: "EC", crv: "P-256", x: key.x, y: key.y },
        { name: "ECDSA", namedCurve: "P-256" },
        false,
        ["verify"],
      );

    case "EdDSA":
      return crypto.subtle.importKey(
        "jwk",
        { kty: "OKP", crv: "Ed25519", x: key.x },
        "Ed25519",
        false,
        ["verify"],
      );

    case "RS256":
      return crypto.subtle.importKey(
        "jwk",
        { kty: "RSA", n: key.n, e: key.e },
        { name: "RSASSA-PKCS1-v1_5", hash: "SHA-256" },
        false,
        ["verify"],
      );

    default:
      return key satisfies never;
  }
};

const verifySignature = async (key: Key, signed: string, signature: string) => {
  try {
    const cryptoKey = await importKey(key);
    return await crypto.subtle.verify(
      key.tag === "ES256" ? { name: "ECDSA", hash: "SHA-256" } : cryptoKey.algorithm,
      cryptoKey,
      fromBase64Url(signature),
      fromBase64Url(signed),
    );
  } catch {
    return false;
  }
};

const fromHex = (value: string) =>
  Uint8Array.from(value.match(/../g) ?? [], (pair) => parseInt(pair, 16));

const verifyWebhook = async (secret: string, signed: string, signatures: string[]) => {
  try {
    const key = await crypto.subtle.importKey(
      "raw",
      new TextEncoder().encode(secret),
      { name: "HMAC", hash: "SHA-256" },
      false,
      ["verify"],
    );
    const data = new TextEncoder().encode(signed);
    for (const signature of signatures) {
      if (await crypto.subtle.verify("HMAC", key, fromHex(signature), data)) return true;
    }
    return false;
  } catch {
    return false;
  }
};

//-- INIT

const local = process.env.VERCEL ? null : "local";

const commitUrl = () => {
  const {
    VERCEL_GIT_PROVIDER,
    VERCEL_GIT_REPO_OWNER,
    VERCEL_GIT_REPO_SLUG,
    VERCEL_GIT_COMMIT_SHA,
  } = process.env;
  return VERCEL_GIT_PROVIDER === "github" &&
    VERCEL_GIT_REPO_OWNER &&
    VERCEL_GIT_REPO_SLUG &&
    VERCEL_GIT_COMMIT_SHA
    ? `https://github.com/${VERCEL_GIT_REPO_OWNER}/${VERCEL_GIT_REPO_SLUG}/commit/${VERCEL_GIT_COMMIT_SHA}`
    : null;
};

const databaseUrl = () => {
  const { DATABASE_URL, API_DATABASE_PASSWORD } = process.env;
  if (!DATABASE_URL || !API_DATABASE_PASSWORD || !URL.canParse(DATABASE_URL)) return null;
  const url = new URL(DATABASE_URL);
  url.username = "api";
  url.password = API_DATABASE_PASSWORD;
  return url.href;
};

const origin = () => {
  const { VERCEL_ENV, VERCEL_PROJECT_PRODUCTION_URL, VERCEL_URL } = process.env;
  const host = VERCEL_ENV === "production" ? VERCEL_PROJECT_PRODUCTION_URL : VERCEL_URL;
  return host ? `https://${host}` : "http://localhost:5173";
};

const app = Elm.Main.init({
  flags: {
    databaseUrl: databaseUrl(),
    neonEndpoint: process.env.NEON_ENDPOINT ?? null,
    brevoApiKey: process.env.BREVO_API_KEY || null,
    emailFrom: process.env.EMAIL_FROM || null,
    origin: origin(),
    releaseId: process.env.VERCEL_GIT_COMMIT_SHA || process.env.VERCEL_DEPLOYMENT_ID || local,
    releaseUrl: commitUrl(),
    region: process.env.VERCEL_REGION || local,
    local: local !== null,
    paddleEnvironment: process.env.PADDLE_ENVIRONMENT || null,
    paddleApiKey: process.env.PADDLE_API_KEY || null,
    paddleWebhookSecret: process.env.PADDLE_WEBHOOK_SECRET || null,
    paddleClientToken: process.env.PADDLE_CLIENT_TOKEN || null,
    paddleProPriceId: process.env.PADDLE_PRO_PRICE_ID || null,
    r2AccountId: process.env.R2_ACCOUNT_ID || null,
    r2AccessKeyId: process.env.R2_ACCESS_KEY_ID || null,
    r2SecretAccessKey: process.env.R2_SECRET_ACCESS_KEY || null,
    r2Bucket: process.env.R2_BUCKET || null,
    r2UploadBucket: process.env.R2_UPLOAD_BUCKET || null,
    imagesOrigin: process.env.IMAGES_ORIGIN || null,
    moderationEmail: process.env.MODERATION_EMAIL || null,
  },
});

//-- PORTS:SUBSCRIBE

app.ports.interopFromElm.subscribe((msg) => {
  switch (msg.tag) {
    case "Respond":
      return fromResolver(msg.resolve)(
        new Response(msg.body, {
          status: msg.status,
          headers: msg.headers,
        }),
      );

    case "Log":
      return console[msg.level](msg.message);

    case "VerifySignature":
      return void verifySignature(msg.key, msg.signed, msg.signature).then((valid) =>
        app.ports.interopToElm.send({
          tag: "SignatureChecked",
          resolve: msg.resolve,
          signIn: msg.signIn,
          valid,
        }),
      );

    case "VerifyWebhook":
      return void verifyWebhook(msg.secret, msg.signed, msg.signatures).then((valid) =>
        app.ports.interopToElm.send({
          tag: "WebhookChecked",
          resolve: msg.resolve,
          body: msg.body,
          valid,
        }),
      );

    default:
      return msg satisfies never;
  }
});

//-- HANDLER

export async function handle(req: Request): Promise<Response> {
  const url = new URL(req.url);
  const request = {
    method: req.method,
    path: url.pathname.split("/").filter(Boolean).map(decodeURIComponent),
    query: Object.fromEntries(url.searchParams),
    headers: Object.fromEntries(req.headers),
    body: await req.text(),
  };

  return new Promise<Response>((resolve) =>
    app.ports.interopToElm.send({
      tag: "Request",
      resolve: toResolver(resolve),
      request,
    }),
  );
}
