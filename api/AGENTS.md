# Server

The Elm HTTP server in `api/`: flows, answers, queries, emails and their tests.

## Structure

- `Main` is wiring. Each flow is a module in `api/src/Flow/` with its own `Msg`, a
  function per route that returns a `Reply Msg`, and an `update`. Flows never import
  `Main` or each other.
- A server that can't start properly (bad flags or origin, no database, no mailer on
  Vercel) is `Broken`, not down: it answers 503 to every route but health, which fails
  with the reason, and still answers 404 and 405.
- Answers are built only in `Response.elm`; the `session` cookie is read, set and cleared
  only in `SessionCookie.elm`.
- `origin` and `local` come from `app.ts` (Vercel's variables), never from the request.
  A passkey's `rpId` is the origin's host, so a passkey belongs to one host:
  production's, a preview's own, or `localhost`. `local` picks the log mailer: a log
  mailer in production would write link tokens to the logs.
- Secrets stay out of logs: emails link to `<origin>/verify#<token>`, in the fragment,
  and the verify routes take the token as `authorization: Bearer <token>`. A refusal's
  reason (WebAuthn, sign-in) and a database error are only logged; the answer is a short
  public text, since Postgres messages can name roles and tables.
- A `SessionToken` and a `LinkToken` are different types, and `Db` takes an `Email` or a
  `Username` only parsed, so an unchecked value doesn't compile. Request bodies stay raw
  strings, so the server can tell a 400 (not that JSON) from a 422 (refused).
- Nothing may run after the answer on Vercel: a route that sends an email waits for
  Brevo before answering.
- `log Interop.Info|Warn|Error message`: `app.ts` calls `console[level]`, which Vercel
  reads as the level.
- `Effect.Http` sends a `JsonBody`, a `FormBody` (`application/x-www-form-urlencoded`) or an
  `EmptyBody`. The server never sends picture bytes: uploads use presigned links.
- `Hmac` and `SigV4` sign requests in Elm. `SigV4.sign` returns the headers to send and never a
  `host` header: `fetch` sets it from the URL. Never sign `content-type` on a request with a body:
  `elm/http` sets it from the body, and a second one would break the signature.
  `SigV4.presign` makes a short-lived link (query signing) that the browser can use itself.
- Elm can't check a signature, so `app.ts` does, with WebCrypto, and nothing else:
  `VerifySignature`, answered by `SignatureChecked` with the `signIn` it was given, as
  the model keeps nothing between the two. Attestation is `none`: nothing proves which
  authenticator made a key.
- `Aaguid.name` holds the names from
  [passkey-authenticator-aaguids](https://github.com/passkeydeveloper/passkey-authenticator-aaguids)'s
  `aaguid.json`, copied on 2026-11-10.
- An email template is a module in `api/src/Services/Mailer/` that lists `Layout` blocks;
  `Layout.render` writes the HTML and the text, escaping every value: never build HTML in
  a template. A `Mailer` is a record of functions: never compare two with `==`. A new
  provider is a new module with a `mailer`.

## Adding a route

1. A variant of `Api.Route` in `shared/Api.elm`: keep `Api.parse` in step with
   `Api.method` and `Api.path`.
2. A branch in `Main.handle`, and one in `brokenAnswer`.
3. A function in its flow (`api/src/Flow/`) that returns a `Reply Msg`. A new flow is a
   new module and a new `Main.Msg` variant.
4. In `tests/ApiTest.elm`, which walks the routes with `next`: chain the new route after
   the last one.

## Rate limiting

- Limits live in `Main`'s model, never in the database: a burst must not become a burst
  of writes. Fluid Compute keeps an instance, and the Elm program in it, between
  requests, so the table catches bursts; a cold start, a deployment or another instance
  starts empty, and that is accepted.
- One policy per key, each a function in `api/src/RateLimit.elm` that takes what it limits
  (`ClientAddress`, `Email`); a key is written nowhere else. Name it after what it limits
  and whom, like `magic-link:email:<email>`.
- `Main` counts every request against `RateLimit.requests` before routing it. A flow
  limits its route further with `Reply.Limit policies next`: every policy is counted or
  none is, and a refusal answers 429 with the longest wait, before any query.
- The time comes from `Effect.Clock`, in epoch milliseconds: never `//` or `modBy` on
  it, since Elm's integer division truncates to 32 bits.

## Queries

- **SQL values are parameters**: never `++` a request value into SQL; pass `$n` with
  `Neon.text`, `Neon.int`... Only constants may be joined in. An identifier that depends
  on a request is picked from a fixed list.
- Read `migrations/AGENTS.md` before writing a query or a SQL function: one statement per
  check-then-change, row-level security, functions for work across users.
- No answer tells whether an email has an account: sign-up checks the username first, an
  email that has a user gets a sign-in link instead, and one without gets no email and
  the same 202.
- WebAuthn values are unpadded base64url text, as on the wire. `sign_count` is a `bigint`:
  it arrives as text.

### Neon driver

- `Services/Neon.elm` uses `elm/http`, so `app.ts` imports `xhr.ts` first: Bun, Node and
  Vercel have no `XMLHttpRequest`.
- Rows are JSON objects by column name. `boolean`, `smallint`, `integer`, `real`,
  `double precision`, `json`, `jsonb` and arrays arrive as JSON; `NULL` as `null`;
  everything else (`bigint`, `numeric`, `timestamptz`...) as Postgres's text. Duplicate
  column names keep the last: use `AS`.
- Parameters travel as text and Postgres converts them; `Neon.json` for `json`/`jsonb`.

## Vercel functions

- **Vercel functions**: every `.ts` under `api/` becomes one unless its name or folder
  starts with `_`. Server TypeScript lives in `api/_ts`; `app.ts` uses Web APIs only
  (`crypto.subtle`, `atob`: their types come from `"WebWorker"` in `api/tsconfig.json`).

## Tests

- Server: the first argument of `request` stands for the Promise's `resolve` and comes
  back in the response. Neon and Brevo are the only HTTP calls: answer them with
  `simulateHttpOk`. `advanceTime` moves the clock that latency and rate limits are measured with.
- A `Neon.Request` and a mailer's `Http` effect are records: test `.body` and
  `.onResponse` with no program.
- A new email template gets a test that its values reach the HTML and the text.
- `api/tests/WebAuthnTest.elm` uses fixtures written by another encoder, byte by byte,
  for `https://pixo.pics`: one per flag or fault.
