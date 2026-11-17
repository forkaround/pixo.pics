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

## Billing

- Paddle is the source of truth for Pro. `POST /api/billing/webhook` mirrors each
  `subscription.*` event into `subscriptions` through `apply_paddle_subscription`, which
  only moves a row to a newer `updated_at` and never changes its owner.
- `app.ts` checks the `Paddle-Signature` HMAC with WebCrypto (`VerifyWebhook`, answered by
  `WebhookChecked` with the body it was given). Elm parses the header, refuses a timestamp
  more than 5 s away, and keeps only 64-hex signatures.
- Only the configured Pro price grants Pro. The account is `custom_data.user_id`.
- Any non-2xx makes Paddle retry (3 days in live). Events we choose to skip answer 200.
  The body is never logged.
- The webhook route has its own rate limit (`RateLimit.webhooks`), not `requests`.
- Pro is `is_pro(user)`: a subscription that is `active`, `trialing` or `past_due`.
- Checkout: the server creates the transaction (price and `custom_data.user_id` are never the
  browser's) and the browser opens Paddle.js with its id and the public client token. The app
  never trusts the checkout's own `completed` event: it polls `GET /api/auth/session` with
  growing waits.
- Paddle's default payment link is `/settings/account`. Paddle appends `?_ptxn=<transaction>`
  to it in its emails (payment-method updates) and in every transaction's `checkout.url`; the
  browser opens that checkout on any page, signed in or not.
- Billing travels with the session (`Api.Me`): no route of its own, so a page load costs one
  request.
- An account with a renewing subscription can't be deleted (`'renewing'`, 409): cancel first.

## Cloud sync

- A Pro user connects Dropbox or Google Drive once; the server keeps the refresh token,
  encrypted in `sync_connections` with `pgp_sym_encrypt` under `SYNC_TOKEN_KEY`, and the
  signed-in browser asks for a short-lived access token whenever it syncs. Rotating the key
  means re-encrypting every row or reconnecting everyone. The key is a query parameter, never
  part of the SQL text.
- The authorization code flow uses PKCE (S256) and a `state` that is single use, bound to the
  user and valid for 10 minutes: Postgres makes both, and `finishSync` checks and spends the
  state in one `DELETE ... RETURNING`. Scopes are the narrowest: Dropbox App folder, Google
  `drive.file`.
- Access tokens go only to the signed-in Pro owner and are never logged, nor are refresh
  tokens, codes, verifiers or the key. Connections travel with the session (`Api.Me`).
  Disconnecting revokes the token at the provider, then forgets it; a failed revoke is only
  logged.

## Pictures

- Publishing never sends picture bytes through the server. `POST /api/pictures` checks the
  metadata and declared size, inserts the row only for a Pro user with fewer than 3 unfinished
  publishes, and answers a 10-minute presigned `PUT` for `incoming/<id>.jpg` in the private
  upload bucket, with `content-type: image/jpeg` signed (R2 enforces it).
- `POST /api/pictures/<id>/published` `HEAD`s the staged file, refuses a size other than the
  declared one (so over 2 MiB) or a type other than `image/jpeg`, copies it into the public
  bucket with `REPLACE` metadata, deletes the staged file, then sets `published_at`. Only rows
  with `published_at` are ever shown.
- Unpublishing and account deletion delete the public file before the row, one file at a time.
  The upload bucket's lifecycle rule removes anything staged and never confirmed.
- A picture's tags (at most 8 `Data.Tag`s, from the library's tagger) are stored in
  `published_pictures.tags`, a `text[]` with a GIN index, so the feed can filter by one.
- A presigned URL is a credential until it expires: never log one.
- `removed_at` is for moderation; `api` can't write it.
- The feed, the top tags and profiles read through `feed_pictures` (optionally by one tag),
  `feed_tags` and `profile`, which return only public columns and skip removed pictures and
  owners who aren't Pro: a lapsed subscription hides, never deletes. Their answers are the
  same for every visitor and cached by the CDN for a minute (`Response.shared`): never put
  anything about the visitor in them, and never read or set a cookie on those routes.
- Reports store a reason and a time, never who reported. `take_down_picture` and
  `dismiss_reports` are the owner's, run by hand; `api` can't execute them.

## Adding a route

1. A variant of `Api.Route` in `shared/Api.elm`: keep `Api.parse` in step with
   `Api.method` and `Api.path`.
2. A branch in `Main.handle`, one in `brokenAnswer`, and one in `routePolicy` (its rate
   limit: `RateLimit.requests` unless it needs its own).
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
- `Main` counts every request against its route's policy before routing it
  (`routePolicy`: `RateLimit.requests`, or `RateLimit.webhooks` for Paddle's webhook; an
  unknown path counts against `requests`). A flow
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
