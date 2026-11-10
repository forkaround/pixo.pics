# AGENTS.md

A Vite app, run by Bun: an Elm browser app and an Elm HTTP server sharing one API contract,
on Postgres (Neon), deployed to Vercel.

## Layout

| Path               | What                                                                             |
| ------------------ | -------------------------------------------------------------------------------- |
| `client/`          | Browser app (root `elm.json`). `client/UI/`: elmcn components                    |
| `shared/`          | Elm both sides compile: `Api` (routes, endpoints, codecs)                        |
| `tests/`           | elm-test suites for `client/` and `shared/` (see [Tests](#tests))                |
| `api/tests/`       | elm-test suites for the server                                                   |
| `api/src/`         | Server Elm (`api/elm.json`): `Main`, `Db`, `Neon`, `Brevo`, `WebAuthn`, `Aaguid` |
| `api/src/Email/`   | Email templates (see [Email](#email))                                            |
| `api/_ts/`         | Server TypeScript: `app.ts` (starts Elm), `xhr.ts`, `dev.ts` (Vite plugin)       |
| `api/index.ts`     | The only Vercel Function                                                         |
| `migrations/`      | Graphile Migrate (`.gmrc`)                                                       |
| `.scripts/neon.sh` | Starts the local Neon proxy                                                      |
| `.agents/`         | Agent skills, tracked by `skills-lock.json`; `.claude` links here                |

## Commands

```sh
bun dev                 # types + dev:proxy + dev:vite (:5173, Elm server on /api)
bun run dev:proxy       # local Neon proxy in Docker, then SELECT 1 with DATABASE_URL
bun run build           # types:api, types:app, check, then vite build (server, then client)
bun run preview         # the build on :4173
bun run types           # regenerate *.elm.d.ts after changing an InteropDefinitions.elm
bun check               # type-check both TypeScript projects
bun run test            # test:app (tests/), then test:api (api/tests/); not `bun test`
bun fmt                 # format: fmt:bun and fmt:elm (elm-format), in parallel
bun fmt-check           # report what bun fmt would change, and change nothing
bun db <command>        # graphile-migrate: migrate, watch, commit, uncommit, status, reset --erase
```

CI (`.github/workflows/ci.yml`) runs `bun fmt-check`, `bun run test` and `bun run build`, on
pull requests and on `main`; it needs no database.

## Rules

- **Bun, not npm**: `bun add -d <pkg>`.
- **No comments** in code or config. Exceptions: tool directives
  (`// @ts-expect-error`, `/// <reference ... />`) and generated or signed files (`.gmrc`,
  `*.elm.d.ts`, committed migrations).
- **Elm is formatted by elm-format**: the `fmt` script replaces Bun's `bun fmt` and runs
  `fmt:bun` (Bun's formatter) and `fmt:elm` (`elm-format` over `client`, `shared`,
  `api/src` and both `tests` folders) in parallel. A new Elm folder goes in `fmt:elm` and
  `fmt-check:elm`. A multi-line `exposing` list comes out one name per line, sorted:
  keeping groups takes `@docs` in a doc comment, which the rule above excludes.
- **`bun fmt-check`, not `bun fmt --check`**: `bun run --parallel` takes `--check` for one
  more script to run, so `bun fmt --check` formats in place, then fails. For Bun's flags
  (a path, `--staged`...), run `bun fmt --no-script <flags>`.
- **`.env.example` mirrors `.env`**: same variables, same order, changed in the same
  commit, with values that work on a fresh checkout and never a secret. Vercel-only
  variables (`DATABASE_URL_UNPOOLED`) stay out of both.
- **SQL values are parameters**: never `++` a request value into SQL; pass `$n` with
  `Neon.text`, `Neon.int`... Only constants may be joined in. An identifier that depends
  on a request is picked from a fixed list.

  ```elm
  Neon.execute ("UPDATE users SET name = '" ++ name ++ "'") []   -- injectable
  Neon.execute "UPDATE users SET name = $1" [ Neon.text name ]    -- safe
  ```

- **Migrations**: write `migrations/current.sql` to rerun cleanly: undo, then redo (see
  [Migrations](#migrations)). Never edit `migrations/committed/`: they are hash-signed.
- **Agents never commit a migration**: no `bun db commit`. Run `current.sql` with
  `bun db watch --once`, then ask for a review: the user commits.
- **Every table has row-level security**: `ENABLE` and `FORCE`, its policies and the
  narrowest grants to `api`, right after its `CREATE TABLE` (see
  [Row-level security](#row-level-security)). `migrations/check-rls.sql` fails the
  migration otherwise.
- **Keep the Elm projects apart**: the root `elm.json` never lists `api/` folders, so the
  database password can't reach the browser bundle. A package `shared/` imports goes in
  both `elm.json`s; server packages: `elm install` inside `api/`.
- **Every route is handled**: `Api.Route` is matched without a wildcard, so a new route
  doesn't compile until the server handles it. Keep `Api.parse` in step with
  `Api.method` and `Api.path`.
- **Every `FromElm` is handled**: the port handlers in `app.ts` and `client/main.ts` take
  `msg` and end `switch (msg.tag)` with a `default` that is `never`:
  `return msg satisfies never;` when `FromElm` has several variants, as in both now, but
  `return msg.tag satisfies never;` when it has one: elm-ts-interop then emits a plain
  object, not a union, and TypeScript narrows only `msg.tag`. Switch forms when the count
  crosses one.
- **UI**: use the elmcn registry component instead of writing one; Phosphor
  (`phosphor-icons/phosphor-elm`, `Regular`) instead of hand-written SVG; `sr-only` text
  on icon-only controls.
- **Elm steps in `build` run one at a time**: keep `build` as
  `bun run --sequential types:* check build:*`, never `types` (which runs both
  generators in parallel). On an empty `~/.elm`, as on every fresh Vercel machine, two
  Elm processes downloading packages at once corrupt the cache: "I ran into a
  compilation error when trying to build the following package". It's a race, so it may
  pass locally and still fail on Vercel.
- **Vercel functions**: every `.ts` under `api/` becomes one unless its name or folder
  starts with `_`. Server TypeScript lives in `api/_ts`; `app.ts` uses Web APIs only
  (`crypto.subtle`, `atob`: their types come from `"WebWorker"` in `api/tsconfig.json`).

## Commits

```
<scope>: <description>

[body]

[trailers]
```

- The scope is the area the commit touches, not its kind: no `feat`, `fix` or `chore`.
  Lower-case, one of `client`, `api`, `shared`, `db` (migrations), `docs`, `pkg`
  (dependencies, build and tool config), `agents` (`.agents/`), `vercel`; or a module
  inside one, as a path: `api/neon`, `client/ui`.
- The description is imperative and lower-case, with no final period; the whole subject
  fits in 72 characters: `db: add row-level security`.
- Several areas: the scope that holds them all, else both (`api, client: ...`), else
  `all`.
- The body says why, when the diff doesn't. A ticket or issue goes in a trailer.
- Reverts and merges keep Git's message.

## Environment

| Variable                | Used by                                                      |
| ----------------------- | ------------------------------------------------------------ |
| `DATABASE_URL`          | Migrations, as the owner; the server takes host and database |
| `API_DATABASE_PASSWORD` | Server: the password of `api`, the role it connects as       |
| `NEON_ENDPOINT`         | Server (local proxy instead of Neon), `dev:proxy`'s port     |
| `BREVO_API_KEY`         | Server: Brevo's API key; empty locally, so nothing is sent   |
| `EMAIL_FROM`            | Server: the sender's address, verified in Brevo              |
| `SHADOW_DATABASE_URL`   | `bun db watch` and `commit`                                  |
| `ROOT_DATABASE_URL`     | `bun db reset`, the proxy's own connection                   |

- Bun loads `.env`, then `.env.local`, then the shell wins, but only when it runs a file
  itself (`bun <file>`): that's why `dev:vite`, `preview` and `db` call
  `bun node_modules/...`. Plain `vite`, `bunx` or `bun --bun <bin>` run as `node` without
  `.env`, and every database route answers 503. `bun run` doesn't pass `.env` to scripts,
  so `neon.sh` reads the files itself, in the same order.
- Nothing comes from Vercel: its Development environment has no database variables.

## Local database

Needs a running Postgres on `localhost:5432` with a `postgres` superuser, and its client
tools (`psql`, `pg_dump`) on `PATH`. The app uses the `app` database, reached through
Neon's HTTP proxy in Docker at `NEON_ENDPOINT`, as production reaches Neon. One-time
setup:

```sh
cp .env.example .env
bun run dev:proxy
psql postgres://postgres@localhost:5432/postgres \
  -c "SET scram_iterations = 1" \
  -c "CREATE ROLE app LOGIN PASSWORD 'app' BYPASSRLS" \
  -c "CREATE ROLE api LOGIN PASSWORD 'api'" \
  -c "ALTER ROLE api SET search_path = public, pg_temp" \
  -c "CREATE DATABASE app OWNER app"
bun db migrate
```

- `app` owns the database and runs migrations; the server connects as `api` (see
  [Row-level security](#row-level-security)). An `app` created before this needs
  `ALTER ROLE app BYPASSRLS`.
- The proxy checks passwords, so each role needs one; one SCRAM iteration makes that ~9 ms
  instead of ~100 ms. Local only.
- `neon.sh` reuses any proxy container already on the port, or starts/creates
  `neon-http-proxy` (`--restart unless-stopped`). The proxy's own connection must be a
  superuser (`ROOT_DATABASE_URL`, `localhost` → `host.docker.internal`); with the app role
  every query fails with "Control plane request failed". It fails only without Docker or
  on a port clash, which stops `bun dev`; a failed `SELECT 1` only warns.
- One proxy serves every database on that Postgres. It holds connections open, so
  `bun db reset --erase` needs `docker stop neon-http-proxy` first. ~50 ms per query.
- `bun db migrate` creates the tables in [Schema](#schema); `/api/health` needs none.

## Migrations

- `current.sql` runs whole, again and again: `bun db watch` reruns it on every save that
  changes more than comments or whitespace, each run in one transaction (a failed run
  changes nothing), and `bun db commit` replays every committed migration on an empty
  shadow database, then runs the new one once more locally. Each run must end in the same
  state, whether the database has none of the file, all of it, or an earlier draft.
- Undo, then redo. `CREATE ... IF NOT EXISTS` is not enough: once the object exists, edits
  to its definition are skipped, and the local database drifts from what `commit` builds.

  ```sql
  DROP TABLE IF EXISTS notes CASCADE;
  CREATE TABLE notes (
    id bigint GENERATED ALWAYS AS IDENTITY PRIMARY KEY,
    body text NOT NULL
  );
  ```

  - New table, view or type: `DROP ... IF EXISTS ... CASCADE`, then `CREATE`.
  - Column on an earlier table: `ALTER TABLE t DROP COLUMN IF EXISTS c CASCADE`, then
    `ADD COLUMN`.
  - Function: `CREATE OR REPLACE FUNCTION` for a new body; new arguments or return type
    need `DROP FUNCTION IF EXISTS f(args) CASCADE` first.
  - Trigger, policy, index: `DROP TRIGGER IF EXISTS x ON t`, `DROP POLICY IF EXISTS x ON t`,
    `DROP INDEX IF EXISTS x`, then `CREATE`.
  - Enum value: `ALTER TYPE t ADD VALUE IF NOT EXISTS 'v'`. Values can't be dropped, nor
    used in the transaction that adds them: commit it alone, then use it.
  - Renames and anything else without `IF EXISTS`: a `DO` block that checks first
    (`information_schema`; enums only in `pg_catalog.pg_enum`).

    ```sql
    DO $$
    BEGIN
      IF EXISTS (
        SELECT 1 FROM information_schema.columns
        WHERE table_schema = 'public' AND table_name = 'users' AND column_name = 'username'
      ) THEN
        ALTER TABLE users RENAME COLUMN username TO name;
      END IF;
    END $$;
    ```

- The undo also runs once in every other database: undo only what this migration adds. A
  `DROP TABLE` of an earlier migration's table empties it in production.
- `CASCADE` also drops dependents elsewhere (views, foreign keys, triggers, policies):
  recreate them after the redo. `commit` dumps the shadow schema to `migrations/schema.sql`
  (`afterAllMigrations` in `.gmrc`); its diff shows what went missing.
- An object that leaves the file stays in the local database: a rename, a removal, or a
  typo that ran on save. Keep its `DROP ... IF EXISTS`; elsewhere it does nothing. Agents
  write the file with `watch` stopped, run it with `bun db watch --once`, and stop there:
  the user reviews it and runs `bun db commit`.
- Previews run only committed migrations, so commit before pushing a branch that needs
  its schema. Each migration names the previous one's hash: if `main` gains a migration
  first, `bun db uncommit`, rebase, `bun db watch --once`, `bun db commit`. Git may merge
  two migrations without a conflict (different `-m` names) and every `migrate` then
  fails. The preview's Neon branch ran the old hash; reset it:
  `bunx neon@latest branches reset preview/<git> --parent`.

## Schema

`migrations/schema.sql`, written by `bun db commit`, is the whole schema. Login is by magic
link or passkey.

| Table                 | Holds                                                        |
| --------------------- | ------------------------------------------------------------ |
| `users`               | `email` and `username`: both unique, stored lower-case       |
| `magic_links`         | `sha256` of the emailed token, its `email`, a new `username` |
| `passkeys`            | A user's WebAuthn credentials                                |
| `webauthn_challenges` | Challenges issued for a `registration` or `authentication`   |
| `sessions`            | `sha256` of the session token; `id` names it to the user     |
| `rate_limits`         | One row per key (see [Rate limiting](#rate-limiting))        |

- A `users` row means a proven email: a link carries the `email`, and consuming it creates
  the user. `users.username` is required, so a sign-up link carries the `username` too; a
  sign-in link has none and creates nobody. Its format is server policy.
- `create_sign_in_link(email, ttl_seconds)` does the same for an email that has a user
  (`sign-in`), and nothing for one that has none (`no-account`).
- `sign_out(token)` deletes the session of that token: `api` can't filter `sessions` by
  `token_hash`, which it may not read.
- `magic_link(token)` reads an unexpired link without using it: its `email`, the
  `username` (the account's, when the email has one) and `new_account`.
- `consume_magic_link(token, session_seconds)` deletes the link, creates the user of a
  sign-up link (`ON CONFLICT DO NOTHING`), starts a session and returns its token once,
  with an `outcome`: `verified`, `invalid` (used, expired or unknown) or `username-taken`
  (taken since the link was made: the link is spent, as its username can't change). A
  second sign-up link for an email that now has a user signs that user in.
- Tokens are never stored: `token_hash` is `sha256(convert_to($1, 'UTF8'))`, computed by
  Postgres from the token passed as a parameter.
- `create_sign_up_link(email, username, ttl_seconds)` makes the token (two random UUIDs,
  as unpadded base64url), stores its hash and returns it once, with an `outcome`:
  `username-taken` (no link), `sign-in` (the email has a user: a link without a
  username) or `sign-up`. The username is checked first, so the answer never tells
  whether an email has an account.
- WebAuthn values (`credential_id`, `public_key`, `challenge`) are unpadded base64url text,
  as on the wire. `sign_count` is a `bigint`: it arrives as text. `public_key` is the COSE
  key (see [WebAuthn](#webauthn)).
- A registration challenge is made by `api` itself, under `own_rows`: `Db.startPasskey`
  deletes the user's earlier ones and inserts a new one (two random UUIDs, as
  `new_token()` makes them). `Db.addPasskey` deletes it and inserts the passkey in one
  statement, `ON CONFLICT DO NOTHING`: no row back means the challenge was expired, used
  or unknown, or the credential is already someone's.
- The driver can't read, then decide, inside a transaction: every check-then-change is one
  statement. A link or a challenge is used once by
  `DELETE ... WHERE ... AND expires_at > now() RETURNING`; a passkey's counter moves by
  `UPDATE ... WHERE $2 > sign_count OR ($2 = 0 AND sign_count = 0) RETURNING`.
- Passkey sign-in is three functions. `create_authentication_challenge(ttl_seconds)`
  stores a challenge that has no user yet and returns it. `passkey_public_key(credential_id)`
  reads the key to check the signature against. `sign_in_with_passkey(challenge,
credential_id, sign_count, backed_up, session_seconds)` uses the challenge up, moves the
  passkey's counter by the rule above, starts a session and returns its token once, with
  an `outcome`: `signed-in`, `no-challenge` (expired, used or unknown) or `no-passkey`
  (gone, or a counter that did not move forward: maybe a copy).
- Postgres can't check a signature: `sign_in_with_passkey` trusts that the server did,
  as `authenticate` trusts nothing but the token. It is the one function that takes no
  secret.
- `passkeys.name` is what made the passkey ("Apple Passwords"), from its AAGUID (see
  [WebAuthn](#webauthn)), or `NULL`.
- `delete_expired()` removes expired links, challenges and sessions, and stale rate-limit
  rows. Nothing schedules it yet.

## Row-level security

Migrations run as the database owner (`DATABASE_URL`), which is `BYPASSRLS`. The server
connects as `api`, which is not and owns nothing: `app.ts` puts `api` and
`API_DATABASE_PASSWORD` into `DATABASE_URL`.

| Table                        | `api` may                                     |
| ---------------------------- | --------------------------------------------- |
| `users`                      | `SELECT`                                      |
| `passkeys`                   | `SELECT`, `INSERT`, `UPDATE (name)`, `DELETE` |
| `webauthn_challenges`        | `SELECT`, `INSERT`, `DELETE`                  |
| `sessions`                   | `SELECT` except `token_hash`, `DELETE`        |
| `magic_links`, `rate_limits` | Nothing: functions only                       |

- Each granted table has the policy `own_rows`: `USING` and `WITH CHECK` compare the row's
  user to `(SELECT NULLIF(current_setting('app.user_id', true), '')::uuid)`. With nobody
  signed in that is `NULL`: reads find no rows and writes fail with `42501`.
- `authenticate(token)` finds the unexpired session and sets `app.user_id` until the
  transaction ends. `Db.signedIn token` starts a `Neon.Transaction` with it and gives the
  user's id, or `Nothing`. Under `Neon.run` the setting ends with its statement. Nothing
  else sets `app.user_id`: the policies trust it.
- Work before sign-in or across users is a function: `SECURITY DEFINER`,
  `SET search_path = public, pg_temp`, `REVOKE EXECUTE ... FROM PUBLIC`,
  `GRANT EXECUTE ... TO api`. It runs as the owner, past every policy, so it takes the
  secret as an argument and filters by it. There are `authenticate`, `rate_limit`,
  `create_sign_up_link`, `create_sign_in_link`, `magic_link`, `consume_magic_link`,
  `sign_out`, `delete_expired`, `create_authentication_challenge`, `passkey_public_key`
  and `sign_in_with_passkey`. `new_token()` is theirs alone: `api` may not execute it.
- A new table: `ALTER TABLE t ENABLE ROW LEVEL SECURITY, FORCE ROW LEVEL SECURITY`,
  `CREATE POLICY`, `GRANT`, after its `CREATE TABLE` (`DROP TABLE ... CASCADE` drops them
  too), and an index on its user column. Never grant `TRUNCATE`, `REFERENCES` or
  `TRIGGER`. A view needs `WITH (security_invoker = true)`.
- `migrations/check-rls.sql` runs after `migrate` and after `current.sql` (`.gmrc`). It
  fails them over a table without `ENABLE` and `FORCE`, an `api` that is privileged or owns
  a table, a `SECURITY DEFINER` function without `search_path` or open to `PUBLIC`, and an
  owner without `BYPASSRLS`. `/api/health` fails when the server's role is exempt.
- It can't tell a grant that is too wide: `migrations/schema.sql` lists every grant, so
  read them in its diff.
- Try policies as `api`, never as the owner, who skips them:
  `psql postgres://api:api@localhost:5432/app`, then
  `BEGIN; SELECT authenticate('<token>'); ...`.

## Rate limiting

`Db.rateLimit { key, limit, windowSeconds }` runs `rate_limit(key, limit, window_seconds)`
and gives `Allowed { remaining }` or `Limited { retryAfter }`, in seconds.

- GCRA: `limit` hits at once, then one more every `window / limit`. A denied hit writes
  nothing.
- One policy per key: name it after what it limits and whom, like
  `magic-link:email:<email>`. At most 512 bytes.
- Several keys in one transaction (`Neon.commit`): always in the same order, or two
  requests deadlock.
- `rate_limits` is a logged table on purpose: Neon drops unlogged tables when the compute
  suspends, which would reset every window.

## Browser app

- `client/Main.elm`, started by `client/main.ts`. `client/main.css` is the only
  stylesheet, linked from `index.html`: don't import CSS from TypeScript.
- `init` and `update` return a `Main.Effect`, never a `Cmd`: `perform` turns it into one
  in `main`, and the tests simulate it (see [Tests](#tests)).
- Calls the server only through `ApiClient.request` with an `Api.Endpoint`, wrapped in the
  `Request` effect; `ApiClient.send` is its `Cmd`. On an error status it decodes
  `Api.errorDecoder`, then the endpoint's response (the health report comes with a 503).
- A `Browser.application` without the `Navigation.Key` in its model: `main` keeps it
  beside the model and hands it to `perform`, for the `PushUrl` and `Load` effects.
  `open` picks the `Page` from the URL: `/`, `/auth/signup`, `/auth/signin`,
  `/auth/verify`, `/dashboard`, else `NotFound`. A new page outside `/auth/` needs its
  rewrite in `vercel.json`. `open` also ends a passkey ceremony left unfinished.
- `init` asks `Api.session` on every page: `model.session` is `Unknown`, then `SignedOut`
  or `SignedIn account`. `/dashboard` shows the account, a sign-out button and the
  Passkeys card (`Api.passkeys`, "Add a passkey", and a bin on each row that asks
  "Remove ...?" before `Api.deletePasskey`: `model.confirming` holds the id meanwhile), and
  sends a signed-out visitor to `/auth/signin`.
- `/auth/signup` and `/auth/signin` post `Api.signUp` and `Api.signIn`, then say to
  check the inbox. `/auth/signin` also has "Sign in with a passkey": post
  `Api.passkeySignInOptions`, the `GetPasskey` port (`navigator.credentials.get`, any
  passkey the device has for this site), then `PasskeyUsed` goes to `Api.passkeySignIn`
  and on to `/dashboard`. `/auth/verify#<token>`
  asks `Api.verification` who the link is for, shows the username and the email, and
  only on the button's click posts `Api.verify`. Both send the token with
  `ApiClient.withToken`, as a bearer token. A sign-in then goes to `/dashboard`; a new
  account is first offered a passkey ("Not now" skips it).
- A passkey takes three steps, `model.ceremony` being `Creating` meanwhile: post
  `Api.passkeyOptions`, send them to `main.ts` through the `CreatePasskey` port
  (`navigator.credentials.create`), and post what comes back in `PasskeyCreated` to
  `Api.addPasskey`. `PasskeyFailed` carries the `DOMException`'s name; `passkeyProblem`
  words it. `main.ts` only converts base64url and buffers: the ceremony's options
  (`residentKey: "required"`, `attestation: "none"`) are its only constants.
- Home: the Server card (one row per `Api.checks`, release and region in the
  footer) and links to sign up and sign in, or to the dashboard. Every page: a theme toggle (`UI.Tabs` without panels, saved to `localStorage` via the
  `WriteToLocalStorage` port, read back from the flags).
- No layout shift while loading: rows come from `Api.checkNames` with `UI.Skeleton`s,
  refreshes keep the last report, an empty footer keeps its height. Name checks only in
  `Api.namedChecks`.

### elmcn components

shadcn/ui for Elm, copied into `client/UI/` and ours to change. Read a module for its API.

```sh
npx shadcn@latest search ricardocasares/elmcn -q dialog
npx shadcn@latest view ricardocasares/elmcn/dialog
npx shadcn@latest add ricardocasares/elmcn/dialog
mv -n src/UI/*.elm client/UI/ && ls src/UI
```

- The registry installs into `src/UI/`. `mv -n` keeps existing (maybe edited) components;
  whatever `ls` still lists, merge by hand, then delete `src/`. `--diff` can't see
  `client/UI/`.
- Follow the CLI's notes: some items need an Elm package (`elm install` at the root) or
  a script import in `client/main.ts`. Strip their comments.
- Builders: `X.new`, content, variants and sizes, `toHtml`; one `|>` per line.

  ```elm
  Button.new
      |> Button.text "Save"
      |> Button.outline
      |> Button.toHtml [ onClick Save ]
  ```

## Server

- `api/src/Main.elm` is a `Platform.worker`. `app.ts` exports `handle(request)`: each
  request carries its Promise's `resolve` into Elm, which sends it back with the
  response. The Vercel Function and the `app:api` plugin (`dev.ts`) serve `handle`.
- `init` and `update` return a `Main.Effect`, never a `Cmd`: `Batch`, `SendToJs` (a
  response, a log or a signature to check), `Database`, a `Neon.Request` whose message also takes the
  latency in ms, and `Email`, a `Brevo.Request`. `perform` turns it into a `Cmd` in `main`, and the tests simulate it
  (see [Tests](#tests)).
- `dev.ts` reloads the server on any `.elm`/`.ts` change in `api/` or `shared/`
  (restarting from `init`); `preview` runs `dist/server/app.js`.
- Routes are real URLs under `/api`: entry points pass requests through unchanged.
- Errors are `{ "error": "..." }` (`Api.encodeError`). JSON is `cache-control: no-store`.
  Without `DATABASE_URL` or `API_DATABASE_PASSWORD` the server starts anyway and the health
  check fails.
- Logging: `log Interop.Info|Warn|Error message`; `app.ts` calls `console[level]` with the
  message unchanged, which Vercel reads as the level. Database errors log at `error`.
- `GET /api/health` (IETF health check draft, `application/health+json`) runs `Db.ping`
  with a 5 s `Neon.withTimeout`. It answers `pass` with 200, `warn` with 200 when slower
  than `slowAfter` (1 s), and `fail` with 503, also when the server's role is a superuser,
  `BYPASSRLS` or a table owner. `output` is a short public reason; the full
  error is only logged, since Postgres messages can name roles and tables. Latency
  includes the hop to Neon and any cold start.
- `POST /api/auth/signup` (`Api.signUp`, `{ "email", "username" }`) starts a sign-up: it
  answers 202 with `{}` once the magic link is emailed, and the user exists only when the
  link is consumed. Three steps, each a `Msg`: both rate limits in one transaction
  (`magic-link:ip:<x-real-ip>` 10 and `magic-link:email:<email>` 3 per 15 minutes, in that
  order), `Db.createSignUpLink` (15 minutes), then the `Email` effect.
- `POST /api/auth/signin/passkey/options` (`Api.passkeySignInOptions`) answers
  `{ challenge, rpId }`, the challenge lasting 5 minutes, or 429: `Db.signInChallenge`
  limits `passkey:ip:<x-real-ip>` to 20 per 15 minutes and makes the challenge in one
  statement, only when allowed.
- `POST /api/auth/signin/passkey` (`Api.passkeySignIn`,
  `{ "id", "clientDataJSON", "authenticatorData", "signature" }`, base64url) signs in
  with a passkey, in four steps: `WebAuthn.authenticate`, `Db.passkeyPublicKey`, the
  `VerifySignature` port (`app.ts` answers `SignatureChecked`, with the `signIn` it was
  given: the model keeps nothing between the two), then `Db.signInWithPasskey`. 200 with
  the account and the `session` cookie, as `POST /api/auth/verify`; 400 (not that JSON);
  401 for every refusal, whose reason is only logged, at `warn`.
- `POST /api/auth/signin` (`Api.signIn`, `{ "email" }`) runs the same steps (`Ask` tells
  them apart) with `Db.createSignInLink`. An email without an account gets 202 and no
  email, so the answer doesn't tell; it does come back sooner, without the Brevo call.
- `GET /api/auth/session` (`Api.session`) answers the account of the `session` cookie,
  read through `Db.account` (`Db.signedIn`, then the user's own row), or 401.
  `DELETE /api/auth/session` (`Api.signOut`) runs `Db.signOut` and clears the cookie.
- Sign-up answers 400 (not that JSON), 422 (`SignUp.parse`: the email's shape; a username
  of 3 to 20 of `a-z`, `0-9`, `_`; both trimmed and lower-cased), 409 (username taken),
  429 with `retry-after`, 502 (Brevo failed; the link stays until it expires) and 503 (no
  database or email, or the database failed). An email that has an account gets 202 and
  a sign-in link instead.
- `SignUp.parse` gives an `EmailAddress` and a `Username`, and `EmailAddress.parse` checks a
  sign-in's email: the only way to make either. `Db.createSignUpLink` and
  `Db.createSignInLink` take nothing else, so an unchecked address doesn't compile.
- Both emails link to `<origin>/auth/verify#<token>`: in the fragment, so the token
  reaches no server log. For the same reason the verify routes take it as
  `authorization: Bearer <token>`, and answer 401 without it.
- The two secrets have their own types: a `SessionToken` comes from the `session` cookie
  (`cookieToken`) or new from `Db`, a `LinkToken` from the bearer header (`bearer`) or new
  from `Db`. Neither is ever empty, and `Db` takes each by its type, so one can't be passed
  for the other.
- `GET /api/auth/verify` (`Api.verification`) answers `{ email, username, newAccount }`
  for the link and changes nothing. `POST /api/auth/verify` (`Api.verify`) consumes it:
  200 with `{ email, username }` and the session in a cookie (`session`, `HttpOnly`,
  `SameSite=Lax`, 30 days, `Secure` when `origin` is https). Both answer 404 to a link
  that is used, expired or unknown; the `POST` answers 409 when the username was taken
  meanwhile.
  `origin` comes from `app.ts`: `VERCEL_PROJECT_PRODUCTION_URL` in production,
  `VERCEL_URL` in a preview, else `http://localhost:5173`; never from the request.
- `init` parses `origin` once into an `Origin`: the URL, its host as the passkeys' `rpId`,
  and whether it is https (the cookie's `Secure`). One that is no URL logs `Bad origin` at
  `error` and leaves the server as broken flags do: no database, so those routes answer 503.
- The passkey routes read the `session` cookie and answer 401 without a session.
  `GET /api/auth/passkeys` (`Api.passkeys`) lists the account's passkeys: `id` (the
  credential id) and `createdAt` (ISO 8601, UTC).
- `POST /api/auth/passkeys/options` (`Api.passkeyOptions`) starts a registration: a
  challenge that lasts 5 minutes, `rp` (see [WebAuthn](#webauthn)), `user` (`id` is the
  user's UUID, whose UTF-8 bytes are the user handle; `name` the email, `displayName` the
  username), `algorithms` and `excludeCredentials`, the ids the account already has.
- `POST /api/auth/passkeys` (`Api.addPasskey`,
  `{ "clientDataJSON", "attestationObject", "transports" }`, base64url) checks the
  browser's answer with `WebAuthn.register`, then saves the passkey: 201 with the passkey,
  400 (not that JSON), 422 (refused: the reason is only logged, at `warn`) and 409 (the
  challenge is expired, used or unknown).
- `DELETE /api/auth/passkeys/<id>` (`Api.deletePasskey id`) removes one of the account's
  passkeys (`Db.removePasskey`, under `own_rows`): 200 with `{}`, or 404. It is the only
  route with a parameter; at `/api/auth/passkeys/options` the method tells it from
  `Api.PostPasskeyOptions`.
- Adding and removing a passkey email the account (`notify`): the answer waits for Brevo,
  since nothing may run after it on Vercel, but is the same when the email fails (logged
  at `error`) or isn't configured. Removing a passkey leaves its sessions alone.
- The report's `releaseId` (`VERCEL_GIT_COMMIT_SHA`, else `VERCEL_DEPLOYMENT_ID`),
  `region` (`VERCEL_REGION`, our own field) and `links.release` (GitHub commit URL from
  `VERCEL_GIT_*`) come from `app.ts`. Off Vercel they are `local`.

## Neon driver

- `Neon.elm` is `neon()` from `@neondatabase/serverless` in Elm: one HTTPS request per
  query or transaction. `Neon.run` and `Neon.commit` take the message for the result and
  return a `Neon.Request`, a record describing that HTTP request; `Neon.send` is its
  `Task`. It uses `elm/http`, so `app.ts` imports `xhr.ts` first: Bun, Node and Vercel
  have no `XMLHttpRequest`.
- Rows are JSON objects by column name. `boolean`, `smallint`, `integer`, `real`,
  `double precision`, `json`, `jsonb` and arrays arrive as JSON; `NULL` as `null`;
  everything else (`bigint`, `numeric`, `timestamptz`...) as Postgres's text. Duplicate
  column names keep the last: use `AS`. Decode with the same decoders as the `Api` codecs.
- `Neon.selectOne` gives a `Maybe`; `Neon.one` fails with `UnexpectedResult` unless exactly
  one row comes back.
- Parameters travel as text and Postgres converts them; `Neon.json` for `json`/`jsonb`.
- The endpoint comes from the host (`ep-x.eu-central-1.aws.neon.tech` →
  `api.eu-central-1.aws.neon.tech`); `Neon.withEndpoint` overrides it (`NEON_ENDPOINT`).
- The connection string holds the password: server only.

## Email

- `Brevo.elm` posts one transactional email to `https://api.brevo.com/v3/smtp/email`, as
  Brevo's SDK does: `Brevo.email client message toMsg` returns a `Brevo.Request`, the same
  record as a `Neon.Request`; `Brevo.send` is its `Task`. `Brevo.client` takes the API
  key, sent as the `api-key` header: server only.
- A `Message` has `from`, `to`, `replyTo`, `tags` and `content`: a template's `subject`,
  `html` and `text`, always sent together. No Brevo-hosted templates, attachments,
  scheduling or batches.
- Any 2xx is `Ok { messageId }`. An error status with Brevo's `{ "code", "message" }` is
  `Rejected { status, code, message }`, anything else `ServerError`. Nothing is retried.
- A template is a module in `api/src/Email/` with one `email` function from its values
  to an `Email.Layout.Email`. It lists `Layout` blocks (`Heading`, `Paragraph`, `Button`,
  `Footnote`) and `Layout.render` writes the HTML and the text from them, escaping every
  value: never build HTML in a template.
- Templates: `Email.SignIn` and `Email.SignUp`, for the two kinds of magic link;
  `Email.PasskeyAdded` and `Email.PasskeyRemoved`, which tell the account what happened
  to it and link to `<origin>/dashboard`.
- `Email.configure` (`api/src/Email.elm`) builds the `Mailer` in the model from `BREVO_API_KEY` and `EMAIL_FROM`,
  an address verified as a sender in Brevo; its name is `pixo`. Without either the server
  starts anyway, warns, and `model.email` is an `Err`.
- `Email.send mailer { to, tag } email toMsg` is the `Brevo.Request` for one recipient,
  with a 10 s timeout; wrap it in the `Email` effect. The tag names the template
  (`sign-in`) and shows in Brevo's logs.
- `POST /api/auth/signup` sends `Email.SignUp`, or `Email.SignIn` (see [Server](#server)).

## WebAuthn

- `WebAuthn.elm` checks a registration in Elm, with no port: `WebAuthn.register` takes the
  expected `origin` and `rpId` and the browser's answer, and gives a `Registration` (the
  challenge to use up, the credential id, the public key, the counter, the backup flags
  and the transports it knows) or the reason as text, for the log.
- The relying party's id is the origin's host (`WebAuthn.rpId`), so a passkey belongs to
  one host: production's, a preview's own, or `localhost`.
- It checks the client data (`webauthn.create`, the origin, not cross-origin) and the
  authenticator data (the hash of the id, user present, a credential, backed up only if
  eligible, an algorithm from `WebAuthn.algorithms`: EdDSA, ES256, RS256). The challenge
  is checked by the database, which uses it up.
- Attestation is `none`: the statement is ignored, whatever its format, so nothing proves
  which authenticator made the key. User verification is preferred, not required.
- `WebAuthn.authenticate` checks a sign-in the same way (`webauthn.get`) and gives the
  challenge, the counter, the backup flag and `signed`: the authenticator data followed
  by the SHA-256 of the client data, which is what the passkey signed.
- Elm can't check the signature, so `app.ts` does, with WebCrypto, and nothing else:
  `WebAuthn.key` turns the stored COSE key into the parts of a JWK (`Es256`, `EdDsa`,
  `Rs256`) and `WebAuthn.signature` turns an ES256 signature from DER into the two
  32-byte integers WebCrypto takes. A key that fails to import counts as a bad signature.
- `Aaguid.name` names what made a passkey, from the AAGUID in its authenticator data:
  the names in
  [passkey-authenticator-aaguids](https://github.com/passkeydeveloper/passkey-authenticator-aaguids)'s
  `aaguid.json`, copied on 2026-11-10. Security keys and anything that sends zeros have
  no name, and the app says "Passkey". Passkeys made before this have none either.
- Packages: `elm-toulouse/cbor` (the attestation object and the COSE key, which is stored
  re-encoded, without the extensions that may follow it), `folkertdev/elm-sha2`,
  `danfishgold/base64-bytes`.

## Tests

`bun run test` runs [elm-test](https://github.com/rtfeldman/node-test-runner) in both Elm
projects, one after the other: `test:app` over `tests/` (root `elm.json`: `client/` and
`shared/`), `test:api` over `api/tests/` (`api/elm.json`). `bun test` is Bun's own runner
and finds nothing. One project, rerun on save: `bun run test:app --watch`. Test packages
go in `test-dependencies`: `bunx elm-test install <package>`, inside `api/` for the server.

- Each project's `MainTest.elm` drives its whole `Main` with
  [elm-program-test](https://elm-program-test.netlify.app/): `start`, then what happens
  to the program, ending in one `expect*` (`ensure*` in between).
- `simulateEffects` there mirrors `Main.perform`, and `simulateSubscriptions` mirrors
  `main`'s `subscriptions`. Both match `Main.Effect` without a wildcard, so a new variant
  doesn't compile until `perform` and `simulateEffects` handle it.
- A new endpoint, query, email or `FromElm` variant needs no new simulation: reuse
  `Request`, `Database`, `Email` and `SendToJs`. Add an `Effect` variant only for a new kind of command.
- Write response bodies and port values as the JSON on the wire, not with the `Api` or
  `Interop` encoders: the test then pins the format the other side relies on.
- Browser: act as a user does. `startAt "/auth/signup"` opens a page; `fillIn` a field by
  its id and label, `clickButton` by its text (the `sr-only` label on an icon-only
  control), `clickLink`, then look for text in the view.
- Server: `request "first" "GET" [ "api", "health" ]` sends a request in through the port.
  Its first argument stands for the Promise's `resolve` and comes back in the response.
  Neon and Brevo are the only HTTP calls: answer Neon with
  `simulateHttpOk "POST" endpoint` and the rows, Brevo at `Brevo.endpoint`.
  `advanceTime` moves the clock that `Database` measures latency with.
- A passing check of outgoing port values clears them: look at logs and responses in one
  `expectSent`, not one after the other.
- `api/tests/NeonTest.elm` needs no program: a `Neon.Request` is a record, so `.body` is
  what would be sent and `.onResponse` decodes an `Http.Response`.
- `api/tests/BrevoTest.elm` works the same way; `api/tests/EmailTest.elm` covers `Email`
  and renders the templates. A new template gets a test that its values reach the HTML and the text.
- `api/tests/WebAuthnTest.elm` feeds `WebAuthn` attestations and assertions written by
  another encoder, byte by byte, for `https://pixo.pics`: one fixture per flag or fault.
  No test checks a real signature: `MainTest` answers `VerifySignature` itself, with
  `SignatureChecked`.
- `tests/ApiTest.elm` checks `Api.parse` against `Api.method` and `Api.path` for each
  route in its `routes`: add a new route there.

## Vercel

The Vercel project `pixo.pics` (team `forkaround`) deploys from GitHub: a push to `main`
deploys production, a push to any other branch a preview. `vercel.json` configures it. The
Neon project `pixo` (`small-lab-61985339`, `aws-eu-central-1`) was created from the Vercel
Marketplace and is connected as the store `pixo`, which manages the variables. Keep
`regions` next to Neon's region.

| Vercel            | Neon branch                 | Variables                               |
| ----------------- | --------------------------- | --------------------------------------- |
| Production        | `main`                      | `DATABASE_URL`, `DATABASE_URL_UNPOOLED` |
| Preview (`<git>`) | `preview/<git>`, off `main` | same, injected into each deployment     |
| Development       | None: not connected         | None: local runs use `.env`             |

- A preview has its own Neon branch only because the store's connection has "Create
  database branch for deployment" ticked for Preview, and not for Production. It is off by
  default, and without it a preview build migrates `main`, which is production.
- That option is in the dashboard only (Storage, `pixo`, Connect Project):
  `vercel integration resource connect` can't set it, and changing it means removing the
  connection and connecting again. To check it:
  `vercel api "/v1/storage/stores?teamId=forkaround"` lists `preview` under
  `projectsMetadata[].deployments.actions`.
- A preview's variables override the project's for that deployment and don't show in the
  project's settings. Its Neon branch goes when Vercel removes the deployment.
- Previews sit behind Vercel Authentication: request them with `vercel curl <url>` from a
  folder linked to the project.
- Build: `DATABASE_URL="$DATABASE_URL_UNPOOLED" bun db migrate && bun run build`. Each
  deployment migrates its own branch, so a preview's migrations never reach `main`; a
  missing variable fails the build.
- The `api` role is created once on Neon's `main`, in SQL as the owner, because a role made
  in the console, CLI or API is `BYPASSRLS`:
  `CREATE ROLE api LOGIN PASSWORD '<random>'; ALTER ROLE api SET search_path = public, pg_temp;`.
  Its password is `API_DATABASE_PASSWORD`, a Vercel secret in Production and Preview, as
  are `BREVO_API_KEY` and `EMAIL_FROM` (not a secret). A
  branch copies its parent's roles and passwords, so previews need nothing more, unless
  `main` is a protected branch: its children get new passwords. A branch older than the
  role fails its build at `GRANT ... TO api`: reset it as in [Migrations](#migrations).
- The function's environment still holds the owner's `DATABASE_URL`: RLS contains the
  server's queries, not a compromised process.
- `build` runs the Elm steps one at a time: on a fresh machine, two processes filling
  `~/.elm` at once corrupt it ("PROBLEM BUILDING DEPENDENCIES"), and elm-ts-interop still
  exits 0. `types:api` must `cd api`, as elm-ts-interop uses the nearest `elm.json`.
- `dist/client` is served; `dist/server` is bundled into `api/index.ts`, which every
  `/api/*` rewrites to (Vercel keeps the original URL in `request.url`). `/auth/*`
  and `/dashboard` rewrite to `index.html`, for the browser app's pages. Bun runtime
  (`"bunVersion": "1.x"`, beta).
- Nothing is linked locally (`.vercel` and `.neon` are not committed): pass
  `--scope forkaround --project pixo.pics` to `vercel` and
  `--project-id small-lab-61985339` to `bunx neon@latest`. Use `--no-env-pull` with
  `neon link` or `neon checkout`, or they write Neon's `DATABASE_URL` into `.env`.
