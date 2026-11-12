# AGENTS.md

A Vite app, run by Bun: an Elm browser app and an Elm HTTP server sharing one API contract,
on Postgres (Neon), deployed to Vercel.

## Layout

| Path                | What                                                                                                                                              |
| ------------------- | ------------------------------------------------------------------------------------------------------------------------------------------------- |
| `client/`           | Browser app (root `elm.json`). `client/UI/`: elmcn components                                                                                     |
| `client/Data/`      | Browser-only value types: `Dimension`, `Steps`, `Seed`, `Prompt` (see [Image pipeline](#image-pipeline))                                          |
| `shared/`           | Elm both sides compile: `Api` (routes, endpoints, codecs), `Path` (pages), `Data`                                                                 |
| `shared/Data/`      | Value types both sides compile: `Email`, `Username`, `LinkToken`, `CredentialId`, `Challenge`                                                     |
| `tests/`            | elm-test suites for `client/` and `shared/` (see [Tests](#tests))                                                                                 |
| `api/tests/`        | elm-test suites for the server                                                                                                                    |
| `api/src/`          | Server Elm (`api/elm.json`): `Main`, `Server`, `Effect`, `Reply`, `Db`, `Request`, `Response`, `RateLimit`, `SessionCookie`, `WebAuthn`, `Aaguid` |
| `api/src/Flow/`     | One module per request flow: `Health`, `MagicLink`, `Session`, `Passkeys`, `PasskeySignIn`                                                        |
| `api/src/Data/`     | Server-only value types: `SessionToken`, `Origin`, `SignUp`, `UserId`, `ClientAddress`                                                            |
| `api/src/Services/` | Drivers: `Neon`; `Mailer`, the interface, with `BrevoMailer` and `LogMailer` behind it and its templates in `Mailer/` (see [Email](#email))       |
| `api/_ts/`          | Server TypeScript: `app.ts` (starts Elm), `xhr.ts`, `dev.ts` (Vite plugin)                                                                        |
| `api/index.ts`      | The only Vercel Function                                                                                                                          |
| `migrations/`       | Graphile Migrate (`.gmrc`)                                                                                                                        |
| `.scripts/neon.sh`  | Starts the local Neon proxy                                                                                                                       |
| `.agents/`          | Agent skills, tracked by `skills-lock.json`; `.claude` links here                                                                                 |

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
- **Value types live in `Data`**: one module per type, the type exposed
  without its constructor, made only by that module's `parse`, `fromString`
  or `decoder`. `shared/Data/` for a value that crosses the wire or sits in
  a `Path`, `api/src/Data/` for one only the server holds, `client/Data/` for
  one only the browser holds. Import as
  `import Data.Email as Email exposing (Email)`. Drivers of outside services
  live in `api/src/Services/`, imported as `import Services.Neon as Neon`.
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
  (`phosphor-icons/phosphor-elm`, `Regular`, and `Bold` in the sidebars' menus, their
  trigger, the user menu, the theme toggle and the login and sign-up forms, where a 1 px line looks
  thin beside medium text) instead of hand-written SVG; `sr-only` text on icon-only
  controls.
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

| Variable                | Used by                                                       |
| ----------------------- | ------------------------------------------------------------- |
| `DATABASE_URL`          | Migrations, as the owner; the server takes host and database  |
| `API_DATABASE_PASSWORD` | Server: the password of `api`, the role it connects as        |
| `NEON_ENDPOINT`         | Server (local proxy instead of Neon), `dev:proxy`'s port      |
| `BREVO_API_KEY`         | Server: Brevo's API key; empty locally, so emails are printed |
| `EMAIL_FROM`            | Server: the sender's address, verified in Brevo               |
| `SHADOW_DATABASE_URL`   | `bun db watch` and `commit`                                   |
| `ROOT_DATABASE_URL`     | `bun db reset`, the proxy's own connection                    |

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
- `bun db migrate` creates the tables in [Schema](#schema). `/api/health` needs them
  too: it runs `rate_limit`, so it fails until the database is migrated.

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
  key (see [WebAuthn](#webauthn)). In Elm a credential id is a `CredentialId`, a challenge
  a `Challenge` and a user's id a `UserId`; the public key and the signature stay text
  inside `WebAuthn`.
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

`Db.rateLimit policy` runs `rate_limit(key, limit, window_seconds)` and gives
`RateLimit.Allowed { remaining }` or `RateLimit.Limited { retryAfter }`, in seconds.

- GCRA: `limit` hits at once, then one more every `window / limit`. A denied hit writes
  nothing and takes no lock: `rate_limit` reads the row first and answers from it, so a
  flood on one key doesn't queue. Only a hit that may be allowed runs the upsert, which
  locks the key's row until its transaction ends.
- One policy per key: name it after what it limits and whom, like
  `magic-link:email:<email>`. At most 512 bytes. Each policy is a function in
  `RateLimit.elm` that takes what it limits (`ClientAddress`, `Email`); a key is written
  nowhere else. A new limit is a new function there.
- Several keys in one transaction (`Neon.commit`): always in the same order, or two
  requests deadlock.
- `rate_limits` is a logged table on purpose: Neon drops unlogged tables when the compute
  suspends, which would reset every window.

## Browser app

- `client/Main.elm`, started by `client/main.ts`. `client/main.css` is the only
  stylesheet, linked from `index.html`: don't import CSS from TypeScript. The font is
  Inter, served with the app: `main.css` imports `@fontsource-variable/inter` and names
  it in `--font-sans`, so no page asks another site for it.
- `init` and `update` return a `Main.Effect`, never a `Cmd`: `perform` turns it into one
  in `main`, and the tests simulate it (see [Tests](#tests)).
- Calls the server only through `ApiClient.request` with an `Api.Endpoint`, wrapped in the
  `Request` effect; `ApiClient.send` is its `Cmd`. On an error status it decodes
  `Api.errorDecoder`, then the endpoint's response (the health report comes with a 503).
  A failure is an `ApiClient.Error`: `Unauthorized` for a 401, `Failed` for anything else,
  each with the text to show (`ApiClient.errorToString`).
- A `Browser.application` without the `Navigation.Key` in its model: `main` keeps it
  beside the model and hands it to `perform`, for the `PushUrl` and `Load` effects.
  `open` picks the `Page` from `Path.parse` (`shared/Path.elm`; `Path.Verify` holds a
  `Maybe LinkToken`, and `ApiClient.withToken` takes a `LinkToken`), which names every
  address: `/` (Explore), `/studio`, `/library`, `/settings/models` (also `/settings`),
  `/settings/server`, `/settings/account`, `/signup`, `/login`, `/verify`, `/passkey`,
  and `Path.NotFound` for the rest. It reads them with `Url.Parser`, so a trailing slash or a
  query string leads to the same page. Links, `PushUrl` and the server's emails write the
  same addresses with `Path.toString`. A new page needs a `Path` variant (`open` doesn't
  compile until it handles it) and its line in `Path.parse`'s parser. `open` also ends a
  passkey ceremony left unfinished. `NotFound` always links home and adds what fits the
  session: the account, sign-in and sign-up, or "Try again", in a row
  that keeps its height while the session is `Unknown`.
- `init` asks `Api.session` on every page: `model.session` is `Unknown`, then `SignedOut`
  (only on a 401), `SignedIn account`, or `Unavailable reason` when the check failed some
  other way: the page then says so and offers "Try again" (`SessionRetried`), and sends
  nobody to `/login`. `/settings/account`, a section of the settings listed only to a
  signed-in visitor, shows the account, a sign-out button and the Passkeys card
  (`Api.passkeys`, "Add a passkey", and a bin on each row that asks "Remove ...?"
  before `Api.deletePasskey`: the `Account` section holds the list, the id being
  confirmed and why a removal failed, and a removal never touches `model.ceremony`),
  and sends a signed-out visitor to `/login`. There is no dashboard: a sign-in leads
  home, or to `/passkey` when it was by link and the account has no passkey.
- `/signup`, `/login`, `/verify`, `/passkey` and the not-found page fill the window,
  outside the shell, in one frame, `viewPlain`: the logo, which leads home, over a narrow column in the middle
  of what is left, its text aligned left, over the theme toggle; and beside all that,
  from `lg` up, `ExploreView.mosaic`, Explore's tiles in three columns that fade out
  towards the bottom (`mask-b-from-55%`): decoration only. The column starts with a
  heading and one line of text (`viewLead`, the page's `h1`), and its inputs and
  buttons are `h-10`.
- On `/signup` and `/login` the column starts with `viewAuthTabs`, `UI.Tabs` without
  panels, as wide as the column, which reads "Log in" and "Create an account": a
  click is `PathSelected`, which pushes the other address, so each tab has its own
  URL and `open` starts its form empty. Under the tabs come the heading and the form. Each input is a `UI.InputGroup` with
  an icon at its start and a placeholder, its text medium like the menus'; its label is still there, `sr-only`, for
  screen readers and for `fillIn` in the tests.
- They post `Api.signUp` and `Api.signIn`, then say to check the inbox. On `/login`
  the passkey comes first, since a passkey costs no email: "Log in with a passkey",
  the form's only filled button, then under "or" the email field and "Email me a
  link", an outline button. The passkey: post `Api.passkeySignInOptions`, the
  `GetPasskey` port (`navigator.credentials.get`, any passkey the device has for
  this site), then `PasskeyUsed` goes to `Api.passkeySignIn` and on to `/`.
  `/verify#<token>` asks
  `Api.verification` who the link is for, shows the username and the email
  (`viewIdentity`: an avatar of two letters beside them, as in the user menu), and
  only on the button's click posts `Api.verify`. Both send the token with
  `ApiClient.withToken`, as a bearer token. Once the link is used the page asks
  `Api.passkeys` (`GotPasskeyCheck`): an account with none goes on to `/passkey`,
  new or not and every time it signs in by link; one that has a passkey, or whose
  list can't be read, goes to `/`. A link that is refused offers "Sign up again"
  and "Log in".
- `/passkey` asks the account for a passkey, on a page of its own: "Create a passkey"
  runs the ceremony below and then leads home, "Not now" leads home without one
  (a browser may have no passkeys, and the next sign-in is then by link). It holds
  no state but `model.ceremony`, so anyone signed in may open it; a signed-out
  visitor is sent to `/login`, as at `/settings/account`.
- Showing who the link is for gives nothing away: the token that reads the email and
  the username could sign in as them. It is there so that someone sent another
  person's link sees whose account it is before the click. While the link is
  checked, skeletons of the same sizes stand in, so nothing moves when it is found.
- A passkey takes three steps, `model.ceremony` being `Waiting` meanwhile: post
  `Api.passkeyOptions`, send them to `main.ts` through the `CreatePasskey` port
  (`navigator.credentials.create`), and post what comes back in `PasskeyCreated` to
  `Api.addPasskey`. `PasskeyFailed` carries the `DOMException`'s name; `passkeyProblem`
  words it. `main.ts` only converts base64url and buffers: the ceremony's options
  (`residentKey: "required"`, `attestation: "none"`) are its only constants.
- Every page but `/signup`, `/login`, `/verify`, `/passkey` and the not-found page
  is in a shell, `viewShell`: a
  `UI.Sidebar` that collapses to its icons (a sheet below `md`, closed by `open` once a
  page is picked; its `State` is `model.sidebar`) with Explore, Studio, Library and
  Settings, and in its footer the user menu, whoever is there. Its header is the logo,
  `Logo.wordmark` (`client/Logo.elm`),
  the one SVG drawn by hand, stroked in `currentColor` so it takes the text's color. Its paths
  run along the middle of a 5-unit stroke and the view box leaves room for it: change
  the stroke and both change. A class goes on it with `Svg.Attributes.class`. It is as
  tall as a menu icon, in a row as tall as a menu item, so the menu under it stays put
  when the sidebar opens or closes. The `pixo-logo` wrapper transitions from the wordmark's
  width to one icon's width (`w-4`), with the sidebar's 200 ms linear timing.
  `client/logo.ts` observes that width and interpolates the SVG path into its
  `data-collapsed` X: both paths must have the same commands and coordinate count.
  The collapsed X is centered at 20 SVG units; its shape and stroke stay the same.
  Reduced motion skips the morph, and the mobile sheet keeps the full wordmark.
  `public/favicon.svg` is the same X, white on a
  dark tile so it shows on a light tab and on a dark one: its path is copied from
  `Logo.elm`, so a change to the X goes in both.
- The sidebars' look is in `UI.Sidebar` itself, changed from the registry's: menu items
  are medium and muted until hovered or current, with `rounded-lg` pills; the header,
  groups and footer keep 12 px from the edges; and a sidebar down to its icons is
  3.5rem wide, so that padding holds and an icon stays where it was. The trigger is
  Phosphor's `sidebarSimple`, `Bold` like the menus' icons. Adding the component again
  from the registry would undo this: merge by hand.
- The page beside the sidebar is `viewFrame`: one top bar across it (`viewBar`: the
  sidebar's trigger, the page's name, then at its right end the page's own tools, as
  the library's search, and `viewEntry`), sticky, and under it an optional second
  sidebar (`viewAside`: the studio's controls, the settings' sections, sticky below
  the bar, as tall as what is left of the window and on the page's own background,
  with a line between it and the content) beside the content. `viewEntry` is
  "Log in" (left out below `sm`, where the guest's menu has it) and "Sign up" for a
  signed-out visitor, on every page of the shell: two ghost buttons that look like
  the sidebar's links, muted until hovered, neither one stressed. They are
  drawn `invisible` while the session is `Unknown`, so the tools beside them don't
  move when they appear.
- The user menu (`viewUserMenu`) is the registry's sidebar demo's: a `UI.DropdownMenu`
  whose trigger is a large menu button with a `UI.Avatar`, two lines of text and a
  caret, so a sidebar down to its icons shows the avatar alone. It opens to the right,
  without saying again who it is for, with what `viewSession` gives it, the theme as
  three radio items, which leave the menu open, and for an account "Sign out". Signed
  in, the trigger shows the username's first two letters (there is no picture), the
  username and the email, and the menu has "Account
  settings" (`PathSelected Path.Account`). Signed out it is the same menu for
  "Guest", "Not signed in", under a person's icon, with "Log in" and "Create an
  account"; when the session can't be checked it says so and has "Try again". While
  the session is `Unknown` a skeleton of the same height stands there, so the footer
  never changes its height.
- The theme is three radio items in that menu; the pages without a sidebar
  (`viewPlain`'s) have the toggle, `UI.Tabs` without panels,
  its icons `Bold` like the menu's. Either
  saves it to `localStorage` via the `WriteToLocalStorage` port, and the flags read
  it back.
- `/studio` makes pictures in the browser and `/library` keeps them there: neither
  needs a session (see [Image pipeline](#image-pipeline) and [Library](#library)).
- `/`, Explore, is home (the logo, "Go home" and a sign-out lead there) and a
  placeholder, wired to nothing yet: `client/ExploreView.elm` draws `tiles`, stock
  photos of several shapes, in the library's columns, and holds no state. The photos
  are Unsplash's, asked from its own server (`images.unsplash.com`, cropped by the
  address to the tile's `width` and `height`): the only thing a page asks another
  site for. A tile keeps its shape while its photo loads. Over the tiles, for
  everyone, a heading ("See what people are making") and one line of text.
- Above the tiles a signed-out visitor gets a first screen: one large photo from
  edge to edge, as tall as the window under the top bar
  (`min-h-[calc(100svh-3rem)]`), with a heading, one line of text, "Open the studio"
  and "Sign up" over its lower left, in white on a dark gradient whatever the theme.
  From 58% of its height down, the photo and its gradient fade into the page's
  background (`mask-dissolve`, a utility in `main.css`): its stops follow an
  S-curve, slow at both ends, since a fade that stops short shows as a line where
  it ends. The text keeps 29svh under it (`pb-[29svh]`) and ends where the photo
  is still nearly whole: white text can't sit on a fade that ends white in the
  light theme.
  The buttons take `Overlay.solid` and `Overlay.glass`, as every button over a
  picture does. The photo is `ExploreView.cover`, one of the tiles' in three widths
  (`srcset`): pick a dark, calm one, since the text sits on it.
- It is `ExploreView.Hero`, which `Main.hero` picks from the session: `Shown` when
  `SignedOut`, `Reserved` while `Unknown` (drawn `invisible`, so it moves nothing
  when it appears), `Hidden` otherwise. Keep it plain: no badge over the heading,
  no gradient text, no list of ticks, nothing that moves. Its second button says
  "Sign up", as the top bar's does: at `/` a test can't `clickLink "Sign up"`,
  which fails on two matches.
- `/settings/models` lists the models (see [Image pipeline](#image-pipeline));
  `/settings/server` has the Server card (one row per `Api.checks`, release and region in
  the footer), which asks `Api.health` when it opens.
- No layout shift while loading: rows come from `Api.checkNames` with `UI.Skeleton`s,
  refreshes keep the last report, an empty footer keeps its height. Name checks only in
  `Api.namedChecks`.

### Image pipeline

The studio (`/studio`) runs [bonsai-image-pipeline-lib](https://github.com/nishlumi/bonsai-image-pipeline-lib)
(WebGPU, MIT), a dependency pinned to a commit in `package.json`: the model is downloaded
from Hugging Face into the browser and runs there. The server takes no part.

- `client/Pipeline.elm` decides everything: `Settings` (the `Weights`, the prompt, width,
  height, steps and seed), the `Engine` (`Unchecked`, `Checking`, `Unsupported`, `Empty`,
  `Clearing`, `Loading`, `Ready`, `Generating`, `Releasing`) and `update`, which returns
  the model and a `Maybe Command`. It imports no `Html`: `client/StudioView.elm` draws
  the studio (`controls`, the second sidebar, and `canvas`, the pictures) and
  `client/ModelsView.elm` the list at `/settings/models`.
- `Main` keeps it in `model.pipeline`, not in the page, since the model stays in the
  graphics card's memory across pages: `GotPipeline` wraps its `Msg`, `runPipeline` turns
  a `Command` into `SendToJs (Interop.RunPipeline command)`, and opening the studio or
  the models calls `Pipeline.enter`, which asks once whether the browser has WebGPU.
- Nobody loads a model by hand. "Generate" plans the `Job`, then `start`s it: with the
  chosen weights in memory it generates; with none, `Loading` holds the job and
  `ModelLoaded` starts it; with other weights in memory, `Releasing` holds a `Pending`
  (weights, the job) and `ModelReleased` loads it. "Download" at `/settings/models` is
  the same path without a job, so a download ends with the model in memory. `cache` is
  always true and `force` always false: a model is downloaded again only after "Remove
  downloaded models".
- The library loads a model one part after the other (`Component`: the text encoder, the
  transformer, the decoder). The studio and `/settings/models` show the same single bar
  (`StudioView.loading`), like the one for the steps: the part last heard of
  (`Pipeline.current`) and how far it is, so the bar starts over for each part.
- `model.pipeline.downloaded` lists the weights that finished loading, newest first:
  the library can't tell whether its cache holds a whole model. `runPipeline` writes it
  to `localStorage` (`models`, a JSON list of ids, `Pipeline.remember`) whenever it
  changes, and the `models` flag brings it back (`Pipeline.remembered`, which drops ids
  no longer in `Weights`). A cancelled download is not in it; "Remove downloaded models"
  empties it. The studio's button says "Download and generate" for weights not in it.
- `Pipeline.presets` are the sizes for social networks (`Square` 1:1, `Portrait` 4:5,
  `Story` 9:16, `Widescreen` 16:9, `Pin` 2:3, `Link card` 2:1, `Banner` 3:1), each the
  largest of its shape on the `Dimension` grid: a new one must be on it, or `preset`
  leaves it out. One is pressed when the width and height are its own. The studio
  draws them as one row that scrolls sideways and runs to the panel's edges (`-mx-4`
  against the panel's `p-4`), so a cut-off preset shows there are more.
- The library runs in a web worker, `client/pipeline.worker.ts`, so downloading and
  generating never block the page. Each `Command` is a `FromElm` variant (`CheckWebGpu`,
  `LoadModel`, `GenerateImage`, `CancelPipeline`, `ReleaseModel`, `ClearModelCache`)
  whose `data` is the library's own options: `main.ts` posts it to the worker as it is,
  starting the worker on the first one, and the worker passes the options to the
  library. What the library reports comes back the same way, as a `Pipeline.Event`
  (`WebGpuChecked`, `ModelProgress`, `ModelLoaded`, `GenerationStep`, `ImageGenerated`,
  `ModelReleased`, `ModelCacheCleared`, `PipelineFailed`).
- The worker holds the pipeline object and one `AbortController`, and decides nothing: a
  new option or rule goes in `Pipeline.elm`. Its constants are `progressInterval` (the
  library reports progress some 300 times a second, and the worker sends on one report
  every 100 ms, and the last of each part) and `largestBatch` (see the next point). Its
  `switch` ends with the same `never` as the port handlers.
- A worker frees the page's thread, not the graphics card, which also draws the page.
  The library submits each pass without waiting for the card, some 350 passes a step, so
  the browser's own frames queue behind them: measured in a Chromium browser on an Apple GPU,
  stalls of 1.4 s, several per picture. So `pace` in the worker wraps the library's
  `runtime.host.submit`, which every pass awaits: after a batch of passes it awaits
  `onSubmittedWorkDone`, halving the batch when that took longer than `frameBudget` and
  doubling it (up to `largestBatch`) when it took less than half. At 512 by 512 the
  worst stall is then 16 ms, for 13% more time.
- `frameBudget` comes from Elm with each `GenerateImage`, always `Pipeline.frameBudget`,
  8 ms: there is no way to turn pacing off. The worker takes it out of the options
  before they reach the library.
- Pacing can't split one pass: at 1024 by 1024 single passes run up to 1.4 s, and the
  page still stutters (stalls up to 250 ms), far less than without it. Only the library's
  shaders could change that. `pace` reaches into the library (`runtime.host`), so check
  it when the pinned commit moves: `client/bonsai.d.ts` types what it touches.
- To measure a stall without a visible window, time a 1 by 1 WebGL `readPixels` on the
  page while the worker generates: idle it takes under 1 ms. `requestAnimationFrame`
  tells nothing here: it keeps its rhythm while nothing on the page changes.
- The worker answers `ImageGenerated` with the PNG as a `Blob`, and `main.ts` makes the
  object URL: one made in the worker would not outlive it. A worker that fails to start
  comes back as `PipelineFailed`.
- WebGPU is asked for inside the worker, so a browser that has it only on the page gets
  the "can't make pictures" card.
- An event is applied only in the state that waits for it, so one that arrives late
  (after a cancel, or for work never started) changes nothing. A cancel waits for the
  library to stop (`PipelineFailed` with the name `AbortError`, which shows no message):
  the library runs one job at a time. An `AbortError` shows no message only while
  loading or `Stopping`: a graphics card that drops the device mid-picture rejects with
  the same name ("Device was lost before MapAsync completed"), and that one is shown.
  The model in memory is then unusable until it is released and loaded again.
- The values the library takes have types in `client/Data/`: a `Dimension` is 256 to 1024
  in steps of 64 (the model needs multiples of 16), `Steps` 1 to 50, a `Seed` 0 to
  4294967295 (the library reads it as 32 bits), a `Prompt` trimmed and never blank.
  `guidanceScale` is always `Pipeline.guidance`, 1: the library takes nothing else.
- `Weights` lists the models the browser can run, the `-mlx-` ones: `-gemlite-` ones are
  for PyTorch and answer 404. `Pipeline.weightsSize` is what `Loading` adds up to, read
  from a real download: change it with the list.
- A random seed is drawn in Elm, from `model.random`, a `Random.Seed` started from the
  `entropy` flag (`crypto.getRandomValues` in `main.ts`) and stepped on each picture: no
  `Effect` for it, and a test fixes it through the flags.
- "Remove downloaded models" (`clearCache`) works only with no model in memory, as the
  library needs: it removes every model, since they share one cache.
- A picture is that object URL, kept in `model.pipeline.pictures` with the `Job` and the
  `Weights` that made it, until the page is closed or it is saved (see
  [Library](#library)). Nothing is uploaded.
- The studio shows a picture as the picture and nothing else, like the library's dialog:
  hover or keyboard focus lays the prompt, the settings, "Save", download and
  "Use this seed" over it, and since a touch screen has no hover the picture is a button
  that pins them (`model.pipeline.detailed`, one picture at a time). The buttons over a
  picture share their looks in `client/Overlay.elm` (`glass`, `solid`, `red`).
- `client/bonsai.d.ts` types the library, which ships none: keep it to what the worker
  calls. Vite builds the worker, library included, as its own file (`worker.format` is
  `es` in `vite.config.ts`), fetched on the first `CheckWebGpu`. The worker imports
  `Main.elm` for its types only: Elm never runs there.

### Library

`/library` shows the pictures saved from the studio. They live in the browser's
IndexedDB (database `pixo`, store `pictures`), each row the PNG and what made it: `id`
(auto-incremented, so a higher id is a newer picture), `prompt`, `width`, `height`,
`steps`, `seed` and `model` (the weights' id).

- `client/Library.elm` decides everything, like `Pipeline.elm`: `update` returns the
  model and a list of `Command`s, and `client/LibraryView.elm` draws it (`search`, the
  field in the page's bar, and `view`). `Main` keeps it in `model.library`: `GotLibrary`
  wraps its `Msg`, `runLibrary` turns each `Command` into
  `SendToJs (Interop.RunLibrary command)`, and opening `/library` calls `Library.enter`,
  which reads the pictures once (again after a failed read).
- `client/library.ts` is the whole of IndexedDB and decides nothing: `LoadLibrary` reads
  every row, `SavePicture` fetches the studio's object URL and adds the `Blob` with the
  settings Elm sent, as they are, `DeletePicture` deletes by id and revokes the URL. It
  answers `LibraryLoaded`, `PictureSaved` (with the `source` URL it was asked to save)
  and `PictureDeleted`, each picture with a new object URL, or `LibraryFailed` with the
  error's message. A new field is one more line in `InteropDefinitions`.
- A stored row comes back as a `Library.Stored`, raw; `fromStored` parses it into a
  `Picture` with the `Data` types, and a row that doesn't parse is left out.
- `model.library.saves` tells the studio what became of each of its pictures, by object
  URL: `Saving`, then `Saved`, or unsaved again after `LibraryFailed`.
- The grid is CSS columns (`columns-2` up to `2xl:columns-5`), each tile sized by its
  own `aspect-ratio` with the picture covering it, so pictures of any shape fill their
  cards and nothing shifts as they load. Newest first, down each column. Hover or focus
  shows the prompt, download, the share menu and the red bin, which asks before
  `DeletePicture`.
- A click on a tile opens `UI.Dialog` as the picture and nothing else: no padding, no
  title (`Dialog.label` names it), no close button of its own, and as wide as the
  picture, which takes the most of the viewport its shape allows, up to twice its own
  pixels. Hover, keyboard focus or an open share menu lay the prompt, the settings, the
  same actions and "Close" over it. A touch screen has no hover: the picture is a button
  that pins them (`model.library.detailed`, off again for the next picture), and a
  question or a notice pins them too.
- Search is BM25 over the prompts, in `client/Bm25.elm` (`k1` 1.2, `b` 0.75): `shelve`
  rebuilds the index whenever the pictures change, and `Library.visible` ranks them for
  `model.query`, best first, newer first on a tie. Words are lower-cased and split at
  ASCII punctuation and spaces, and lose their accents (`Bm25.fold`: "Kraków" and
  "krakow" are one word, "ß" is "ss", a combining accent is dropped), in the prompts
  and in the query alike: Elm has no Unicode normalisation, so a Latin letter missing
  from `plain` is added there. Other alphabets are kept as they are. The last word of
  the query matches as a prefix, since it is still being typed, the others whole. Any word may match: more words rank, they
  don't filter.
- Sharing: a picture has no address to share, so the menu hands over the file. "Share…"
  is the device's share sheet (`SharePicture`, `navigator.share` with the file and the
  prompt), offered only when the `canShare` flag says the browser can; "Copy picture"
  is `CopyPicture` (`navigator.clipboard`); a network (`client/Share.elm`: X, Threads,
  Bluesky, LinkedIn, Reddit) copies the picture and opens the network's composer with
  the prompt (`OpenTab`, `window.open`), to paste it there. `client/share.ts` answers
  `PictureCopied` or `ShareFailed` with the error's name; `AbortError`, a closed share
  sheet, shows nothing. A new network is a `Network` variant: `compose` and
  `LibraryView.logo` don't compile until they handle it, and `Share.networks` lists it.

### elmcn components

shadcn/ui for Elm, copied into `client/UI/` and ours to change. Read a module for its API.

```sh
npx shadcn@latest search ricardocasares/elmcn -q dialog
npx shadcn@latest view ricardocasares/elmcn/dialog
npx shadcn@latest add ricardocasares/elmcn/dialog
mv -n src/UI/* client/UI/ && ls src/UI
```

- The registry installs into `src/UI/`. `mv -n` keeps existing (maybe edited) components;
  whatever `ls` still lists, merge by hand, then delete `src/`. `--diff` can't see
  `client/UI/`.
- Follow the CLI's notes: some items need an Elm package (`elm install` at the root) or
  a script import in `client/main.ts` (`UI/elmcn-dialog.ts`, the custom element that
  opens `UI.Dialog` and the sidebar's sheet). Strip their comments, keeping the
  `-- SECTION` headings.
- A class of ours that must win over a component's own takes Tailwind's `!`
  (`bg-red-600!`): the order of two classes in the attribute decides nothing.
- `UI.Select` opens on a solid `bg-popover`, not the registry's translucent, blurred
  one. `UI.DropdownMenu` is frosted, but less than the registry's: `bg-popover/80`
  over `backdrop-blur-xl`. Its items look like the sidebar's: medium, `rounded-lg`
  pills as tall as a sidebar item, muted until hovered, focused or checked, in a menu
  whose corners (0.875rem) are the pill's plus its `p-1.5`. `minHeight` adds up the
  same sizes (`itemHeight`, `menuPadding`, `separatorHeight`): change them together.
  Adding either component again would undo this.
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
- `init` and `update` return an `Effect Msg` (`Effect.elm`), never a `Cmd`: `Batch`,
  `SendToJs` (a response, a log or a signature to check), `Database`, a `Neon.Request`
  whose message also takes the latency in ms and may be an `Err Failure`, `Http`, a
  request record like a `Neon.Request`, and `Succeed`, a message to deliver straight
  away. `Main.perform` turns it into a `Cmd`, and a failed query into
  `DatabaseFailed`, answered once with a 503; the tests simulate it (see
  [Tests](#tests)).
- `Main` is wiring: `init`, `update`, `perform`, and `handle`, the one match on
  `Api.Route` for a running server (`brokenAnswer` is the one for a broken server). Each
  flow is a module in `api/src/Flow/` with its own `Msg`, a function per route that
  returns a `Reply Msg`, and an `update : Server -> Msg -> Effect Msg`; `Main.Msg` wraps
  each flow's `Msg`. A new route is a branch in `Main.handle`, one in `brokenAnswer`,
  and a function in the flow it belongs to; a new flow is a new module and a new
  `Main.Msg` variant. Flows never import `Main` or each other.
- The model is set once: `Running` with a `Server` (the connection, the origin, the
  release and the mailer), or `Broken` with the reason (flags that don't decode, an origin
  that is no URL, no database, or no mailer on Vercel). A broken server answers 503 to
  every route but health, which fails with the reason, and still answers 404 and 405.
- `dev.ts` reloads the server on any `.elm`/`.ts` change in `api/` or `shared/`
  (restarting from `init`); `preview` runs `dist/server/app.js`.
- Routes are real URLs under `/api`: entry points pass requests through unchanged.
- Errors are `{ "error": "..." }` (`Api.encodeError`). JSON is `cache-control: no-store`.
  Without `DATABASE_URL` or `API_DATABASE_PASSWORD` the server starts anyway and the health
  check fails.
- Answers are built in `Response.elm` (`Response.json`, `Response.error`,
  `Response.withHeader`); the `session` cookie is read, set and cleared only in
  `SessionCookie.elm`, which also holds its 30 days.
- Logging: `log Interop.Info|Warn|Error message`; `app.ts` calls `console[level]` with the
  message unchanged, which Vercel reads as the level. Database errors log at `error`.
- `GET /api/health` (IETF health check draft, `application/health+json`) runs one
  transaction with a 5 s `Neon.withTimeout`: `Db.rateLimit` on `health:ip:<x-real-ip>`
  (60 per minute), then `Db.ping`. An address over its limit gets 429 with
  `retry-after`, as JSON. Otherwise it answers `pass` with 200, `warn` with 200 when
  slower than `slowAfter` (1 s), and `fail` with 503, also when the server's role is a
  superuser, `BYPASSRLS` or a table owner. `output` is a short public reason; the full
  error is only logged, since Postgres messages can name roles and tables. Latency
  includes the rate limit, the hop to Neon and any cold start.
- `POST /api/auth/signup` (`Api.signUp`, `{ "email", "username" }`) starts a sign-up: it
  answers 202 with `{}` once the magic link is emailed, and the user exists only when the
  link is consumed. Three steps, each a `Msg`: both rate limits in one transaction
  (`magic-link:ip:<x-real-ip>` 10 and `magic-link:email:<email>` 3 per 15 minutes, in that
  order), `Db.createSignUpLink` (15 minutes), then the mailer's effect.
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
- `SignUp.parse` gives an `Email` and a `Username`, and `Email.parse` checks a
  sign-in's email: the only way to make either. `Db.createSignUpLink` and
  `Db.createSignInLink` take nothing else, so an unchecked address doesn't compile.
  `Mailer.send` takes an `Email` as well, and `Api.Account` and `Api.Verification` carry
  `Email` and `Username`: `Db` reads them with `Email.decoder` and `Username.decoder`,
  which parse. Request bodies (`Api.SignUp`, `Api.SignIn`, `Api.Assertion`,
  `Api.Attestation`) stay raw strings, so the server can tell a 400 from a 422.
- Both emails link to `<origin>/verify#<token>`: in the fragment, so the token
  reaches no server log. For the same reason the verify routes take it as
  `authorization: Bearer <token>`, and answer 401 without it.
- The two secrets have their own types: a `SessionToken` comes from the `session` cookie
  (`SessionCookie.read`) or new from `Db`, a `LinkToken` from the bearer header (`Request.linkToken`) or new
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
  and whether it is https (the cookie's `Secure`). `Origin.parse` rebuilds it from the
  scheme, host and port: a trailing slash, an upper-case host and a default port are
  dropped, and a path, query or fragment is refused. One that is no URL logs `Bad origin` at
  `error` and leaves the server `Broken`.
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
- `DELETE /api/auth/passkeys/<id>` (`Api.deletePasskey id`, with a `CredentialId`) removes one of the account's
  passkeys (`Db.removePasskey`, under `own_rows`): 200 with `{}`, or 404. It is the only
  route with a parameter; at `/api/auth/passkeys/options` the method tells it from
  `Api.PostPasskeyOptions`.
- Adding and removing a passkey email the account (`notify`): the answer waits for Brevo,
  since nothing may run after it on Vercel, but is the same when the email fails (logged
  at `error`). Removing a passkey leaves its sessions alone.
- The report's `releaseId` (`VERCEL_GIT_COMMIT_SHA`, else `VERCEL_DEPLOYMENT_ID`),
  `region` (`VERCEL_REGION`, our own field) and `links.release` (GitHub commit URL from
  `VERCEL_GIT_*`) come from `app.ts`. Off Vercel they are `local`.

## Neon driver

- `Services/Neon.elm` is `neon()` from `@neondatabase/serverless` in Elm: one HTTPS request per
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

- `Services/BrevoMailer.elm` is the mailer that sends:
  `BrevoMailer.mailer { apiKey, from }` builds a `Mailer` whose `send` is one `Http`
  effect, a POST of one transactional email to `BrevoMailer.endpoint`
  (`https://api.brevo.com/v3/smtp/email`), as Brevo's SDK does. The API key travels as
  the `api-key` header: server only. The sender is `from` (`EMAIL_FROM`, an address
  verified as a sender there) under the name `pixo`; the request has a 10 s timeout.
- It sends a template's `subject`, `html` and `text`, always together, to one
  recipient, with the envelope's tag. No Brevo-hosted templates, attachments,
  scheduling or batches.
- Any 2xx is `Ok ()`. Anything else is an `Err` with the reason as text, for the log:
  an error status with Brevo's `{ "code", "message" }` says the status, the code and
  the message; any other status keeps its body; a timeout and an unreachable network
  each have their own text. Nothing is retried.
- A template is a module in `api/src/Services/Mailer/` with one `email` function from its values
  to a `Services.Mailer.Layout.Message`. It lists `Layout` blocks (`Heading`, `Paragraph`, `Button`,
  `Footnote`) and `Layout.render` writes the HTML and the text from them, escaping every
  value: never build HTML in a template.
- Templates: `Services.Mailer.SignIn` and `Services.Mailer.SignUp`, for the two kinds of magic link;
  `Services.Mailer.PasskeyAdded` and `Services.Mailer.PasskeyRemoved`, which tell the account what happened
  to it and link to `<origin>/settings/account`.
- `Services/Mailer.elm` is the interface for sending email: a `Mailer` is a record
  with one job, `send`, from an `Envelope` (`to`, an `Email`, and `tag`) and a
  template's `Message` to an `Effect` whose message is an `Outcome`. It knows no
  provider, nothing of logs and nothing of how an email is sent.
  `Services/BrevoMailer.elm` builds one that posts to Brevo, and `Services/LogMailer.elm`
  one that logs the email and then `Succeed`s. A third provider is a third module with a
  `mailer`. Never compare two `Mailer`s with `==`: they hold functions.
- `Main.init` picks one from `BREVO_API_KEY`, `EMAIL_FROM` and the `local` flag:
  the Brevo mailer with both variables, `EMAIL_FROM` parsed as an `Email`; without
  them the log mailer on a local server (`local`, which `app.ts` sets when `VERCEL`
  is unset), so sign-up works on a fresh checkout and the magic link shows in the
  log, and `init` logs a warning saying so; on Vercel the server is `Broken`. `local`
  is never inferred from a request or the origin: a log mailer in production would
  write link tokens to the logs. Neither mailer knows why it was chosen.
- `Mailer.send mailer { to, tag } email toMsg` is the effect that sends one email
  to one recipient; a flow returns it as it is. `toMsg` takes a `Mailer.Outcome`:
  `Ok ()`, or the reason as text for the log. The tag names the template (`sign-in`)
  and shows in Brevo's logs.
- `POST /api/auth/signup` sends `Services.Mailer.SignUp`, or `Services.Mailer.SignIn` (see [Server](#server)).

## WebAuthn

- `WebAuthn.elm` checks a registration in Elm, with no port: `WebAuthn.register` takes the
  expected `Origin` and the browser's answer, and gives a `Registration` (the
  challenge to use up, the credential id, the public key, the counter, the backup flags
  and the transports it knows) or the reason as text, for the log.
- The relying party's id is the origin's host (`Origin.rpId`), so a passkey belongs to
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
  `main`'s `subscriptions`. Both match `Effect.Effect` without a wildcard, so a new variant
  doesn't compile until `perform` and `simulateEffects` handle it.
- A new endpoint, query, email or `FromElm` variant needs no new simulation: reuse
  `Request`, `Database`, `Http` and `SendToJs`. Add an `Effect` variant only for a new kind of command.
- Write response bodies and port values as the JSON on the wire, not with the `Api` or
  `Interop` encoders: the test then pins the format the other side relies on.
- Browser: act as a user does. `startAt "/signup"` opens a page; `fillIn` a field by
  its id and label, `clickButton` by its text (the `sr-only` label on an icon-only
  control), `clickLink`, then look for text in the view. `clickButton` takes any button
  that contains the text, and fails when two do: a button repeated per row gets an
  `aria-label` that names its row ("Download Bonsai binary, 1-bit"). Where a page
  and the sidebar both have the button, as "Try again" and "Sign out" at
  `/settings/account`, click inside one: `ProgramTest.within (Query.find [ tag "section" ])`.
- `/studio` sends `CheckWebGpu` and `/library` sends `LoadLibrary` as they open: a test
  that reads every port value starts at `/settings/server`, which sends none.
- Server: `request "first" "GET" [ "api", "health" ]` sends a request in through the port.
  Its first argument stands for the Promise's `resolve` and comes back in the response.
  Neon and Brevo are the only HTTP calls: answer Neon with
  `simulateHttpOk "POST" endpoint` and the rows, Brevo at `BrevoMailer.endpoint`.
  `advanceTime` moves the clock that `Database` measures latency with.
- A passing check of outgoing port values clears them: look at logs and responses in one
  `expectSent`, not one after the other.
- `tests/PipelineTest.elm` runs `Pipeline.update` with no program, for the rules a user
  can't reach from the page (an event out of turn, a slider off its grid), and
  `tests/LibraryTest.elm` does the same for `Library.update`; `tests/MainTest.elm` covers
  the pages, sending the library's and IndexedDB's events in through `interopToElm`. No
  test runs the library, the worker, IndexedDB, the clipboard or the share sheet: they
  need a browser.
- `tests/Bm25Test.elm` pins the search's ranking on prompts small enough to rank by
  hand, and `tests/ShareTest.elm` each network's address.
- `api/tests/NeonTest.elm` needs no program: a `Neon.Request` is a record, so `.body` is
  what would be sent and `.onResponse` decodes an `Http.Response`.
- `api/tests/BrevoMailerTest.elm` works the same way, on the `Http` effect the mailer's
  `send` returns. `api/tests/LogMailerTest.elm` covers the log mailer, and
  `api/tests/MailerTest.elm` the interface and the templates. A new template gets a
  test that its values reach the HTML and the text. The choice between the two
  mailers is tested through `init` in `MainTest.elm`.
- `api/tests/WebAuthnTest.elm` feeds `WebAuthn` attestations and assertions written by
  another encoder, byte by byte, for `https://pixo.pics`: one fixture per flag or fault.
  No test checks a real signature: `MainTest` answers `VerifySignature` itself, with
  `SignatureChecked`.
- `tests/ApiTest.elm` checks `Api.parse` against `Api.method` and `Api.path` for each
  route its `next` reaches, starting at `GetHealth`. `next` matches `Api.Route` without a
  wildcard, so a new route doesn't compile there until it is named: chain it after the
  last one.

## Vercel

The Vercel project `pixo.pics` (team `forkaround`) deploys from GitHub: a push to `main`
deploys production, a push to any other branch a preview. `vercel.json` configures it. The
Neon project `pixo` (`small-lab-61985339`, `aws-eu-central-1`) was created from the Vercel
Marketplace and is connected as the store `pixo`, which manages the variables. Keep
`regions` next to Neon's region: the function runs in `fra1`, Frankfurt, where
`aws-eu-central-1` is.

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
  `/api/*` rewrites to (Vercel keeps the original URL in `request.url`). Every other
  address that is not a file rewrites to `index.html`, so the browser app starts anywhere
  and answers an unknown address with its own not-found page (and a 200, not a 404).
  Bun runtime (`"bunVersion": "1.x"`, beta).
- Nothing is linked locally (`.vercel` and `.neon` are not committed): pass
  `--scope forkaround --project pixo.pics` to `vercel` and
  `--project-id small-lab-61985339` to `bunx neon@latest`. Use `--no-env-pull` with
  `neon link` or `neon checkout`, or they write Neon's `DATABASE_URL` into `.env`.
