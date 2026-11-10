# AGENTS.md

A Vite app, run by Bun: an Elm browser app and an Elm HTTP server sharing one API contract,
on Postgres (Neon), deployed to Vercel.

## Layout

| Path               | What                                                                       |
| ------------------ | -------------------------------------------------------------------------- |
| `client/`          | Browser app (root `elm.json`). `client/UI/`: elmcn components              |
| `shared/`          | Elm both sides compile: `Api` (routes, endpoints, codecs)                  |
| `api/src/`         | Server Elm (`api/elm.json`): `Main`, `Db`, `Neon` driver                   |
| `api/_ts/`         | Server TypeScript: `app.ts` (starts Elm), `xhr.ts`, `dev.ts` (Vite plugin) |
| `api/index.ts`     | The only Vercel Function                                                   |
| `migrations/`      | Graphile Migrate (`.gmrc`)                                                 |
| `.scripts/neon.sh` | Starts the local Neon proxy                                                |
| `.agents/`         | Agent skills, tracked by `skills-lock.json`; `.claude` links here          |

## Commands

```sh
bun dev                 # types + dev:proxy + dev:vite (:5173, Elm server on /api)
bun run dev:proxy       # local Neon proxy in Docker, then SELECT 1 with DATABASE_URL
bun run build           # types:api, types:app, check, then vite build (server, then client)
bun run preview         # the build on :4173
bun run types           # regenerate *.elm.d.ts after changing an InteropDefinitions.elm
bun check               # type-check both TypeScript projects
bun fmt                 # format (--check to only report)
bun db <command>        # graphile-migrate: migrate, watch, commit, uncommit, status, reset --erase
```

CI (`.github/workflows/ci.yml`) runs `bun fmt --check` and `bun run build`; it needs no
database.

## Rules

- **Bun, not npm**: `bun add -d <pkg>`.
- **No comments** in code or config. Exceptions: tool directives
  (`// @ts-expect-error`, `/// <reference ... />`) and generated or signed files (`.gmrc`,
  `*.elm.d.ts`, committed migrations).
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
  `return msg satisfies never;` when `FromElm` has several variants, but
  `return msg.tag satisfies never;` when it has one, as in `client/`: elm-ts-interop then
  emits a plain object, not a union, and TypeScript narrows only `msg.tag`. Switch forms
  when the count crosses one.
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
  starts with `_`. Server TypeScript lives in `api/_ts`; `app.ts` uses Web APIs only.

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
| `SHADOW_DATABASE_URL`   | `bun db watch` and `commit`                                  |
| `ROOT_DATABASE_URL`     | `bun db reset`, the proxy's own connection                   |

- Bun loads `.env`, then `.env.local`, then the shell wins, but only when it runs a file
  itself (`bun <file>`): that's why `dev:vite`, `preview` and `db` call
  `bun node_modules/...`. Plain `vite`, `bunx` or `bun --bun <bin>` run as `node` without
  `.env`, and every database route answers 503. `bun run` doesn't pass `.env` to scripts,
  so `neon.sh` reads the files itself, in the same order.
- `vercel env pull` writes `.env.local`, pointing local runs at Neon's `vercel-dev` branch.

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
- Consuming a sign-up link fails on `users_username_key` when the name was taken
  meanwhile: the statement rolls back and the link stays. Two sign-up links for one email
  consumed at the same instant fail the same way, so retry once before answering "taken".
- Tokens are never stored: `token_hash` is `sha256(convert_to($1, 'UTF8'))`, computed by
  Postgres from the token passed as a parameter.
- WebAuthn values (`credential_id`, `public_key`, `challenge`) are unpadded base64url text,
  as on the wire. `sign_count` is a `bigint`: it arrives as text.
- The driver can't read, then decide, inside a transaction: every check-then-change is one
  statement. A link or a challenge is used once by
  `DELETE ... WHERE ... AND expires_at > now() RETURNING`; a passkey's counter moves by
  `UPDATE ... WHERE $2 > sign_count OR ($2 = 0 AND sign_count = 0) RETURNING`.
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
  secret as an argument and filters by it. There are `authenticate`, `rate_limit` and
  `delete_expired`; consuming a link, passkey sign-in and creating a session are not
  written yet.
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
- Calls the server only through `ApiClient.request` with an `Api.Endpoint`. On an error
  status it decodes `Api.errorDecoder`, then the endpoint's response (the health report
  comes with a 503).
- The page: the Server card (one row per `Api.checks`, release and region in the
  footer) and a theme toggle (`UI.Tabs` without panels, saved to `localStorage` via the
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
- The report's `releaseId` (`VERCEL_GIT_COMMIT_SHA`, else `VERCEL_DEPLOYMENT_ID`),
  `region` (`VERCEL_REGION`, our own field) and `links.release` (GitHub commit URL from
  `VERCEL_GIT_*`) come from `app.ts`. Off Vercel they are `local`.

## Neon driver

- `Neon.elm` is `neon()` from `@neondatabase/serverless` in Elm: one HTTPS request per
  query or transaction; `Neon.run` and `Neon.commit` return `Task`s. It uses `elm/http`,
  so `app.ts` imports `xhr.ts` first: Bun, Node and Vercel have no `XMLHttpRequest`.
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

## Vercel

The Vercel project `elm-neon-template` deploys from GitHub: a push to `main` deploys
production, a push to any other branch a preview. `vercel.json` configures it. The Neon
project `elm-neon` is attached through Neon's Vercel integration (installed from the Neon
console, not `vercel integration add neon`, which would provision a new database), which
manages the variables. `.vercel` and `.neon` are local links, not committed. Keep
`regions` next to Neon's region.

| Vercel            | Neon branch                 | Variables                                  |
| ----------------- | --------------------------- | ------------------------------------------ |
| Production        | `main`                      | `DATABASE_URL`, `DATABASE_URL_UNPOOLED`    |
| Preview (`<git>`) | `preview/<git>`, off `main` | same, scoped to the git branch             |
| Development       | `vercel-dev`                | same, for `vercel env pull` (avoid, below) |

- Each preview gets its own Neon branch: the integration cancels the first build, creates
  the branch and its variables, then builds again. Pushing a commit that's already on
  another branch triggers nothing.
- Build: `DATABASE_URL="$DATABASE_URL_UNPOOLED" bun db migrate && bun run build`. Each
  deployment migrates its own branch, so a preview's migrations never reach `main`; a
  missing variable fails the build.
- The `api` role is created once on Neon's `main`, in SQL as the owner, because a role made
  in the console, CLI or API is `BYPASSRLS`:
  `CREATE ROLE api LOGIN PASSWORD '<random>'; ALTER ROLE api SET search_path = public, pg_temp;`.
  Its password is `API_DATABASE_PASSWORD`, a Vercel secret in Production and Preview. A
  branch copies its parent's roles and passwords, so previews need nothing more, unless
  `main` is a protected branch: its children get new passwords. A branch older than the
  role fails its build at `GRANT ... TO api`: reset it as in [Migrations](#migrations).
- The function's environment still holds the owner's `DATABASE_URL`: RLS contains the
  server's queries, not a compromised process.
- `build` runs the Elm steps one at a time: on a fresh machine, two processes filling
  `~/.elm` at once corrupt it ("PROBLEM BUILDING DEPENDENCIES"), and elm-ts-interop still
  exits 0. `types:api` must `cd api`, as elm-ts-interop uses the nearest `elm.json`.
- `dist/client` is served; `dist/server` is bundled into `api/index.ts`, which every
  `/api/*` rewrites to (Vercel keeps the original URL in `request.url`). Bun runtime
  (`"bunVersion": "1.x"`, beta).
- Don't `vercel env pull`: it writes the `vercel-dev` connection into `.env.local`, which
  overrides `.env` and points local runs at Neon instead of the local database.
- `neon` CLI: `bunx neon@latest`. Use `--no-env-pull` with `neon link` or
  `neon checkout`, or they write Neon's `DATABASE_URL` into `.env`.
