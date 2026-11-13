# AGENTS.md

A Vite app, run by Bun: an Elm browser app (`client/`, root `elm.json`) and an Elm HTTP
server (`api/`, its own `elm.json`) sharing one API contract (`shared/`), on Postgres
(Neon), deployed to Vercel.

This file holds what the code can't tell you: rules, reasons and traps. For the rest,
read the source: scripts in `package.json`, first-time setup in `README.md`, the whole
schema and every grant in `migrations/schema.sql`, routes in `shared/Api.elm`, pages in
`shared/Path.elm`.

## Commands

- `bun run test`, not `bun test`: that is Bun's own runner and finds nothing. One project,
  rerun on save: `bun run test:app --watch` (`test:api` for the server).
- `bun fmt-check`, not `bun fmt --check`: `bun run --parallel` takes `--check` for one
  more script to run, so `bun fmt --check` formats in place, then fails. For Bun's flags
  (a path, `--staged`...), run `bun fmt --no-script <flags>`. Markdown is checked too.
- `bun run types` after changing an `InteropDefinitions.elm`.
- `bun db <command>` is graphile-migrate.
- CI runs `bun fmt-check`, `bun run test` and `bun run build`; it needs no database.

## Rules

- **Bun, not npm**: `bun add -d <pkg>`.
- **No comments** in code or config. Exceptions: tool directives
  (`// @ts-expect-error`, `/// <reference ... />`) and generated or signed files (`.gmrc`,
  `*.elm.d.ts`, committed migrations).
- **Elm is formatted by elm-format**. A new Elm folder goes in `fmt:elm` and
  `fmt-check:elm`. A multi-line `exposing` list comes out one name per line, sorted:
  keeping groups takes `@docs` in a doc comment, which the rule above excludes.
- **`.env.example` mirrors `.env`**: same variables, same order, changed in the same
  commit, with values that work on a fresh checkout and never a secret. Vercel-only
  variables (`DATABASE_URL_UNPOOLED`) stay out of both.
- **SQL values are parameters**: never `++` a request value into SQL; pass `$n` with
  `Neon.text`, `Neon.int`... Only constants may be joined in. An identifier that depends
  on a request is picked from a fixed list.
- **Migrations**: write `migrations/current.sql` to rerun cleanly: undo, then redo (see
  [Migrations](#migrations)). Never edit `migrations/committed/`: they are hash-signed.
- **Agents never commit a migration**: no `bun db commit`. Write the file with `watch`
  stopped, run it with `bun db watch --once`, then ask for a review: the user commits.
- **Every table has row-level security** (see [Row-level security](#row-level-security)).
- **Keep the Elm projects apart**: the root `elm.json` never lists `api/` folders, so the
  database password can't reach the browser bundle. A package `shared/` imports goes in
  both `elm.json`s; server packages: `elm install` inside `api/`. Test packages:
  `bunx elm-test install <package>`, inside `api/` for the server.
- **Value types live in `Data`**: one module per type, the type exposed without its
  constructor, made only by that module's `parse`, `fromString` or `decoder`.
  `shared/Data/` for a value that crosses the wire or sits in a `Path`, `api/src/Data/`
  for one only the server holds, `client/Data/` for one only the browser holds. Import as
  `import Data.Email as Email exposing (Email)`. Drivers of outside services live in
  `api/src/Services/`, imported as `import Services.Neon as Neon`.
- **No wildcards over `Api.Route`, `Path`, `Effect` or `FromElm`**: a new variant must
  fail to compile until every place handles it. Keep `Api.parse` in step with
  `Api.method` and `Api.path`.
- **Every `FromElm` is handled**: the port handlers (`api/_ts/app.ts`, `client/main.ts`,
  the pipeline worker) end `switch (msg.tag)` with a `default` that is `never`:
  `return msg satisfies never;` when `FromElm` has several variants, as now, but
  `return msg.tag satisfies never;` when it has one: elm-ts-interop then emits a plain
  object, not a union, and TypeScript narrows only `msg.tag`. Switch forms when the count
  crosses one.
- **All application logic belongs in Elm**, including date parsing, formatting and
  presentation decisions. TypeScript is limited to interop with browser and platform
  APIs, and decides nothing: a new option or rule goes in Elm.
- **UI**: use the elmcn registry component instead of writing one; Phosphor
  (`phosphor-icons/phosphor-elm`, `Regular`; `Bold` beside medium text: the sidebars'
  menus and trigger, the user menu, the theme toggle, the login and sign-up forms)
  instead of hand-written SVG; `sr-only` text on icon-only controls.
- **Skeletons faithfully represent the final element**: match its structure, dimensions,
  spacing and alignment, so replacing the skeleton with loaded content causes no layout
  shift. Update the skeleton whenever the final element changes. The same holds for what
  depends on the session: while it is `Unknown`, draw it `invisible` or as a skeleton of
  the same size.
- **Elm steps in `build` run one at a time**: keep `build` as
  `bun run --sequential types:* check build:*`, never `types` (which runs both
  generators in parallel). On an empty `~/.elm`, as on every fresh Vercel machine, two
  Elm processes downloading packages at once corrupt the cache ("PROBLEM BUILDING
  DEPENDENCIES"), and elm-ts-interop still exits 0. It's a race, so it may pass locally
  and still fail on Vercel. `types:api` must `cd api`: elm-ts-interop uses the nearest
  `elm.json`.
- **Vercel functions**: every `.ts` under `api/` becomes one unless its name or folder
  starts with `_`. Server TypeScript lives in `api/_ts`; `app.ts` uses Web APIs only
  (`crypto.subtle`, `atob`: their types come from `"WebWorker"` in `api/tsconfig.json`).

## Commits

```text
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

## Environment and local database

- Bun loads `.env`, then `.env.local`, then the shell wins, but only when it runs a file
  itself (`bun <file>`): that's why `dev:vite`, `preview` and `db` call
  `bun node_modules/...`. Plain `vite`, `bunx` or `bun --bun <bin>` run as `node` without
  `.env`, and every database route answers 503. `bun run` doesn't pass `.env` to scripts,
  so `.scripts/neon.sh` reads the files itself, in the same order.
- `DATABASE_URL` is the owner's, for migrations; the server takes only its host and
  database, and connects as `api` with `API_DATABASE_PASSWORD`. Nothing comes from
  Vercel: its Development environment has no database variables.
- `BREVO_API_KEY` is empty locally, so emails are printed: the magic link is in the
  server's log.
- The app reaches the local Postgres through Neon's HTTP proxy in Docker
  (`NEON_ENDPOINT`), as production reaches Neon: ~50 ms per query. The proxy's own
  connection must be a superuser (`ROOT_DATABASE_URL`); with the app role every query
  fails with "Control plane request failed".
- The proxy holds connections open, so `bun db reset --erase` needs
  `docker stop neon-http-proxy` first.
- `/api/health` runs `rate_limit`, so it fails until the database is migrated.

## Migrations

- `current.sql` runs whole, again and again: `bun db watch` reruns it on every save, each
  run in one transaction, and `bun db commit` replays every committed migration on an
  empty shadow database, then runs the new one once more locally. Each run must end in
  the same state, whether the database has none of the file, all of it, or an earlier
  draft.
- Undo, then redo. `CREATE ... IF NOT EXISTS` is not enough: once the object exists, edits
  to its definition are skipped, and the local database drifts from what `commit` builds.
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
- The undo also runs once in every other database: undo only what this migration adds. A
  `DROP TABLE` of an earlier migration's table empties it in production.
- `CASCADE` also drops dependents elsewhere (views, foreign keys, triggers, policies):
  recreate them after the redo. `commit` dumps the shadow schema to
  `migrations/schema.sql`; its diff shows what went missing.
- An object that leaves the file stays in the local database: a rename, a removal, or a
  typo that ran on save. Keep its `DROP ... IF EXISTS`; elsewhere it does nothing.
- Previews run only committed migrations, so the user commits before pushing a branch
  that needs its schema. Each migration names the previous one's hash: if `main` gains a
  migration first, `bun db uncommit`, rebase, `bun db watch --once`, `bun db commit`. Git
  may merge two migrations without a conflict (different `-m` names) and every `migrate`
  then fails. The preview's Neon branch ran the old hash; reset it:
  `bunx neon@latest branches reset preview/<git> --parent`.

## Database

- The Neon driver can't read, then decide, inside a transaction: every check-then-change
  is one statement. A link or a challenge is used once by
  `DELETE ... WHERE ... AND expires_at > now() RETURNING`; a passkey's counter moves by
  `UPDATE ... WHERE $2 > sign_count OR ($2 = 0 AND sign_count = 0) RETURNING`.
- Tokens are never stored: `token_hash` is `sha256(convert_to($1, 'UTF8'))`, computed by
  Postgres from the token passed as a parameter, and a new token is returned once.
- A `users` row means a proven email: consuming a magic link creates the user, never the
  sign-up request.
- No answer tells whether an email has an account: sign-up checks the username first, an
  email that has a user gets a sign-in link instead, and one without gets no email and
  the same 202.
- Postgres can't check a signature: `sign_in_with_passkey` trusts that the server did.
  It is the one function that takes no secret.
- WebAuthn values are unpadded base64url text, as on the wire. `sign_count` is a `bigint`:
  it arrives as text.
- `delete_expired()` exists, and nothing schedules it yet.

### Row-level security

- Migrations run as the database owner, which is `BYPASSRLS`. The server connects as
  `api`, which is not and owns nothing.
- Each table `api` is granted has the policy `own_rows`: `USING` and `WITH CHECK` compare
  the row's user to `(SELECT NULLIF(current_setting('app.user_id', true), '')::uuid)`.
  With nobody signed in that is `NULL`: reads find no rows and writes fail with `42501`.
- `authenticate(token)` sets `app.user_id` until the transaction ends; `Db.signedIn token`
  starts a `Neon.Transaction` with it. Under `Neon.run` the setting ends with its
  statement. Nothing else sets `app.user_id`: the policies trust it.
- Work before sign-in or across users is a function: `SECURITY DEFINER`,
  `SET search_path = public, pg_temp`, `REVOKE EXECUTE ... FROM PUBLIC`,
  `GRANT EXECUTE ... TO api`. It runs as the owner, past every policy, so it takes the
  secret as an argument and filters by it. `api` can't read `sessions.token_hash`,
  `magic_links` or `rate_limits`, nor execute `new_token()`: functions only.
- A new table: `ALTER TABLE t ENABLE ROW LEVEL SECURITY, FORCE ROW LEVEL SECURITY`,
  `CREATE POLICY`, the narrowest `GRANT`s, right after its `CREATE TABLE`
  (`DROP TABLE ... CASCADE` drops them too), and an index on its user column. Never grant
  `TRUNCATE`, `REFERENCES` or `TRIGGER`. A view needs `WITH (security_invoker = true)`.
- `migrations/check-rls.sql` runs after `migrate` and after `current.sql` and fails them
  over what it can see. It can't tell a grant that is too wide: read the grants in
  `migrations/schema.sql`'s diff.
- Try policies as `api`, never as the owner, who skips them:
  `psql postgres://api:api@localhost:5432/app`, then
  `BEGIN; SELECT authenticate('<token>'); ...`.

### Rate limiting

- One policy per key, each a function in `api/src/RateLimit.elm` that takes what it limits
  (`ClientAddress`, `Email`); a key is written nowhere else. Name it after what it limits
  and whom, like `magic-link:email:<email>`. At most 512 bytes.
- Several keys in one transaction (`Neon.commit`): always in the same order, or two
  requests deadlock.
- `rate_limits` is a logged table on purpose: Neon drops unlogged tables when the compute
  suspends, which would reset every window.

### Neon driver

- `Services/Neon.elm` uses `elm/http`, so `app.ts` imports `xhr.ts` first: Bun, Node and
  Vercel have no `XMLHttpRequest`.
- Rows are JSON objects by column name. `boolean`, `smallint`, `integer`, `real`,
  `double precision`, `json`, `jsonb` and arrays arrive as JSON; `NULL` as `null`;
  everything else (`bigint`, `numeric`, `timestamptz`...) as Postgres's text. Duplicate
  column names keep the last: use `AS`.
- Parameters travel as text and Postgres converts them; `Neon.json` for `json`/`jsonb`.
- The connection string holds the password: server only.

## Server

- `init` and `update` return an `Effect Msg` (`api/src/Effect.elm`), never a `Cmd`:
  `Main.perform` turns it into one, and the tests simulate it.
- `Main` is wiring. Each flow is a module in `api/src/Flow/` with its own `Msg`, a
  function per route that returns a `Reply Msg`, and an `update`. A new route is a
  branch in `Main.handle`, one in `brokenAnswer`, and a function in its flow; a new flow
  is a new module and a new `Main.Msg` variant. Flows never import `Main` or each other.
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

## Browser app

- `init` and `update` return an `Effect Msg` (`client/Effect.elm`), never a `Cmd`. The
  `Navigation.Key` is not in the model: `main` keeps it beside it and hands it to
  `perform`.
- `Main` is wiring, the shell and what every page shares: the session, the theme, the
  pipeline and the library. A page with state of its own is a module in `client/Page/`.
  A new one is that module, a `Path` variant with its line in `Path.parse`'s parser, a
  `Main.Msg` variant, and its branches in `open`, `update`, `title` and `viewBody`.
  Pages never import `Main` or each other; what several share is a module beside `Main`.
- Only `Main` writes `model.session`. A page that changes who is signed in exposes
  `outcome`, which tells it from the message alone, and `Main` applies it whether or not
  the page is still open, so an answer that arrives late still signs in or out. Any
  other message for a page that is no longer open changes nothing and runs no effect.
- `open` starts a page's model anew, which is also what ends an unfinished passkey
  ceremony.
- A session check that fails other than with a 401 is `Unavailable`: the page says so and
  offers "Try again", and sends nobody to `/login`.
- The server is called only through `ApiClient.request` with an `Api.Endpoint`. On an
  error status it decodes `Api.errorDecoder`, then the endpoint's own response: the
  health report comes with a 503.
- Addresses are written only with `Path.toString`: links, `PushUrl`, the server's emails.
- `client/main.css` is the only stylesheet, linked from `index.html`: don't import CSS
  from TypeScript. Inter is served with the app; Explore's Unsplash photos are the only
  thing a page asks another site for.
- Inputs with a placeholder keep their label, `sr-only`: screen readers and the tests'
  `fillIn` need it.
- Text over a picture is white on a dark gradient whatever the theme, and never sits
  where the picture fades into the page (white on white in the light theme). Buttons
  over a picture take their looks from `client/Overlay.elm`.
- Explore's first screen for a signed-out visitor stays plain: no badge over the
  heading, no gradient text, no list of ticks, nothing that moves.
- `client/Logo.elm` is the one SVG drawn by hand. Elm interpolates between its expanded
  and collapsed paths, which must have the same commands and coordinate count. Its paths
  run along the middle of a 5-unit stroke and the view box leaves room for it: change
  the stroke and both change. `public/favicon.svg` copies the X's path: a change goes in
  both. The sidebar's animation is Elm frames driving `--sidebar-progress`, with no CSS
  transition or TypeScript.

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
- `UI.Sidebar`, `UI.Select` and `UI.DropdownMenu` are changed from the registry's: adding
  them again would undo it, so merge by hand. `DropdownMenu.minHeight` adds up
  `itemHeight`, `menuPadding` and `separatorHeight`: change them together.
- Follow the CLI's notes: some items need an Elm package (`elm install` at the root) or
  a script import in `client/main.ts`. Strip their comments, keeping the `-- SECTION`
  headings.
- A class of ours that must win over a component's own takes Tailwind's `!`
  (`bg-red-600!`): the order of two classes in the attribute decides nothing.

### Image pipeline

The studio runs
[bonsai-image-pipeline-lib](https://github.com/nishlumi/bonsai-image-pipeline-lib)
(WebGPU), pinned to a commit in `package.json`, in a web worker
(`client/pipeline.worker.ts`). `client/Pipeline.elm` decides everything; the server takes
no part.

- `Main` keeps the pipeline in `model.pipeline`, not in a page: the model stays in the
  graphics card's memory across pages.
- An event is applied only in the state that waits for it, so one that arrives late
  changes nothing. The library runs one job at a time: a cancel waits for it to stop
  (`PipelineFailed` named `AbortError`, which shows no message). A graphics card that
  drops the device mid-picture rejects with the same name, and that one is shown: the
  model in memory is then unusable until released and loaded again.
- The library can't tell whether its cache holds a whole model, so
  `model.pipeline.downloaded` (in `localStorage`) lists the weights that finished
  loading. Models share one cache: "Remove downloaded models" removes them all, and
  works only with no model in memory.
- `Weights` lists only `-mlx-` models: `-gemlite-` ones are for PyTorch and answer 404.
  `Pipeline.weightsSize` was read from a real download: change it with the list.
- What the library takes: a `Dimension` in multiples of 16 (ours: 256 to 1024 in steps
  of 64, and a preset must be on that grid), a seed read as 32 bits, `guidanceScale` 1
  and nothing else.
- A worker frees the page's thread, not the graphics card, which also draws the page.
  The library submits some 350 passes a step without waiting for the card, and the
  browser's frames queue behind them: stalls of 1.4 s. So `pace` in the worker wraps the
  library's `runtime.host.submit` and awaits `onSubmittedWorkDone` after each batch,
  sized to `frameBudget` (always `Pipeline.frameBudget`, 8 ms). It reaches into the
  library: check it when the pinned commit moves. It can't split one pass: at 1024 by
  1024 the page still stutters, and only the library's shaders could change that.
- To measure a stall, time a 1 by 1 WebGL `readPixels` on the page while the worker
  generates: idle it takes under 1 ms. `requestAnimationFrame` tells nothing: it keeps
  its rhythm while nothing on the page changes.
- The library reports progress some 300 times a second; the worker sends on one report
  every 100 ms.
- The worker answers with the PNG as a `Blob`, and `main.ts` makes the object URL: one
  made in the worker would not outlive it.
- WebGPU is asked for inside the worker: a browser that has it only on the page can't
  make pictures.
- A random seed is drawn in Elm from the `entropy` flag, with no `Effect`: a test fixes
  it through the flags.
- `client/bonsai.d.ts` types the library, which ships no types: keep it to what the
  worker calls. The worker imports `Main.elm` for its types only: Elm never runs there.

### Library

Saved pictures live in the browser's IndexedDB; `client/Library.elm` decides everything,
down to the database's name and a row's fields. `client/Idb.elm` and `client/idb.ts` are
IndexedDB for Elm and know nothing of pictures.

- Browsers already hold the database: its name (`pixo`), its version, the store
  (`pictures`), its key (`id`) and the names of a row's fields can't change without
  losing the pictures people saved. A new field is decoded with a default.
- Elm can't hold a `Blob`: `Idb.Add` takes `blobs`, a field and an object URL each, and
  the driver stores the `Blob` behind the URL in that field. A `Blob` it reads comes
  back as a new object URL in the same field, which `Idb.Release` revokes.
- There is no command to open a database: every `Idb.Command` carries the `Database` it
  is for, and the driver opens it on first use. Port messages sent from one `update`
  reach TypeScript in the reverse of the order they were listed in, so no two may
  depend on their order.
- Every `Idb.Event` goes to the library, the driver's only user
  (`InteropDefinitions.answered`): a second user needs them told apart, by `label`.
- A stored row that doesn't parse into a `Picture` is left out, not an error.
- Search (`client/Bm25.elm`) folds accents by hand, since Elm has no Unicode
  normalisation: a Latin letter missing from `plain` is added there.
- A picture has no address to share, so sharing hands over the file: a network copies
  the picture and opens its composer, to paste it there. An `AbortError` from the share
  sheet is a closed sheet, and shows nothing.

## Tests

- Each project's `MainTest.elm` drives its whole `Main` with
  [elm-program-test](https://elm-program-test.netlify.app/). `simulateEffects` mirrors
  `Main.perform` and `simulateSubscriptions` mirrors `main`'s `subscriptions`.
- A new endpoint, query, email or `FromElm` variant needs no new simulation: reuse
  `Request`, `Database`, `Http` and `SendToJs`. Add an `Effect` variant only for a new
  kind of command.
- Write response bodies and port values as the JSON on the wire, not with the `Api` or
  `Interop` encoders: the test then pins the format the other side relies on.
- Browser: act as a user does: `fillIn` by id and label, `clickButton` by its text (the
  `sr-only` label on an icon-only control), `clickLink`, then look for text in the view.
- `clickButton` and `clickLink` fail when two elements match. A button repeated per row
  gets an `aria-label` that names its row; where a page and the sidebar both have one,
  as "Try again" and "Sign out" at `/settings/account`, click inside one:
  `ProgramTest.within (Query.find [ tag "section" ])`. At `/` there are two "Sign up".
- `/studio` sends `CheckWebGpu` and `/library` sends `IdbGetAll` as they open: a test
  that reads every port value starts at `/`, which sends none.
- A passing check of outgoing port values clears them: look at logs and responses in one
  `expectSent`, not one after the other.
- Server: the first argument of `request` stands for the Promise's `resolve` and comes
  back in the response. Neon and Brevo are the only HTTP calls: answer them with
  `simulateHttpOk`. `advanceTime` moves the clock that latency is measured with.
- A `Neon.Request` and a mailer's `Http` effect are records: test `.body` and
  `.onResponse` with no program.
- Rules a user can't reach from the page (an event out of turn) are tested on the
  `update` alone: `PipelineTest`, `LibraryTest`, `EnrolmentTest`.
- `tests/ApiTest.elm` walks the routes with `next`: chain a new route after the last one.
- A new email template gets a test that its values reach the HTML and the text.
- `api/tests/WebAuthnTest.elm` uses fixtures written by another encoder, byte by byte,
  for `https://pixo.pics`: one per flag or fault.
- No test runs the library, the worker, IndexedDB, the clipboard, the share sheet or a
  real signature: they need a browser.

## Vercel

The Vercel project `pixo.pics` (team `forkaround`) deploys from GitHub: `main` to
production, any other branch to a preview. The Neon project `pixo` (`small-lab-61985339`,
`aws-eu-central-1`) is connected as the Marketplace store `pixo`, which manages
`DATABASE_URL` and `DATABASE_URL_UNPOOLED`.

- Nothing is linked locally: pass `--scope forkaround --project pixo.pics` to `vercel` and
  `--project-id small-lab-61985339` to `bunx neon@latest`. Use `--no-env-pull` with
  `neon link` or `neon checkout`, or they write Neon's `DATABASE_URL` into `.env`.
- A preview has its own Neon branch (`preview/<git>`, off `main`) only because the
  store's connection has "Create database branch for deployment" ticked for Preview. It
  is off by default, and without it a preview build migrates `main`, which is production.
  The option is in the dashboard only (Storage, `pixo`, Connect Project); to check it:
  `vercel api "/v1/storage/stores?teamId=forkaround"` lists `preview` under
  `projectsMetadata[].deployments.actions`.
- A preview's variables override the project's for that deployment and don't show in the
  project's settings.
- Previews sit behind Vercel Authentication: request them with `vercel curl <url>` from a
  folder linked to the project.
- The build migrates its own branch first
  (`DATABASE_URL="$DATABASE_URL_UNPOOLED" bun db migrate && bun run build`).
- The `api` role was created once on Neon's `main`, in SQL as the owner, because a role
  made in the console, CLI or API is `BYPASSRLS`:
  `CREATE ROLE api LOGIN PASSWORD '<random>'; ALTER ROLE api SET search_path = public, pg_temp;`.
  Its password is `API_DATABASE_PASSWORD`, a Vercel secret, as is `BREVO_API_KEY`. A
  branch copies its parent's roles and passwords, unless `main` is a protected branch:
  its children get new passwords. A branch older than the role fails its build at
  `GRANT ... TO api`: reset it as in [Migrations](#migrations).
- The function's environment still holds the owner's `DATABASE_URL`: RLS contains the
  server's queries, not a compromised process.
- Keep `regions` in `vercel.json` next to Neon's region: `fra1` for `aws-eu-central-1`.
- Every `/api/*` rewrites to `api/index.ts`, the only function; every other address that
  is not a file rewrites to `index.html`, so an unknown address gets the app's own
  not-found page with a 200, not a 404.
