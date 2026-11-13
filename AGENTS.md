# AGENTS.md

A Vite app, run by Bun: an Elm browser app (`client/`, root `elm.json`) and an Elm HTTP
server (`api/`, its own `elm.json`) sharing one API contract (`shared/`), on Postgres
(Neon), deployed to Vercel.

This file holds what the code can't tell you: rules, reasons and traps. For the rest,
read the source: scripts in `package.json`, first-time setup in `README.md`, the whole
schema and every grant in `migrations/schema.sql`, routes in `shared/Api.elm`, pages in
`shared/Path.elm`.

## Read before editing

Each area has its own guide, with the reasons behind its rules. Read it before changing
anything there.

| Editing                                                       | Read                                                 |
| ------------------------------------------------------------- | ---------------------------------------------------- |
| `api/`: flows, answers, queries, emails, server tests         | [api/AGENTS.md](api/AGENTS.md)                       |
| `client/`: pages, session, ports, UI                          | [client/AGENTS.md](client/AGENTS.md)                 |
| `tests/`: browser tests                                       | [tests/AGENTS.md](tests/AGENTS.md)                   |
| `migrations/`, any SQL, row-level security                    | [migrations/AGENTS.md](migrations/AGENTS.md)         |
| `.env`, running the app or the database locally, the proxy    | [docs/agents/local-env.md](docs/agents/local-env.md) |
| Vercel, previews, Neon branches, production roles and secrets | [docs/agents/vercel.md](docs/agents/vercel.md)       |

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
- **Migrations are the user's to commit**: write `migrations/current.sql`, run it with
  `bun db watch --once`, and ask for a review: never `bun db commit`, never edit
  `migrations/committed/`. Every table has row-level security. Both in
  [migrations/AGENTS.md](migrations/AGENTS.md).
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
- **Elm steps in `build` run one at a time**: keep `build` as
  `bun run --sequential types:* check build:*`, never `types` (which runs both
  generators in parallel). On an empty `~/.elm`, as on every fresh Vercel machine, two
  Elm processes downloading packages at once corrupt the cache ("PROBLEM BUILDING
  DEPENDENCIES"), and elm-ts-interop still exits 0. It's a race, so it may pass locally
  and still fail on Vercel. `types:api` must `cd api`: elm-ts-interop uses the nearest
  `elm.json`.
- **`Effect`, not `Cmd`**: in both Elm programs `init` and `update` return an `Effect Msg`
  (`api/src/Effect.elm`, `client/Effect.elm`), never a `Cmd`: `Main.perform` turns it into
  one, and the tests simulate it. `Main` is wiring: each guide says what goes where.

## Tests

- Each project's `MainTest.elm` drives its whole `Main` with
  [elm-program-test](https://elm-program-test.netlify.app/). `simulateEffects` mirrors
  `Main.perform` and `simulateSubscriptions` mirrors `main`'s `subscriptions`.
- A new endpoint, query, email or `FromElm` variant needs no new simulation: reuse
  `Request`, `Database`, `Http` and `SendToJs`. Add an `Effect` variant only for a new
  kind of command.
- Write response bodies and port values as the JSON on the wire, not with the `Api` or
  `Interop` encoders: the test then pins the format the other side relies on.
- No test runs the library, the workers, IndexedDB, the clipboard, the share sheet or a
  real signature: they need a browser.

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
