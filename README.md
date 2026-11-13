# pixo

Make pictures from a prompt, in your browser. The image models run on your graphics card
through WebGPU: prompts and pictures stay on your device, and the server only knows who
you are.

| Page     | Does                                                                          |
| -------- | ----------------------------------------------------------------------------- |
| Explore  | What people are making                                                        |
| Studio   | Writes a picture from a prompt; a small language model can rewrite the prompt |
| Library  | The pictures you kept, in this browser's IndexedDB, with search and sharing   |
| Settings | About (release, region, latency), the models you downloaded, your account     |

No account is needed to make pictures. An account signs in with a passkey or a link by
email; the server keeps the account, its passkeys and sessions, nothing more.

## How it's built

All of it is Elm, run by [Bun](https://bun.com) and deployed to Vercel on Neon.

| Part     | Where         | Built with                                                              |
| -------- | ------------- | ----------------------------------------------------------------------- |
| Browser  | `client/`     | Elm, Vite, Tailwind, [elmcn](https://github.com/ricardocasares/elmcn)   |
| Models   | `client/`     | [bonsai-image-pipeline-lib][bonsai] and [WebLLM][webllm] in web workers |
| Server   | `api/`        | Elm `Platform.worker` in one Vercel Function (Bun runtime)              |
| Shared   | `shared/`     | Routes, pages and the JSON codecs both sides compile against            |
| Database | `api/src/`    | `Services/Neon.elm`, Postgres over HTTP, with row-level security        |
| Schema   | `migrations/` | [Graphile Migrate](https://github.com/graphile/migrate)                 |

[bonsai]: https://github.com/nishlumi/bonsai-image-pipeline-lib
[webllm]: https://github.com/mlc-ai/web-llm

The studio needs a browser with WebGPU: a recent Chrome, Edge or Safari. The image
models are 2.9 to 3.4 GB and the prompt writer under 1 GB, each downloaded once.

## Run it locally

Needs Bun, [Docker](https://www.docker.com) and Postgres on `localhost:5432`, with
`psql` and `pg_dump` on `PATH`.

```sh
git clone https://github.com/forkaround/pixo.pics.git
cd pixo.pics
bun install
cp .env.example .env
psql postgres://postgres@localhost:5432/postgres \
  -c "SET scram_iterations = 1" \
  -c "CREATE ROLE app LOGIN PASSWORD 'app' BYPASSRLS" \
  -c "CREATE ROLE api LOGIN PASSWORD 'api'" \
  -c "ALTER ROLE api SET search_path = public, pg_temp" \
  -c "CREATE DATABASE app OWNER app"
bun db migrate
bun dev
```

Open http://localhost:5173. Locally no email is sent: the sign-in link is printed in the
server's log.

## Scripts

| Script              | Does                                                                      |
| ------------------- | ------------------------------------------------------------------------- |
| `bun dev`           | Runs `types`, `dev:proxy` and `dev:vite` together                         |
| `bun run dev:proxy` | Starts the local Neon proxy in Docker, checks `DATABASE_URL` through it   |
| `bun run dev:vite`  | Vite on :5173, with the Elm server on `/api`                              |
| `bun run build`     | Types, type-check, then builds into `dist/client` and `dist/server`       |
| `bun run preview`   | Serves the build on :4173                                                 |
| `bun run types`     | Regenerates the TypeScript types of both Elm programs                     |
| `bun check`         | Type-checks the browser app and the server                                |
| `bun run test`      | Runs the Elm tests of the browser app, then the server's                  |
| `bun fmt`           | Formats everything, Elm with elm-format                                   |
| `bun fmt-check`     | Reports what `bun fmt` would change                                       |
| `bun db <command>`  | Graphile Migrate: `migrate`, `watch`, `commit`, `status`, `reset --erase` |

## Environment

`.env.example` lists every variable, with values that work on a fresh checkout.

| Variable                | For                                                            |
| ----------------------- | -------------------------------------------------------------- |
| `DATABASE_URL`          | The app's database, as its owner: migrations                   |
| `API_DATABASE_PASSWORD` | The password of `api`, the role the server connects as         |
| `NEON_ENDPOINT`         | The local proxy, `http://localhost:4445/sql`; not on Vercel    |
| `SHADOW_DATABASE_URL`   | A scratch database for `bun db watch` and `commit`             |
| `ROOT_DATABASE_URL`     | A superuser, for `bun db reset` and the proxy                  |
| `BREVO_API_KEY`         | Sends the sign-in emails; empty prints them to the log instead |
| `EMAIL_FROM`            | The address those emails come from                             |

## Row-level security

Every table has row-level security. Migrations run as the owner; the server connects as
`api`, which owns nothing and sees only the signed-in user's rows: `authenticate(token)`
sets the user for one transaction, and `Db.signedIn` starts that transaction. On Neon,
create `api` in SQL and set `API_DATABASE_PASSWORD` on Vercel: see
[migrations/AGENTS.md](migrations/AGENTS.md#row-level-security).

## Health check

`GET /api/health` queries the database as `api` and answers in the
[health check format](https://datatracker.ietf.org/doc/html/draft-inadarei-api-health-check):
`pass` (200), `warn` when slower than 1 s (200), or `fail` (503). An address gets 60
checks a minute, then 429 with `retry-after`. Settings, About shows the same report each
time it opens.

```json
{
  "status": "pass",
  "releaseId": "7990ba2…",
  "region": "fra1",
  "links": { "release": "https://github.com/…/commit/7990ba2…" },
  "checks": { "database": { "status": "pass", "latencyMs": 25 } }
}
```

## Schema changes

```sh
bun db watch                              # applies migrations/current.sql on every save
bun db commit --message "Create users"    # turns it into a committed migration
```

## Deploying

Vercel builds `main` for production and every other branch as a preview, each migrating
its own Neon branch first. The details are in
[docs/agents/vercel.md](docs/agents/vercel.md).

## More

[AGENTS.md](AGENTS.md) and the guides it links to have the conventions and the reasons
behind them. License: [MIT](LICENSE).
