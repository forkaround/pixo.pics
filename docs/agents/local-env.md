# Local environment

Read when touching `.env`, running the app or the database locally, or the proxy.

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
