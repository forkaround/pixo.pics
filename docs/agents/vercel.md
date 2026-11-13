# Vercel and Neon

Read when touching Vercel, previews, Neon branches, or production roles and secrets.

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
  `GRANT ... TO api`: reset it as in [migrations/AGENTS.md](../../migrations/AGENTS.md#committing-and-previews).
- The function's environment still holds the owner's `DATABASE_URL`: RLS contains the
  server's queries, not a compromised process.
- Keep `regions` in `vercel.json` next to Neon's region: `fra1` for `aws-eu-central-1`.
- Every `/api/*` rewrites to `api/index.ts`, the only function; every other address that
  is not a file rewrites to `index.html`, so an unknown address gets the app's own
  not-found page with a 200, not a 404.
