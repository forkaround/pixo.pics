# Migrations and database

Migrations, the schema's conventions and row-level security. The SQL the server sends is in
`api/AGENTS.md`.

## Workflow

1. With `watch` stopped, write `migrations/current.sql` to rerun cleanly: undo, then redo
   (below). Never edit `migrations/committed/`: they are hash-signed.
2. Run it with `bun db watch --once`; `migrations/check-rls.sql` runs after it.
3. Ask the user for a review. Agents never commit a migration: no `bun db commit`. The
   user commits, and `commit` dumps the shadow schema to `migrations/schema.sql`.

## Rerunnable migrations

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

## Committing and previews

- Previews run only committed migrations, so the user commits before pushing a branch
  that needs its schema. Each migration names the previous one's hash: if `main` gains a
  migration first, `bun db uncommit`, rebase, `bun db watch --once`, `bun db commit`. Git
  may merge two migrations without a conflict (different `-m` names) and every `migrate`
  then fails. The preview's Neon branch ran the old hash; reset it:
  `bunx neon@latest branches reset preview/<git> --parent`.

## Schema conventions

- The Neon driver can't read, then decide, inside a transaction: every check-then-change
  is one statement. A link or a challenge is used once by
  `DELETE ... WHERE ... AND expires_at > now() RETURNING`; a passkey's counter moves by
  `UPDATE ... WHERE $2 > sign_count OR ($2 = 0 AND sign_count = 0) RETURNING`.
- Tokens are never stored: `token_hash` is `sha256(convert_to($1, 'UTF8'))`, computed by
  Postgres from the token passed as a parameter, and a new token is returned once.
- A `users` row means a proven email: consuming a magic link creates the user, never the
  sign-up request.
- Postgres can't check a signature: `sign_in_with_passkey` trusts that the server did.
  It is the one function that takes no secret.
- `delete_expired()` exists, and nothing schedules it yet.
- `rate_limits` is a logged table on purpose: Neon drops unlogged tables when the compute
  suspends, which would reset every window.

## Row-level security

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
- `migrations/check-rls.sql` runs after `migrate` and after `current.sql` and fails them
  over what it can see. It can't tell a grant that is too wide: read the grants in
  `migrations/schema.sql`'s diff.
- Try policies as `api`, never as the owner, who skips them:
  `psql postgres://api:api@localhost:5432/app`, then
  `BEGIN; SELECT authenticate('<token>'); ...`.

### A new table

Right after its `CREATE TABLE` (`DROP TABLE ... CASCADE` drops them too):

1. `ALTER TABLE t ENABLE ROW LEVEL SECURITY, FORCE ROW LEVEL SECURITY`.
2. `CREATE POLICY` (`own_rows`, as above).
3. The narrowest `GRANT`s. Never grant `TRUNCATE`, `REFERENCES` or `TRIGGER`.
4. An index on its user column.

A view needs `WITH (security_invoker = true)`.
