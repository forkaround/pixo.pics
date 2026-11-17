--! Previous: sha1:87f744664fc3c13105efd66668dda8c1c8bc42e9
--! Hash: sha1:9e4016020a3e1016e46f6a851621c74f502c6016
--! Message: Keep cloud sync connections

CREATE EXTENSION IF NOT EXISTS pgcrypto;

DROP TABLE IF EXISTS sync_attempts CASCADE;
CREATE TABLE sync_attempts (
  state text PRIMARY KEY CHECK (state ~ '^[A-Za-z0-9_-]{43}$'),
  user_id uuid NOT NULL REFERENCES users ON DELETE CASCADE,
  provider text NOT NULL CHECK (provider IN ('dropbox', 'google-drive')),
  verifier text NOT NULL CHECK (verifier ~ '^[A-Za-z0-9_-]{43}$'),
  expires_at timestamptz NOT NULL
);

ALTER TABLE sync_attempts ENABLE ROW LEVEL SECURITY, FORCE ROW LEVEL SECURITY;
CREATE POLICY own_rows ON sync_attempts
  USING (user_id = (SELECT NULLIF(current_setting('app.user_id', true), '')::uuid))
  WITH CHECK (user_id = (SELECT NULLIF(current_setting('app.user_id', true), '')::uuid));
GRANT SELECT, INSERT, DELETE ON sync_attempts TO api;

DROP INDEX IF EXISTS sync_attempts_user_id_idx;
CREATE INDEX sync_attempts_user_id_idx ON sync_attempts (user_id);

DROP TABLE IF EXISTS sync_connections CASCADE;
CREATE TABLE sync_connections (
  user_id uuid NOT NULL REFERENCES users ON DELETE CASCADE,
  provider text NOT NULL CHECK (provider IN ('dropbox', 'google-drive')),
  refresh_token bytea NOT NULL,
  connected_at timestamptz NOT NULL DEFAULT now(),
  PRIMARY KEY (user_id, provider)
);

ALTER TABLE sync_connections ENABLE ROW LEVEL SECURITY, FORCE ROW LEVEL SECURITY;
CREATE POLICY own_rows ON sync_connections
  USING (user_id = (SELECT NULLIF(current_setting('app.user_id', true), '')::uuid))
  WITH CHECK (user_id = (SELECT NULLIF(current_setting('app.user_id', true), '')::uuid));
GRANT SELECT, INSERT, DELETE ON sync_connections TO api;
GRANT UPDATE (refresh_token, connected_at) ON sync_connections TO api;
