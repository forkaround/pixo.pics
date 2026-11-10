--! Previous: -
--! Hash: sha1:4af6e784fa1a1927c9d781a3cbaa5b75c5a4e131
--! Message: auth, rate limits and row-level security

REVOKE CREATE ON SCHEMA public FROM PUBLIC;
REVOKE TEMP ON DATABASE ":DATABASE_NAME" FROM PUBLIC;
GRANT USAGE ON SCHEMA public TO api;

DROP TABLE IF EXISTS users CASCADE;
CREATE TABLE users (
  id uuid PRIMARY KEY DEFAULT gen_random_uuid(),
  email text NOT NULL UNIQUE CHECK (email = lower(btrim(email)) AND email <> ''),
  username text NOT NULL UNIQUE CHECK (username = lower(btrim(username)) AND username <> ''),
  created_at timestamptz NOT NULL DEFAULT now()
);

ALTER TABLE users ENABLE ROW LEVEL SECURITY, FORCE ROW LEVEL SECURITY;
CREATE POLICY own_rows ON users
  USING (id = (SELECT NULLIF(current_setting('app.user_id', true), '')::uuid))
  WITH CHECK (id = (SELECT NULLIF(current_setting('app.user_id', true), '')::uuid));
GRANT SELECT ON users TO api;

DROP TABLE IF EXISTS magic_links CASCADE;
CREATE TABLE magic_links (
  token_hash bytea PRIMARY KEY CHECK (octet_length(token_hash) = 32),
  email text NOT NULL CHECK (email = lower(btrim(email)) AND email <> ''),
  username text CHECK (username = lower(btrim(username)) AND username <> ''),
  created_at timestamptz NOT NULL DEFAULT now(),
  expires_at timestamptz NOT NULL
);

DROP INDEX IF EXISTS magic_links_email_idx;
CREATE INDEX magic_links_email_idx ON magic_links (email);

DROP INDEX IF EXISTS magic_links_expires_at_idx;
CREATE INDEX magic_links_expires_at_idx ON magic_links (expires_at);

ALTER TABLE magic_links ENABLE ROW LEVEL SECURITY, FORCE ROW LEVEL SECURITY;

DROP TABLE IF EXISTS passkeys CASCADE;
CREATE TABLE passkeys (
  credential_id text PRIMARY KEY CHECK (credential_id ~ '^[A-Za-z0-9_-]+$'),
  user_id uuid NOT NULL REFERENCES users ON DELETE CASCADE,
  public_key text NOT NULL CHECK (public_key ~ '^[A-Za-z0-9_-]+$'),
  sign_count bigint NOT NULL DEFAULT 0 CHECK (sign_count BETWEEN 0 AND 4294967295),
  transports text[] NOT NULL DEFAULT '{}',
  backup_eligible boolean NOT NULL,
  backed_up boolean NOT NULL,
  name text,
  created_at timestamptz NOT NULL DEFAULT now(),
  last_used_at timestamptz,
  CHECK (backup_eligible OR NOT backed_up)
);

DROP INDEX IF EXISTS passkeys_user_id_idx;
CREATE INDEX passkeys_user_id_idx ON passkeys (user_id);

ALTER TABLE passkeys ENABLE ROW LEVEL SECURITY, FORCE ROW LEVEL SECURITY;
CREATE POLICY own_rows ON passkeys
  USING (user_id = (SELECT NULLIF(current_setting('app.user_id', true), '')::uuid))
  WITH CHECK (user_id = (SELECT NULLIF(current_setting('app.user_id', true), '')::uuid));
GRANT SELECT, INSERT, UPDATE (name), DELETE ON passkeys TO api;

DROP TABLE IF EXISTS webauthn_challenges CASCADE;
CREATE TABLE webauthn_challenges (
  challenge text PRIMARY KEY CHECK (challenge ~ '^[A-Za-z0-9_-]+$'),
  ceremony text NOT NULL CHECK (ceremony IN ('registration', 'authentication')),
  user_id uuid REFERENCES users ON DELETE CASCADE,
  created_at timestamptz NOT NULL DEFAULT now(),
  expires_at timestamptz NOT NULL,
  CHECK (ceremony <> 'registration' OR user_id IS NOT NULL)
);

DROP INDEX IF EXISTS webauthn_challenges_user_id_idx;
CREATE INDEX webauthn_challenges_user_id_idx ON webauthn_challenges (user_id);

DROP INDEX IF EXISTS webauthn_challenges_expires_at_idx;
CREATE INDEX webauthn_challenges_expires_at_idx ON webauthn_challenges (expires_at);

ALTER TABLE webauthn_challenges ENABLE ROW LEVEL SECURITY, FORCE ROW LEVEL SECURITY;
CREATE POLICY own_rows ON webauthn_challenges
  USING (user_id = (SELECT NULLIF(current_setting('app.user_id', true), '')::uuid))
  WITH CHECK (user_id = (SELECT NULLIF(current_setting('app.user_id', true), '')::uuid));
GRANT SELECT, INSERT, DELETE ON webauthn_challenges TO api;

DROP TABLE IF EXISTS sessions CASCADE;
CREATE TABLE sessions (
  id uuid PRIMARY KEY DEFAULT gen_random_uuid(),
  token_hash bytea NOT NULL UNIQUE CHECK (octet_length(token_hash) = 32),
  user_id uuid NOT NULL REFERENCES users ON DELETE CASCADE,
  created_at timestamptz NOT NULL DEFAULT now(),
  expires_at timestamptz NOT NULL
);

DROP INDEX IF EXISTS sessions_user_id_idx;
CREATE INDEX sessions_user_id_idx ON sessions (user_id);

DROP INDEX IF EXISTS sessions_expires_at_idx;
CREATE INDEX sessions_expires_at_idx ON sessions (expires_at);

ALTER TABLE sessions ENABLE ROW LEVEL SECURITY, FORCE ROW LEVEL SECURITY;
CREATE POLICY own_rows ON sessions
  USING (user_id = (SELECT NULLIF(current_setting('app.user_id', true), '')::uuid))
  WITH CHECK (user_id = (SELECT NULLIF(current_setting('app.user_id', true), '')::uuid));
GRANT SELECT (id, user_id, created_at, expires_at), DELETE ON sessions TO api;

DROP TABLE IF EXISTS rate_limits CASCADE;
CREATE TABLE rate_limits (
  key text PRIMARY KEY,
  tat timestamptz NOT NULL
);

ALTER TABLE rate_limits ENABLE ROW LEVEL SECURITY, FORCE ROW LEVEL SECURITY;

DROP FUNCTION IF EXISTS authenticate(text) CASCADE;
CREATE OR REPLACE FUNCTION authenticate(p_token text)
RETURNS uuid
LANGUAGE plpgsql
SECURITY DEFINER
SET search_path = public, pg_temp
AS $$
DECLARE
  v_user_id uuid;
BEGIN
  SELECT s.user_id INTO v_user_id
  FROM sessions AS s
  WHERE s.token_hash = sha256(convert_to(p_token, 'UTF8')) AND s.expires_at > now();

  PERFORM set_config('app.user_id', COALESCE(v_user_id::text, ''), true);

  RETURN v_user_id;
END;
$$;

REVOKE EXECUTE ON FUNCTION authenticate(text) FROM PUBLIC;
GRANT EXECUTE ON FUNCTION authenticate(text) TO api;

DROP FUNCTION IF EXISTS rate_limit(text, integer, integer) CASCADE;
CREATE OR REPLACE FUNCTION rate_limit(p_key text, p_limit integer, p_window_seconds integer)
RETURNS TABLE (allowed boolean, remaining integer, retry_after integer)
LANGUAGE plpgsql
SECURITY DEFINER
SET search_path = public, pg_temp
AS $$
DECLARE
  v_now timestamptz := clock_timestamp();
  v_interval interval;
  v_burst interval;
  v_tat timestamptz;
BEGIN
  IF p_key IS NULL THEN
    RAISE EXCEPTION 'rate_limit: key must not be null'
      USING ERRCODE = 'invalid_parameter_value';
  END IF;
  IF octet_length(p_key) > 512 THEN
    RAISE EXCEPTION 'rate_limit: key must be at most 512 bytes, got %', octet_length(p_key)
      USING ERRCODE = 'invalid_parameter_value';
  END IF;
  IF p_limit IS NULL OR p_limit < 1 THEN
    RAISE EXCEPTION 'rate_limit: limit must be at least 1, got %', p_limit
      USING ERRCODE = 'invalid_parameter_value';
  END IF;
  IF p_window_seconds IS NULL OR p_window_seconds < 1 THEN
    RAISE EXCEPTION 'rate_limit: window must be at least 1 second, got %', p_window_seconds
      USING ERRCODE = 'invalid_parameter_value';
  END IF;
  IF p_limit > p_window_seconds::bigint * 1000000 THEN
    RAISE EXCEPTION 'rate_limit: limit % exceeds one per microsecond over % s', p_limit, p_window_seconds
      USING ERRCODE = 'invalid_parameter_value';
  END IF;

  v_interval := (p_window_seconds::bigint * 1000000 / p_limit) * interval '1 microsecond';
  v_burst := v_interval * p_limit;

  INSERT INTO rate_limits AS r (key, tat)
  VALUES (p_key, v_now + v_interval)
  ON CONFLICT (key) DO UPDATE
    SET tat = GREATEST(r.tat, v_now) + v_interval
    WHERE GREATEST(r.tat, v_now) + v_interval <= v_now + v_burst
  RETURNING r.tat INTO v_tat;

  IF FOUND THEN
    allowed := true;
    remaining := floor(
      extract(epoch FROM v_now + v_burst - v_tat) / extract(epoch FROM v_interval)
    )::integer;
    retry_after := 0;
  ELSE
    SELECT r.tat INTO v_tat FROM rate_limits AS r WHERE r.key = p_key;
    allowed := false;
    remaining := 0;
    retry_after := GREATEST(
      1,
      ceil(extract(epoch FROM COALESCE(v_tat, v_now) + v_interval - v_burst - v_now))
    )::integer;
  END IF;

  RETURN NEXT;
END;
$$;

REVOKE EXECUTE ON FUNCTION rate_limit(text, integer, integer) FROM PUBLIC;
GRANT EXECUTE ON FUNCTION rate_limit(text, integer, integer) TO api;

DROP FUNCTION IF EXISTS delete_expired() CASCADE;
CREATE OR REPLACE FUNCTION delete_expired()
RETURNS void
LANGUAGE sql
SECURITY DEFINER
SET search_path = public, pg_temp
AS $$
  DELETE FROM magic_links
  WHERE token_hash IN (
    SELECT m.token_hash
    FROM magic_links AS m
    WHERE m.expires_at <= now()
    FOR UPDATE SKIP LOCKED
  );
  DELETE FROM webauthn_challenges
  WHERE challenge IN (
    SELECT c.challenge
    FROM webauthn_challenges AS c
    WHERE c.expires_at <= now()
    FOR UPDATE SKIP LOCKED
  );
  DELETE FROM sessions
  WHERE id IN (
    SELECT s.id
    FROM sessions AS s
    WHERE s.expires_at <= now()
    FOR UPDATE SKIP LOCKED
  );
  DELETE FROM rate_limits
  WHERE key IN (
    SELECT r.key
    FROM rate_limits AS r
    WHERE r.tat <= now()
    FOR UPDATE SKIP LOCKED
  );
$$;

REVOKE EXECUTE ON FUNCTION delete_expired() FROM PUBLIC;
GRANT EXECUTE ON FUNCTION delete_expired() TO api;
