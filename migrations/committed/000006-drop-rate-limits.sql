--! Previous: sha1:85ffb36160d4d47cce149764df7dcd8d7295ff97
--! Hash: sha1:3adff9e1c24a71707d867bf8fbd2ab828851a1e8
--! Message: drop rate limits

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
$$;

REVOKE EXECUTE ON FUNCTION delete_expired() FROM PUBLIC;
GRANT EXECUTE ON FUNCTION delete_expired() TO api;

DROP FUNCTION IF EXISTS rate_limit(text, integer, integer) CASCADE;

DROP TABLE IF EXISTS rate_limits CASCADE;
