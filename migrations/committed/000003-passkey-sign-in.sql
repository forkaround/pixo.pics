--! Previous: sha1:02277a08bae0c00cdcda923318ddc87281e34dbe
--! Hash: sha1:3c318dc994d729f7cfc692931fc8012065d20b00
--! Message: passkey sign-in

DROP FUNCTION IF EXISTS create_authentication_challenge(integer) CASCADE;
CREATE OR REPLACE FUNCTION create_authentication_challenge(p_ttl_seconds integer)
RETURNS text
LANGUAGE plpgsql
SECURITY DEFINER
SET search_path = public, pg_temp
AS $$
DECLARE
  v_challenge text;
BEGIN
  IF p_ttl_seconds IS NULL OR p_ttl_seconds < 1 THEN
    RAISE EXCEPTION 'create_authentication_challenge: challenge must last at least 1 second, got %', p_ttl_seconds
      USING ERRCODE = 'invalid_parameter_value';
  END IF;

  INSERT INTO webauthn_challenges (challenge, ceremony, expires_at)
  VALUES (new_token(), 'authentication', now() + p_ttl_seconds * interval '1 second')
  RETURNING challenge INTO v_challenge;

  RETURN v_challenge;
END;
$$;

REVOKE EXECUTE ON FUNCTION create_authentication_challenge(integer) FROM PUBLIC;
GRANT EXECUTE ON FUNCTION create_authentication_challenge(integer) TO api;

DROP FUNCTION IF EXISTS passkey_public_key(text) CASCADE;
CREATE OR REPLACE FUNCTION passkey_public_key(p_credential_id text)
RETURNS text
LANGUAGE sql
STABLE
SECURITY DEFINER
SET search_path = public, pg_temp
AS $$
  SELECT p.public_key
  FROM passkeys AS p
  WHERE p.credential_id = p_credential_id;
$$;

REVOKE EXECUTE ON FUNCTION passkey_public_key(text) FROM PUBLIC;
GRANT EXECUTE ON FUNCTION passkey_public_key(text) TO api;

DROP FUNCTION IF EXISTS sign_in_with_passkey(text, text, bigint, boolean, integer) CASCADE;
CREATE OR REPLACE FUNCTION sign_in_with_passkey(
  p_challenge text,
  p_credential_id text,
  p_sign_count bigint,
  p_backed_up boolean,
  p_session_seconds integer
)
RETURNS TABLE (outcome text, session_token text, email text, username text)
LANGUAGE plpgsql
SECURITY DEFINER
SET search_path = public, pg_temp
AS $$
DECLARE
  v_user_id uuid;
BEGIN
  IF p_session_seconds IS NULL OR p_session_seconds < 1 THEN
    RAISE EXCEPTION 'sign_in_with_passkey: session must last at least 1 second, got %', p_session_seconds
      USING ERRCODE = 'invalid_parameter_value';
  END IF;

  DELETE FROM webauthn_challenges AS c
  WHERE c.challenge = p_challenge AND c.ceremony = 'authentication' AND c.expires_at > now();

  IF NOT FOUND THEN
    outcome := 'no-challenge';
    RETURN NEXT;
    RETURN;
  END IF;

  UPDATE passkeys AS p
  SET sign_count = p_sign_count,
      backed_up = p.backup_eligible AND p_backed_up,
      last_used_at = now()
  WHERE p.credential_id = p_credential_id
    AND (p_sign_count > p.sign_count OR (p_sign_count = 0 AND p.sign_count = 0))
  RETURNING p.user_id INTO v_user_id;

  IF NOT FOUND THEN
    outcome := 'no-passkey';
    RETURN NEXT;
    RETURN;
  END IF;

  SELECT u.email, u.username INTO email, username
  FROM users AS u
  WHERE u.id = v_user_id;

  outcome := 'signed-in';
  session_token := new_token();

  INSERT INTO sessions (token_hash, user_id, expires_at)
  VALUES (
    sha256(convert_to(session_token, 'UTF8')),
    v_user_id,
    now() + p_session_seconds * interval '1 second'
  );

  RETURN NEXT;
END;
$$;

REVOKE EXECUTE ON FUNCTION sign_in_with_passkey(text, text, bigint, boolean, integer) FROM PUBLIC;
GRANT EXECUTE ON FUNCTION sign_in_with_passkey(text, text, bigint, boolean, integer) TO api;
