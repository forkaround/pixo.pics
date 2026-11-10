--! Previous: sha1:4af6e784fa1a1927c9d781a3cbaa5b75c5a4e131
--! Hash: sha1:02277a08bae0c00cdcda923318ddc87281e34dbe
--! Message: magic link sign-up, sign-in and sessions

DROP FUNCTION IF EXISTS new_token() CASCADE;
CREATE OR REPLACE FUNCTION new_token()
RETURNS text
LANGUAGE sql
VOLATILE
SET search_path = public, pg_temp
AS $$
  SELECT translate(
    rtrim(encode(uuid_send(gen_random_uuid()) || uuid_send(gen_random_uuid()), 'base64'), '='),
    '+/',
    '-_'
  );
$$;

REVOKE EXECUTE ON FUNCTION new_token() FROM PUBLIC;

DROP FUNCTION IF EXISTS create_sign_up_link(text, text, integer) CASCADE;
CREATE OR REPLACE FUNCTION create_sign_up_link(p_email text, p_username text, p_ttl_seconds integer)
RETURNS TABLE (outcome text, token text)
LANGUAGE plpgsql
SECURITY DEFINER
SET search_path = public, pg_temp
AS $$
DECLARE
  v_email text := lower(btrim(p_email));
  v_username text := lower(btrim(p_username));
BEGIN
  IF v_email IS NULL OR v_email = '' THEN
    RAISE EXCEPTION 'create_sign_up_link: email must not be empty'
      USING ERRCODE = 'invalid_parameter_value';
  END IF;
  IF v_username IS NULL OR v_username = '' THEN
    RAISE EXCEPTION 'create_sign_up_link: username must not be empty'
      USING ERRCODE = 'invalid_parameter_value';
  END IF;
  IF p_ttl_seconds IS NULL OR p_ttl_seconds < 1 THEN
    RAISE EXCEPTION 'create_sign_up_link: ttl must be at least 1 second, got %', p_ttl_seconds
      USING ERRCODE = 'invalid_parameter_value';
  END IF;

  IF EXISTS (SELECT 1 FROM users AS u WHERE u.username = v_username) THEN
    outcome := 'username-taken';
    token := NULL;
    RETURN NEXT;
    RETURN;
  END IF;

  IF EXISTS (SELECT 1 FROM users AS u WHERE u.email = v_email) THEN
    outcome := 'sign-in';
    v_username := NULL;
  ELSE
    outcome := 'sign-up';
  END IF;

  token := new_token();

  INSERT INTO magic_links (token_hash, email, username, expires_at)
  VALUES (
    sha256(convert_to(token, 'UTF8')),
    v_email,
    v_username,
    now() + p_ttl_seconds * interval '1 second'
  );

  RETURN NEXT;
END;
$$;

REVOKE EXECUTE ON FUNCTION create_sign_up_link(text, text, integer) FROM PUBLIC;
GRANT EXECUTE ON FUNCTION create_sign_up_link(text, text, integer) TO api;

DROP FUNCTION IF EXISTS magic_link(text) CASCADE;
CREATE OR REPLACE FUNCTION magic_link(p_token text)
RETURNS TABLE (email text, username text, new_account boolean)
LANGUAGE sql
STABLE
SECURITY DEFINER
SET search_path = public, pg_temp
AS $$
  SELECT m.email, COALESCE(u.username, m.username), u.id IS NULL
  FROM magic_links AS m
  LEFT JOIN users AS u ON u.email = m.email
  WHERE m.token_hash = sha256(convert_to(p_token, 'UTF8'))
    AND m.expires_at > now()
    AND (u.id IS NOT NULL OR m.username IS NOT NULL);
$$;

REVOKE EXECUTE ON FUNCTION magic_link(text) FROM PUBLIC;
GRANT EXECUTE ON FUNCTION magic_link(text) TO api;

DROP FUNCTION IF EXISTS consume_magic_link(text, integer) CASCADE;
CREATE OR REPLACE FUNCTION consume_magic_link(p_token text, p_session_seconds integer)
RETURNS TABLE (outcome text, session_token text, email text, username text)
LANGUAGE plpgsql
SECURITY DEFINER
SET search_path = public, pg_temp
AS $$
DECLARE
  v_email text;
  v_username text;
  v_user_id uuid;
  v_account text;
BEGIN
  IF p_session_seconds IS NULL OR p_session_seconds < 1 THEN
    RAISE EXCEPTION 'consume_magic_link: session must last at least 1 second, got %', p_session_seconds
      USING ERRCODE = 'invalid_parameter_value';
  END IF;

  DELETE FROM magic_links AS m
  WHERE m.token_hash = sha256(convert_to(p_token, 'UTF8')) AND m.expires_at > now()
  RETURNING m.email, m.username INTO v_email, v_username;

  IF NOT FOUND THEN
    outcome := 'invalid';
    RETURN NEXT;
    RETURN;
  END IF;

  IF v_username IS NOT NULL THEN
    INSERT INTO users AS u (email, username)
    VALUES (v_email, v_username)
    ON CONFLICT DO NOTHING;
  END IF;

  SELECT u.id, u.username INTO v_user_id, v_account
  FROM users AS u
  WHERE u.email = v_email;

  IF NOT FOUND THEN
    IF v_username IS NULL THEN
      outcome := 'invalid';
    ELSE
      outcome := 'username-taken';
    END IF;
    RETURN NEXT;
    RETURN;
  END IF;

  outcome := 'verified';
  email := v_email;
  username := v_account;
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

REVOKE EXECUTE ON FUNCTION consume_magic_link(text, integer) FROM PUBLIC;
GRANT EXECUTE ON FUNCTION consume_magic_link(text, integer) TO api;

DROP FUNCTION IF EXISTS create_sign_in_link(text, integer) CASCADE;
CREATE OR REPLACE FUNCTION create_sign_in_link(p_email text, p_ttl_seconds integer)
RETURNS TABLE (outcome text, token text)
LANGUAGE plpgsql
SECURITY DEFINER
SET search_path = public, pg_temp
AS $$
DECLARE
  v_email text := lower(btrim(p_email));
BEGIN
  IF p_ttl_seconds IS NULL OR p_ttl_seconds < 1 THEN
    RAISE EXCEPTION 'create_sign_in_link: ttl must be at least 1 second, got %', p_ttl_seconds
      USING ERRCODE = 'invalid_parameter_value';
  END IF;

  IF NOT EXISTS (SELECT 1 FROM users AS u WHERE u.email = v_email) THEN
    outcome := 'no-account';
    token := NULL;
    RETURN NEXT;
    RETURN;
  END IF;

  outcome := 'sign-in';
  token := new_token();

  INSERT INTO magic_links (token_hash, email, expires_at)
  VALUES (
    sha256(convert_to(token, 'UTF8')),
    v_email,
    now() + p_ttl_seconds * interval '1 second'
  );

  RETURN NEXT;
END;
$$;

REVOKE EXECUTE ON FUNCTION create_sign_in_link(text, integer) FROM PUBLIC;
GRANT EXECUTE ON FUNCTION create_sign_in_link(text, integer) TO api;

DROP FUNCTION IF EXISTS sign_out(text) CASCADE;
CREATE OR REPLACE FUNCTION sign_out(p_token text)
RETURNS void
LANGUAGE sql
SECURITY DEFINER
SET search_path = public, pg_temp
AS $$
  DELETE FROM sessions AS s
  WHERE s.token_hash = sha256(convert_to(p_token, 'UTF8'));
$$;

REVOKE EXECUTE ON FUNCTION sign_out(text) FROM PUBLIC;
GRANT EXECUTE ON FUNCTION sign_out(text) TO api;
