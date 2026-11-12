CREATE OR REPLACE FUNCTION delete_account(p_token text) RETURNS boolean
LANGUAGE sql SECURITY DEFINER
SET search_path = public, pg_temp
AS $$
  WITH deleted AS (
    DELETE FROM users
    WHERE id = (
      SELECT user_id FROM sessions
      WHERE token_hash = sha256(convert_to(p_token, 'UTF8'))
        AND expires_at > now()
    )
    RETURNING email
  ), links AS (
    DELETE FROM magic_links
    WHERE email IN (SELECT email FROM deleted)
  )
  SELECT EXISTS (SELECT 1 FROM deleted);
$$;

REVOKE EXECUTE ON FUNCTION delete_account(text) FROM PUBLIC;
GRANT EXECUTE ON FUNCTION delete_account(text) TO api;
