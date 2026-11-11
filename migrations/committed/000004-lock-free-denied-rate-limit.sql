--! Previous: sha1:3c318dc994d729f7cfc692931fc8012065d20b00
--! Hash: sha1:8cc4bfd73699353d9fcf86272a2cca1089dc87b0
--! Message: lock-free denied rate limit

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

  SELECT r.tat INTO v_tat FROM rate_limits AS r WHERE r.key = p_key;

  IF NOT FOUND OR GREATEST(v_tat, v_now) + v_interval <= v_now + v_burst THEN
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
      RETURN NEXT;
      RETURN;
    END IF;

    SELECT r.tat INTO v_tat FROM rate_limits AS r WHERE r.key = p_key;
  END IF;

  allowed := false;
  remaining := 0;
  retry_after := GREATEST(
    1,
    ceil(extract(epoch FROM COALESCE(v_tat, v_now) + v_interval - v_burst - v_now))
  )::integer;

  RETURN NEXT;
END;
$$;

REVOKE EXECUTE ON FUNCTION rate_limit(text, integer, integer) FROM PUBLIC;
GRANT EXECUTE ON FUNCTION rate_limit(text, integer, integer) TO api;
