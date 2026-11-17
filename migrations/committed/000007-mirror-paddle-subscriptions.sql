--! Previous: sha1:3adff9e1c24a71707d867bf8fbd2ab828851a1e8
--! Hash: sha1:268dc2158df0b451c507e59684c5b0db5b7a7abc
--! Message: Mirror Paddle subscriptions

DROP TABLE IF EXISTS subscriptions CASCADE;
CREATE TABLE subscriptions (
  paddle_subscription_id text PRIMARY KEY CHECK (paddle_subscription_id ~ '^sub_[a-z0-9]+$'),
  user_id uuid NOT NULL REFERENCES users ON DELETE CASCADE,
  paddle_customer_id text NOT NULL CHECK (paddle_customer_id ~ '^ctm_[a-z0-9]+$'),
  status text NOT NULL CHECK (status IN ('active', 'canceled', 'past_due', 'paused', 'trialing')),
  price_id text NOT NULL CHECK (price_id ~ '^pri_[a-z0-9]+$'),
  current_period_ends_at timestamptz,
  scheduled_change text CHECK (scheduled_change IN ('cancel', 'pause', 'resume')),
  scheduled_change_at timestamptz,
  paddle_updated_at timestamptz NOT NULL,
  created_at timestamptz NOT NULL DEFAULT now(),
  updated_at timestamptz NOT NULL DEFAULT now(),
  CHECK ((scheduled_change IS NULL) = (scheduled_change_at IS NULL))
);

DROP INDEX IF EXISTS subscriptions_user_id_idx;
CREATE INDEX subscriptions_user_id_idx ON subscriptions (user_id);

ALTER TABLE subscriptions ENABLE ROW LEVEL SECURITY, FORCE ROW LEVEL SECURITY;
CREATE POLICY own_rows ON subscriptions
  USING (user_id = (SELECT NULLIF(current_setting('app.user_id', true), '')::uuid))
  WITH CHECK (user_id = (SELECT NULLIF(current_setting('app.user_id', true), '')::uuid));
GRANT SELECT ON subscriptions TO api;

DROP FUNCTION IF EXISTS is_pro(uuid) CASCADE;
CREATE OR REPLACE FUNCTION is_pro(p_user_id uuid)
RETURNS boolean
LANGUAGE sql
STABLE
SECURITY DEFINER
SET search_path = public, pg_temp
AS $$
  SELECT EXISTS (
    SELECT 1
    FROM subscriptions AS s
    WHERE s.user_id = p_user_id
      AND s.status IN ('active', 'trialing', 'past_due')
  );
$$;

REVOKE EXECUTE ON FUNCTION is_pro(uuid) FROM PUBLIC;
GRANT EXECUTE ON FUNCTION is_pro(uuid) TO api;

DROP FUNCTION IF EXISTS apply_paddle_subscription(text, uuid, text, text, text, timestamptz, text, timestamptz, timestamptz) CASCADE;
CREATE OR REPLACE FUNCTION apply_paddle_subscription(
  p_subscription_id text,
  p_user_id uuid,
  p_customer_id text,
  p_status text,
  p_price_id text,
  p_period_ends_at timestamptz,
  p_scheduled_change text,
  p_scheduled_change_at timestamptz,
  p_paddle_updated_at timestamptz
)
RETURNS text
LANGUAGE plpgsql
SECURITY DEFINER
SET search_path = public, pg_temp
AS $$
BEGIN
  UPDATE subscriptions AS s
  SET paddle_customer_id = p_customer_id,
      status = p_status,
      price_id = p_price_id,
      current_period_ends_at = p_period_ends_at,
      scheduled_change = p_scheduled_change,
      scheduled_change_at = p_scheduled_change_at,
      paddle_updated_at = p_paddle_updated_at,
      updated_at = now()
  WHERE s.paddle_subscription_id = p_subscription_id
    AND s.paddle_updated_at <= p_paddle_updated_at;

  IF FOUND THEN
    RETURN 'applied';
  END IF;

  IF EXISTS (SELECT 1 FROM subscriptions AS s WHERE s.paddle_subscription_id = p_subscription_id) THEN
    RETURN 'stale';
  END IF;

  INSERT INTO subscriptions (
    paddle_subscription_id, user_id, paddle_customer_id, status, price_id,
    current_period_ends_at, scheduled_change, scheduled_change_at, paddle_updated_at
  )
  SELECT p_subscription_id, u.id, p_customer_id, p_status, p_price_id,
    p_period_ends_at, p_scheduled_change, p_scheduled_change_at, p_paddle_updated_at
  FROM users AS u
  WHERE u.id = p_user_id
  ON CONFLICT (paddle_subscription_id) DO NOTHING;

  IF FOUND THEN
    RETURN 'applied';
  END IF;

  UPDATE subscriptions AS s
  SET paddle_customer_id = p_customer_id,
      status = p_status,
      price_id = p_price_id,
      current_period_ends_at = p_period_ends_at,
      scheduled_change = p_scheduled_change,
      scheduled_change_at = p_scheduled_change_at,
      paddle_updated_at = p_paddle_updated_at,
      updated_at = now()
  WHERE s.paddle_subscription_id = p_subscription_id
    AND s.paddle_updated_at <= p_paddle_updated_at;

  IF FOUND THEN
    RETURN 'applied';
  ELSIF EXISTS (SELECT 1 FROM subscriptions AS s WHERE s.paddle_subscription_id = p_subscription_id) THEN
    RETURN 'stale';
  END IF;

  RETURN 'unmatched';
END;
$$;

REVOKE EXECUTE ON FUNCTION apply_paddle_subscription(text, uuid, text, text, text, timestamptz, text, timestamptz, timestamptz) FROM PUBLIC;
GRANT EXECUTE ON FUNCTION apply_paddle_subscription(text, uuid, text, text, text, timestamptz, text, timestamptz, timestamptz) TO api;
