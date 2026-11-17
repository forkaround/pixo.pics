--! Previous: sha1:41f5e30da7fa0db89feedb3e873eaf644f70bdb7
--! Hash: sha1:87f744664fc3c13105efd66668dda8c1c8bc42e9
--! Message: Report and take down pictures

DROP TABLE IF EXISTS picture_reports CASCADE;
CREATE TABLE picture_reports (
  id uuid PRIMARY KEY DEFAULT gen_random_uuid(),
  picture_id uuid NOT NULL REFERENCES published_pictures ON DELETE CASCADE,
  reason text NOT NULL CHECK (reason IN ('illegal', 'sexual', 'violent', 'hateful', 'spam', 'other')),
  created_at timestamptz NOT NULL DEFAULT now(),
  reviewed_at timestamptz
);

DROP INDEX IF EXISTS picture_reports_picture_id_idx;
CREATE INDEX picture_reports_picture_id_idx ON picture_reports (picture_id);

ALTER TABLE picture_reports ENABLE ROW LEVEL SECURITY, FORCE ROW LEVEL SECURITY;

DROP FUNCTION IF EXISTS report_picture(uuid, text) CASCADE;
CREATE OR REPLACE FUNCTION report_picture(p_picture_id uuid, p_reason text)
RETURNS TABLE (outcome text, first boolean, username text)
LANGUAGE sql
SECURITY DEFINER
SET search_path = public, pg_temp
AS $$
  WITH target AS (
    SELECT p.id, u.username
    FROM published_pictures AS p
    JOIN users AS u ON u.id = p.user_id
    WHERE p.id = p_picture_id
      AND p.published_at IS NOT NULL
      AND p.removed_at IS NULL
  ), earlier AS (
    SELECT count(*) AS n FROM picture_reports AS r WHERE r.picture_id = p_picture_id
  ), added AS (
    INSERT INTO picture_reports (picture_id, reason)
    SELECT target.id, p_reason FROM target
    RETURNING picture_id
  )
  SELECT CASE WHEN EXISTS (SELECT 1 FROM added) THEN 'reported' ELSE 'not-found' END,
         (SELECT n FROM earlier) = 0,
         (SELECT target.username FROM target);
$$;

REVOKE EXECUTE ON FUNCTION report_picture(uuid, text) FROM PUBLIC;
GRANT EXECUTE ON FUNCTION report_picture(uuid, text) TO api;

DROP FUNCTION IF EXISTS take_down_picture(uuid) CASCADE;
CREATE OR REPLACE FUNCTION take_down_picture(p_picture_id uuid)
RETURNS TABLE (object_key text, username text, reports bigint)
LANGUAGE sql
SET search_path = public, pg_temp
AS $$
  WITH reviewed AS (
    UPDATE picture_reports SET reviewed_at = now()
    WHERE picture_id = p_picture_id AND reviewed_at IS NULL
    RETURNING id
  ), removed AS (
    UPDATE published_pictures AS p SET removed_at = now()
    FROM users AS u
    WHERE p.id = p_picture_id AND u.id = p.user_id AND p.removed_at IS NULL
    RETURNING p.id, u.username
  )
  SELECT removed.id::text || '.jpg', removed.username, (SELECT count(*) FROM reviewed)
  FROM removed;
$$;

REVOKE EXECUTE ON FUNCTION take_down_picture(uuid) FROM PUBLIC;

DROP FUNCTION IF EXISTS dismiss_reports(uuid) CASCADE;
CREATE OR REPLACE FUNCTION dismiss_reports(p_picture_id uuid)
RETURNS bigint
LANGUAGE sql
SET search_path = public, pg_temp
AS $$
  WITH reviewed AS (
    UPDATE picture_reports SET reviewed_at = now()
    WHERE picture_id = p_picture_id AND reviewed_at IS NULL
    RETURNING id
  )
  SELECT count(*) FROM reviewed;
$$;

REVOKE EXECUTE ON FUNCTION dismiss_reports(uuid) FROM PUBLIC;
