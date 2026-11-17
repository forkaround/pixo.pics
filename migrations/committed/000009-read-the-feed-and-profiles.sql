--! Previous: sha1:66fca64ed6cba7ea608020d05dabd1212038f79f
--! Hash: sha1:41f5e30da7fa0db89feedb3e873eaf644f70bdb7
--! Message: Read the feed and profiles

DROP FUNCTION IF EXISTS feed_pictures(text, uuid, integer) CASCADE;
CREATE OR REPLACE FUNCTION feed_pictures(p_tag text, p_before uuid, p_limit integer)
RETURNS TABLE (
  id uuid,
  username text,
  prompt text,
  width integer,
  height integer,
  steps integer,
  seed bigint,
  model text,
  tags text[],
  published_at text
)
LANGUAGE plpgsql
STABLE
SECURITY DEFINER
SET search_path = public, pg_temp
AS $$
#variable_conflict use_column
BEGIN
  IF p_tag IS NULL THEN
    RETURN QUERY
    SELECT p.id, u.username, p.prompt, p.width, p.height, p.steps, p.seed, p.model, p.tags,
           to_char(p.published_at AT TIME ZONE 'UTC', 'YYYY-MM-DD"T"HH24:MI:SS"Z"')
    FROM published_pictures AS p
    JOIN users AS u ON u.id = p.user_id
    WHERE p.published_at IS NOT NULL
      AND p.removed_at IS NULL
      AND is_pro(p.user_id)
      AND (
        p_before IS NULL
        OR (p.published_at, p.id) < (
          SELECT b.published_at, b.id FROM published_pictures AS b WHERE b.id = p_before
        )
      )
    ORDER BY p.published_at DESC, p.id DESC
    LIMIT LEAST(GREATEST(p_limit, 1), 60);
  ELSE
    RETURN QUERY
    SELECT p.id, u.username, p.prompt, p.width, p.height, p.steps, p.seed, p.model, p.tags,
           to_char(p.published_at AT TIME ZONE 'UTC', 'YYYY-MM-DD"T"HH24:MI:SS"Z"')
    FROM published_pictures AS p
    JOIN users AS u ON u.id = p.user_id
    WHERE p.published_at IS NOT NULL
      AND p.removed_at IS NULL
      AND p.tags @> ARRAY[p_tag]
      AND is_pro(p.user_id)
      AND (
        p_before IS NULL
        OR (p.published_at, p.id) < (
          SELECT b.published_at, b.id FROM published_pictures AS b WHERE b.id = p_before
        )
      )
    ORDER BY p.published_at DESC, p.id DESC
    LIMIT LEAST(GREATEST(p_limit, 1), 60);
  END IF;
END;
$$;

REVOKE EXECUTE ON FUNCTION feed_pictures(text, uuid, integer) FROM PUBLIC;
GRANT EXECUTE ON FUNCTION feed_pictures(text, uuid, integer) TO api;

DROP FUNCTION IF EXISTS feed_tags(integer) CASCADE;
CREATE OR REPLACE FUNCTION feed_tags(p_limit integer)
RETURNS TABLE (tag text, pictures bigint)
LANGUAGE sql
STABLE
SECURITY DEFINER
SET search_path = public, pg_temp
AS $$
  SELECT t.tag, count(*) AS pictures
  FROM published_pictures AS p
  CROSS JOIN LATERAL unnest(p.tags) AS t(tag)
  WHERE p.published_at IS NOT NULL
    AND p.removed_at IS NULL
    AND is_pro(p.user_id)
  GROUP BY t.tag
  ORDER BY count(*) DESC, t.tag
  LIMIT LEAST(GREATEST(p_limit, 1), 50);
$$;

REVOKE EXECUTE ON FUNCTION feed_tags(integer) FROM PUBLIC;
GRANT EXECUTE ON FUNCTION feed_tags(integer) TO api;

DROP FUNCTION IF EXISTS profile(text, uuid, integer) CASCADE;
CREATE OR REPLACE FUNCTION profile(p_username text, p_before uuid, p_limit integer)
RETURNS TABLE (username text, pictures json)
LANGUAGE sql
STABLE
SECURITY DEFINER
SET search_path = public, pg_temp
AS $$
  SELECT u.username,
         COALESCE((
           SELECT json_agg(page ORDER BY page.sort_at DESC, page.id DESC)
           FROM (
             SELECT p.id, p.prompt, p.width, p.height, p.steps, p.seed, p.model, p.tags,
                    to_char(p.published_at AT TIME ZONE 'UTC', 'YYYY-MM-DD"T"HH24:MI:SS"Z"') AS published_at,
                    p.published_at AS sort_at
             FROM published_pictures AS p
             WHERE p.user_id = u.id
               AND p.published_at IS NOT NULL
               AND p.removed_at IS NULL
               AND (
                 p_before IS NULL
                 OR (p.published_at, p.id) < (
                   SELECT b.published_at, b.id FROM published_pictures AS b
                   WHERE b.id = p_before AND b.user_id = u.id
                 )
               )
             ORDER BY p.published_at DESC, p.id DESC
             LIMIT LEAST(GREATEST(p_limit, 1), 60)
           ) AS page
         ), '[]'::json)
  FROM users AS u
  WHERE u.username = lower(btrim(p_username))
    AND is_pro(u.id);
$$;

REVOKE EXECUTE ON FUNCTION profile(text, uuid, integer) FROM PUBLIC;
GRANT EXECUTE ON FUNCTION profile(text, uuid, integer) TO api;
