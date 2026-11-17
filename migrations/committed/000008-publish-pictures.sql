--! Previous: sha1:268dc2158df0b451c507e59684c5b0db5b7a7abc
--! Hash: sha1:66fca64ed6cba7ea608020d05dabd1212038f79f
--! Message: Publish pictures

DROP TABLE IF EXISTS published_pictures CASCADE;
CREATE TABLE published_pictures (
  id uuid PRIMARY KEY DEFAULT gen_random_uuid(),
  user_id uuid NOT NULL REFERENCES users ON DELETE CASCADE,
  prompt text NOT NULL CHECK (prompt = btrim(prompt) AND prompt <> '' AND char_length(prompt) <= 2000),
  width integer NOT NULL CHECK (width BETWEEN 256 AND 1024 AND width % 64 = 0),
  height integer NOT NULL CHECK (height BETWEEN 256 AND 1024 AND height % 64 = 0),
  steps integer NOT NULL CHECK (steps BETWEEN 1 AND 50),
  seed bigint NOT NULL CHECK (seed BETWEEN 0 AND 4294967295),
  model text NOT NULL CHECK (model <> '' AND char_length(model) <= 200),
  bytes integer NOT NULL CHECK (bytes BETWEEN 1 AND 2097152),
  tags text[] NOT NULL DEFAULT '{}' CHECK (
    cardinality(tags) <= 8
    AND array_position(tags, NULL) IS NULL
    AND array_to_string(tags, ',') ~ '^([^,]{1,24}(,[^,]{1,24})*)?$'
    AND array_to_string(tags, ',') = lower(array_to_string(tags, ','))
  ),
  created_at timestamptz NOT NULL DEFAULT now(),
  published_at timestamptz,
  removed_at timestamptz
);

DROP INDEX IF EXISTS published_pictures_user_id_idx;
CREATE INDEX published_pictures_user_id_idx ON published_pictures (user_id);

DROP INDEX IF EXISTS published_pictures_feed_idx;
CREATE INDEX published_pictures_feed_idx ON published_pictures (published_at DESC, id DESC)
  WHERE published_at IS NOT NULL AND removed_at IS NULL;

DROP INDEX IF EXISTS published_pictures_tags_idx;
CREATE INDEX published_pictures_tags_idx ON published_pictures USING gin (tags)
  WHERE published_at IS NOT NULL AND removed_at IS NULL;

ALTER TABLE published_pictures ENABLE ROW LEVEL SECURITY, FORCE ROW LEVEL SECURITY;
CREATE POLICY own_rows ON published_pictures
  USING (user_id = (SELECT NULLIF(current_setting('app.user_id', true), '')::uuid))
  WITH CHECK (user_id = (SELECT NULLIF(current_setting('app.user_id', true), '')::uuid));
GRANT SELECT, DELETE ON published_pictures TO api;
GRANT INSERT (user_id, prompt, width, height, steps, seed, model, bytes, tags) ON published_pictures TO api;
GRANT UPDATE (published_at) ON published_pictures TO api;
