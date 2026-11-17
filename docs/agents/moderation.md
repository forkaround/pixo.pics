# Moderation

Read when handling a reported picture or touching reports and takedowns.

- The first report of a picture emails `MODERATION_EMAIL` with the picture, the profile and
  the SQL to take it down. Unreviewed reports, newest first:

  ```sql
  SELECT r.picture_id, r.reason, r.created_at FROM picture_reports AS r
  WHERE r.reviewed_at IS NULL ORDER BY r.created_at DESC;
  ```

- Take a picture down in Neon's SQL editor (project pixo, branch main, as the owner):

  ```sql
  SELECT * FROM take_down_picture('<picture id>');
  ```

  It returns the file's key, for example `<id>.jpg`. The picture leaves the feed and the
  profile within about two minutes: Vercel's CDN caches those answers for 60 s, then may
  serve the old copy once more while refreshing.

- Delete the file: Cloudflare dashboard, R2, bucket `pixo-pictures`, the object with that
  key, Delete. Or: `cf r2 buckets objects delete <key> --bucket-name pixo-pictures --force`.
- Purge it from Cloudflare's cache: Caching, Configuration, Purge cache, Custom purge, the
  URL `https://img.pixo.pics/<key>`. Browsers may keep it up to an hour (`max-age=3600`).
- Not a problem:

  ```sql
  SELECT dismiss_reports('<picture id>');
  ```

  It marks the picture's reports reviewed.

- Sexual content involving minors: do not download or keep a copy; report it as the law
  where pixo operates requires (in the US, to NCMEC's CyberTipline), and take it down as
  above. Cloudflare's CSAM Scanning Tool is on for `pixo.pics` and reports what it matches.
- The owner of a taken-down picture still has it in their library; unpublishing it there
  deletes the row.
