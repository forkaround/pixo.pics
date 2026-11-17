# Cloud sync

Read when touching `client/Sync.elm`, `client/Page/Sync.elm` or `/settings/sync`.

- The copy is one way. Nothing in the user's Dropbox or Google Drive is ever deleted or
  overwritten: deleting a picture in pixo leaves its copy where it is, and Dropbox uploads
  use `mode: add` with `autorename`.
- A row's `synced` field maps a provider (`dropbox`, `google-drive`) to the Dropbox path or
  Drive file id of its copy (`Library.markSynced`, label `synced`). A picture without an
  entry for a connected provider is waiting.
- `client/Sync.elm` decides everything. `client/send.ts` (the `SendBlob` port, shared with
  publishing) only sends a blob; the label `<provider>:<picture id>` routes its answer to
  the running job, and `publish:<id>` to `Publish`.
- Access tokens come from `POST /api/sync/<provider>/token` and live only in memory, never
  in a toast, a log or the view. They are forgotten when the account ends or changes.
- The connected providers come with the session (`Me.sync`); no request learns them. It
  runs only for a Pro account, while a tab is open.
- One upload at a time, newest first. Three failures in a row stop a provider until "Try
  again". A 403, 404 or 409 on the token stops it at once.
- Drive needs a `pixo` folder, found or made once per session. A new file lands in My
  Drive's root, so it is renamed and moved into the folder after the upload; when that
  fails the file stays in the root and the picture is tried again later.
