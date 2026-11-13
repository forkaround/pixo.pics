# Library

Read when touching saved pictures: `Library.elm`, `Idb.elm`, `idb.ts`, search or sharing.

Saved pictures live in the browser's IndexedDB; `client/Library.elm` decides everything,
down to the database's name and a row's fields. `client/Idb.elm` and `client/idb.ts` are
IndexedDB for Elm and know nothing of pictures.

- Browsers already hold the database: its name (`pixo`), its version, the store
  (`pictures`), its key (`id`) and the names of a row's fields can't change without
  losing the pictures people saved. A new field is decoded with a default.
- Elm can't hold a `Blob`: `Idb.Add` takes `blobs`, a field and an object URL each, and
  the driver stores the `Blob` behind the URL in that field. A `Blob` it reads comes
  back as a new object URL in the same field, which `Idb.Release` revokes.
- There is no command to open a database: every `Idb.Command` carries the `Database` it
  is for, and the driver opens it on first use.
- Every `Idb.Event` goes to the library, the driver's only user
  (`InteropDefinitions.answered`): a second user needs them told apart, by `label`.
- A stored row that doesn't parse into a `Picture` is left out, not an error.
- Search (`client/Bm25.elm`) folds accents by hand, since Elm has no Unicode
  normalisation: a Latin letter missing from `plain` is added there.
- A picture has no address to share, so sharing hands over the file: a network copies
  the picture and opens its composer, to paste it there. An `AbortError` from the share
  sheet is a closed sheet, and shows nothing.
