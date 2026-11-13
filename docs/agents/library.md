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

## Tags

- Saving a picture asks `client/Tagger.elm` for tags once the row is stored.
  Tagging is best effort: a failure leaves the picture without tags and shows
  nothing (`Idb.Patch` with label `tags`).
- The tagger reads the prompt, not the pixels: the prompt is what the picture
  was made from, and a WebLLM vision model needs about 4 GB beside the 3.4 GB
  image model.
- It runs the prompt writer's model (`Stylist.modelId`) in its own worker,
  `client/tagger.worker.ts`, from the same Cache API copy, so it never
  downloads: it runs only when the prompt writer is downloaded and the card
  has `shader-f16`. It loads for a queue and unloads when the queue is empty,
  so the model doesn't stay in memory beside the image model.
- The prompt writer can't be removed while the tagger works: WebLLM would
  download the weights again inside the tagger.
- A row without `tags` was never tagged. An answer with no usable tag stores
  nothing. `Tagger` keeps the ids it tried, so a failure is not retried in a
  loop.
- `Idb.Patch` reads a row and writes it back with new fields in one
  transaction; a row deleted meanwhile fails it.
- `/library` filters by one tag at a time from its side column. The tag is
  kept in the library's model like the search query, not in the address. A
  chosen tag that no picture carries any more is dropped (`Library.chosen`).
- Pictures without tags in the loaded library are asked for after every
  update (`Main.backfill`), newest first, unless a picture is generating. The
  graphics card is checked on `/studio` and `/settings/models`, not
  `/library`, so older pictures are tagged after a visit to one of those.
