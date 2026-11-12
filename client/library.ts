import type { FromElm, ToElm } from "./Main.elm";

export type Command = Extract<FromElm, { tag: "LoadLibrary" | "SavePicture" | "DeletePicture" }>;

type Picture = Extract<ToElm, { tag: "PictureSaved" }>["picture"];
type Row = Omit<Picture, "url"> & { blob: Blob };

const name = "pictures";

let opened: Promise<IDBDatabase> | undefined;

const open = () =>
  (opened ??= new Promise((resolve, reject) => {
    const request = indexedDB.open("pixo", 1);
    request.onupgradeneeded = () =>
      request.result.createObjectStore(name, { keyPath: "id", autoIncrement: true });
    request.onsuccess = () => resolve(request.result);
    request.onerror = () => reject(request.error);
  }));

const run = async <T>(mode: IDBTransactionMode, ask: (store: IDBObjectStore) => IDBRequest<T>) => {
  const database = await open();
  return new Promise<T>((resolve, reject) => {
    const request = ask(database.transaction(name, mode).objectStore(name));
    request.onsuccess = () => resolve(request.result);
    request.onerror = () => reject(request.error);
  });
};

const shown = ({ blob, ...settings }: Row): Picture => ({
  ...settings,
  url: URL.createObjectURL(blob),
});

const answer = async (msg: Command): Promise<ToElm> => {
  switch (msg.tag) {
    case "LoadLibrary": {
      const rows: Row[] = await run("readonly", (store) => store.getAll());
      return { tag: "LibraryLoaded", pictures: rows.map(shown) };
    }

    case "SavePicture": {
      const { source, ...settings } = msg.data;
      const blob = await (await fetch(source)).blob();
      const id = await run("readwrite", (store) => store.add({ ...settings, blob }));
      return { tag: "PictureSaved", source, picture: shown({ ...settings, blob, id: Number(id) }) };
    }

    case "DeletePicture": {
      await run("readwrite", (store) => store.delete(msg.data.id));
      URL.revokeObjectURL(msg.data.url);
      return { tag: "PictureDeleted", id: msg.data.id };
    }

    default:
      return msg satisfies never;
  }
};

export const library = (msg: Command): Promise<ToElm> =>
  answer(msg).catch((error: unknown) => ({
    tag: "LibraryFailed",
    message: error instanceof Error ? error.message : String(error),
  }));
