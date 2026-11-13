import type { FromElm, ToElm } from "./Main.elm";

export type Command = Extract<FromElm, { tag: "IdbGetAll" | "IdbAdd" | "IdbDelete" }>;

type Database = Command["data"]["database"];
type Row = Extract<ToElm, { tag: "IdbAdded" }>["row"];

const opened = new Map<string, Promise<IDBDatabase>>();

const open = ({ name, version, stores }: Database) => {
  const known = opened.get(name);
  if (known) return known;
  const opening = new Promise<IDBDatabase>((resolve, reject) => {
    const request = indexedDB.open(name, version);
    request.onupgradeneeded = () => {
      for (const store of stores) {
        if (!request.result.objectStoreNames.contains(store.name)) {
          request.result.createObjectStore(store.name, {
            keyPath: store.keyPath,
            autoIncrement: store.autoIncrement,
          });
        }
      }
    };
    request.onsuccess = () => resolve(request.result);
    request.onerror = () => reject(request.error);
  });
  opened.set(name, opening);
  opening.catch(() => opened.delete(name));
  return opening;
};

const run = async <T>(
  database: Database,
  name: string,
  mode: IDBTransactionMode,
  ask: (store: IDBObjectStore) => IDBRequest<T>,
) => {
  const connection = await open(database);
  return new Promise<T>((resolve, reject) => {
    const request = ask(connection.transaction(name, mode).objectStore(name));
    request.onsuccess = () => resolve(request.result);
    request.onerror = () => reject(request.error);
  });
};

const shown = (row: Record<string, unknown>) =>
  Object.fromEntries(
    Object.entries(row).map(([field, value]) => [
      field,
      value instanceof Blob ? URL.createObjectURL(value) : value,
    ]),
  ) as Row;

const answer = async (msg: Command): Promise<ToElm> => {
  switch (msg.tag) {
    case "IdbGetAll": {
      const { database, store, label } = msg.data;
      const rows: Record<string, unknown>[] = await run(database, store, "readonly", (objects) =>
        objects.getAll(),
      );
      return { tag: "IdbRows", label, rows: rows.map(shown) };
    }

    case "IdbAdd": {
      const { database, store, row, blobs, label } = msg.data;
      const fetched = await Promise.all(
        blobs.map(async ({ field, url }) => [field, await (await fetch(url)).blob()] as const),
      );
      const stored = { ...row, ...Object.fromEntries(fetched) };
      const key = await run(database, store, "readwrite", (objects) => objects.add(stored));
      const keyPath = database.stores.find((known) => known.name === store)?.keyPath;
      return {
        tag: "IdbAdded",
        label,
        row: shown(keyPath ? { ...stored, [keyPath]: key } : stored),
      };
    }

    case "IdbDelete": {
      const { database, store, key, label } = msg.data;
      await run(database, store, "readwrite", (objects) => objects.delete(key as IDBValidKey));
      return { tag: "IdbDeleted", label, key };
    }

    default:
      return msg satisfies never;
  }
};

export const idb = (msg: Command): Promise<ToElm> =>
  answer(msg).catch((error: unknown) => ({
    tag: "IdbFailed",
    label: msg.data.label,
    message: error instanceof Error ? error.message : String(error),
  }));

export const release = (urls: string[]) => urls.forEach((url) => URL.revokeObjectURL(url));
