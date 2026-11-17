import type { FromElm, ToElm } from "./Main.elm";

type Asked = Extract<FromElm, { tag: "SendBlob" }>["data"];

export const sendBlob = async ({
  label,
  url,
  method,
  endpoint,
  headers,
  release,
}: Asked): Promise<ToElm> => {
  try {
    const blob = await (await fetch(url)).blob();
    const response = await fetch(endpoint, {
      method,
      headers: Object.fromEntries(headers),
      body: blob,
    });
    return { tag: "BlobSent", label, status: response.status, body: await response.text() };
  } catch (error) {
    return {
      tag: "BlobFailed",
      label,
      message: error instanceof Error ? error.message : String(error),
    };
  } finally {
    if (release) URL.revokeObjectURL(url);
  }
};
