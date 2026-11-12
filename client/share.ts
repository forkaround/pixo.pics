import type { FromElm, ToElm } from "./Main.elm";

type Shared = Extract<FromElm, { tag: "SharePicture" }>["data"];
type Copied = Extract<FromElm, { tag: "CopyPicture" }>["data"];

const picture = (url: string) => fetch(url).then((response) => response.blob());

const failed = (error: unknown): ToElm => ({
  tag: "ShareFailed",
  name: error instanceof Error ? error.name : "Error",
});

export const canShare = () => {
  try {
    return (
      navigator.canShare?.({ files: [new File([], "pixo.png", { type: "image/png" })] }) ?? false
    );
  } catch {
    return false;
  }
};

export const sharePicture = async ({ url, name, text }: Shared): Promise<ToElm | undefined> => {
  try {
    const blob = await picture(url);
    await navigator.share({ files: [new File([blob], name, { type: blob.type })], text });
    return undefined;
  } catch (error) {
    return failed(error);
  }
};

export const copyPicture = async ({ url }: Copied): Promise<ToElm> => {
  try {
    await navigator.clipboard.write([new ClipboardItem({ "image/png": picture(url) })]);
    return { tag: "PictureCopied" };
  } catch (error) {
    return failed(error);
  }
};
