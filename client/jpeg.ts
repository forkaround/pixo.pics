import type { FromElm, ToElm } from "./Main.elm";

type Asked = Extract<FromElm, { tag: "EncodePicture" }>["data"];

export const encodePicture = async ({ label, url, quality }: Asked): Promise<ToElm> => {
  try {
    const bitmap = await createImageBitmap(await (await fetch(url)).blob());
    const canvas = new OffscreenCanvas(bitmap.width, bitmap.height);
    canvas.getContext("2d")?.drawImage(bitmap, 0, 0);
    bitmap.close();
    const blob = await canvas.convertToBlob({ type: "image/jpeg", quality });
    return { tag: "PictureEncoded", label, url: URL.createObjectURL(blob), bytes: blob.size };
  } catch (error) {
    return {
      tag: "PictureEncodeFailed",
      label,
      message: error instanceof Error ? error.message : String(error),
    };
  }
};
