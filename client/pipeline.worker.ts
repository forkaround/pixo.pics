import { BonsaiImagePipeline } from "bonsai-image-pipeline-lib/lib/bonsai-image.js";
import type { FromElm, ToElm } from "./Main.elm";

export type Command = Extract<
  FromElm,
  {
    tag:
      | "CheckWebGpu"
      | "LoadModel"
      | "GenerateImage"
      | "CancelPipeline"
      | "ReleaseModel"
      | "ClearModelCache";
  }
>;

export type Reply =
  Exclude<ToElm, { tag: "ImageGenerated" }> | { tag: "ImageGenerated"; picture: Blob };

type LoadOptions = Extract<Command, { tag: "LoadModel" }>["data"];
type GenerateOptions = Extract<Command, { tag: "GenerateImage" }>["data"];

const progressInterval = 100;
const largestBatch = 64;

let pipeline: BonsaiImagePipeline | null = null;
let work = new AbortController();
let frameBudget = 0;

const reply = (msg: Reply) => self.postMessage(msg);

const failed = (error: unknown, cancelled: boolean): Reply => ({
  tag: "PipelineFailed",
  name: cancelled ? "AbortError" : error instanceof Error ? error.name : "Error",
  message: error instanceof Error ? error.message : String(error),
});

const pace = ({ runtime: { host } }: BonsaiImagePipeline) => {
  const submit = host.submit;
  let pending = 0;
  let batch = 1;
  host.submit = async (buffers, options) => {
    await submit(buffers, options);
    if (++pending < batch) {
      return;
    }
    pending = 0;
    const started = performance.now();
    await host.device.queue.onSubmittedWorkDone();
    const waited = performance.now() - started;
    if (waited > frameBudget) {
      batch = Math.max(1, batch >> 1);
    } else if (waited < frameBudget / 2) {
      batch = Math.min(largestBatch, batch * 2);
    }
  };
};

const checkWebGpu = async (): Promise<Reply> => {
  try {
    const adapter = await navigator.gpu?.requestAdapter();
    return { tag: "WebGpuChecked", features: adapter ? [...adapter.features] : null };
  } catch {
    return { tag: "WebGpuChecked", features: null };
  }
};

const loadModel = async ({ model, cache, force }: LoadOptions): Promise<Reply> => {
  const { signal } = (work = new AbortController());
  let reported = 0;
  try {
    pipeline = await BonsaiImagePipeline.from_pretrained(model, {
      cache,
      force,
      signal,
      onProgress: ({ component, loaded, total, fromCache }) => {
        if (component === undefined || loaded === undefined || total === undefined) {
          return;
        }
        const now = performance.now();
        if (loaded >= total || now - reported >= progressInterval) {
          reported = now;
          reply({ tag: "ModelProgress", component, loaded, total, fromCache: fromCache === true });
        }
      },
    });
    pace(pipeline);
    return { tag: "ModelLoaded" };
  } catch (error) {
    return failed(error, signal.aborted);
  }
};

const generateImage = async ({
  frameBudget: budget,
  ...options
}: GenerateOptions): Promise<Reply> => {
  const { signal } = (work = new AbortController());
  frameBudget = budget;
  try {
    if (!pipeline) {
      throw new Error("The model is not in memory.");
    }
    const result = await pipeline.generate({
      ...options,
      signal,
      callbackOnStepEnd: (_, step) => reply({ tag: "GenerationStep", step }),
    });
    return { tag: "ImageGenerated", picture: result.toBlob() };
  } catch (error) {
    return failed(error, signal.aborted);
  }
};

const releaseModel = async (): Promise<Reply> => {
  try {
    await pipeline?.destroy();
    pipeline = null;
    return { tag: "ModelReleased" };
  } catch (error) {
    return failed(error, false);
  }
};

const clearModelCache = async (): Promise<Reply> => {
  try {
    await BonsaiImagePipeline.clearCache();
    return { tag: "ModelCacheCleared" };
  } catch (error) {
    return failed(error, false);
  }
};

self.addEventListener("message", ({ data: msg }: MessageEvent<Command>) => {
  switch (msg.tag) {
    case "CheckWebGpu":
      return void checkWebGpu().then(reply);

    case "LoadModel":
      return void loadModel(msg.data).then(reply);

    case "GenerateImage":
      return void generateImage(msg.data).then(reply);

    case "CancelPipeline":
      return work.abort();

    case "ReleaseModel":
      return void releaseModel().then(reply);

    case "ClearModelCache":
      return void clearModelCache().then(reply);

    default:
      return msg satisfies never;
  }
});
