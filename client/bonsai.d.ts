declare module "bonsai-image-pipeline-lib/lib/bonsai-image.js" {
  export interface LoadStatus {
    component?: "text_encoder" | "transformer" | "vae";
    loaded?: number;
    total?: number;
    fromCache?: boolean;
  }

  export interface LoadOptions {
    onProgress?: (status: LoadStatus) => void;
    cache?: boolean;
    cacheName?: string;
    force?: boolean;
    signal?: AbortSignal;
  }

  export interface GenerateOptions {
    prompt: string;
    width?: number;
    height?: number;
    numInferenceSteps?: number;
    seed?: number;
    guidanceScale?: number;
    signal?: AbortSignal;
    callbackOnStepEnd?: (pipeline: BonsaiImagePipeline, step: number) => void;
  }

  export interface Host {
    device: { queue: { onSubmittedWorkDone(): Promise<undefined> } };
    submit: (buffers: unknown[], options?: { wait?: boolean }) => Promise<void>;
  }

  export class BonsaiImageResult {
    bytes: Uint8Array;
    width: number;
    height: number;
    prompt: string;
    seed: number;
    toBlob(): Blob;
    toDataURL(): Promise<string>;
    toImageBitmap(): Promise<ImageBitmap>;
  }

  export class BonsaiImagePipeline {
    static isSupported(): Promise<boolean>;
    static from_pretrained(
      modelId: string | null,
      options?: LoadOptions,
    ): Promise<BonsaiImagePipeline>;
    static clearCache(cacheName?: string | null): Promise<void>;
    runtime: { host: Host };
    getModelName(): string;
    generate(options: GenerateOptions): Promise<BonsaiImageResult>;
    destroy(): Promise<void>;
  }
}
