# Studio

Read when touching the studio: the image pipeline, the prompt writer or their workers.

## Image pipeline

The studio runs
[bonsai-image-pipeline-lib](https://github.com/nishlumi/bonsai-image-pipeline-lib)
(WebGPU), pinned to a commit in `package.json`, in a web worker
(`client/pipeline.worker.ts`). `client/Pipeline.elm` decides everything; the server takes
no part.

- `Main` keeps the pipeline in `model.pipeline`, not in a page: the model stays in the
  graphics card's memory across pages.
- An event is applied only in the state that waits for it, so one that arrives late
  changes nothing. The library runs one job at a time: a cancel waits for it to stop
  (`PipelineFailed` named `AbortError`, which shows no message). A graphics card that
  drops the device mid-picture rejects with the same name, and that one is shown: the
  model in memory is then unusable until released and loaded again.
- The library can't tell whether its cache holds a whole model, so
  `model.pipeline.downloaded` (in `localStorage`) lists the weights that finished
  loading. Models share one cache: "Remove downloaded models" removes them all, and
  works only with no model in memory.
- `Weights` lists only `-mlx-` models: `-gemlite-` ones are for PyTorch and answer 404.
  `Pipeline.weightsSize` was read from a real download: change it with the list.
- What the library takes: a `Dimension` in multiples of 16 (ours: 256 to 1024 in steps
  of 64, and a preset must be on that grid), a seed read as 32 bits, `guidanceScale` 1
  and nothing else.
- A worker frees the page's thread, not the graphics card, which also draws the page.
  The library submits some 350 passes a step without waiting for the card, and the
  browser's frames queue behind them: stalls of 1.4 s. So `pace` in the worker wraps the
  library's `runtime.host.submit` and awaits `onSubmittedWorkDone` after each batch,
  sized to `frameBudget` (always `Pipeline.frameBudget`, 8 ms). It reaches into the
  library: check it when the pinned commit moves. It can't split one pass: at 1024 by
  1024 the page still stutters, and only the library's shaders could change that.
- To measure a stall, time a 1 by 1 WebGL `readPixels` on the page while the worker
  generates: idle it takes under 1 ms. `requestAnimationFrame` tells nothing: it keeps
  its rhythm while nothing on the page changes.
- The library reports progress some 300 times a second; the worker sends on one report
  every 100 ms.
- The worker answers with the PNG as a `Blob`, and `main.ts` makes the object URL: one
  made in the worker would not outlive it.
- WebGPU is asked for inside the worker: a browser that has it only on the page can't
  make pictures.
- A random seed is drawn in Elm from the `entropy` flag, with no `Effect`: a test fixes
  it through the flags.
- `client/bonsai.d.ts` types the library, which ships no types: keep it to what the
  worker calls. The worker imports `Main.elm` for its types only: Elm never runs there.

## Prompt writer

AI Style, under the studio's prompt, rewrites a short idea as a FLUX.2 prompt with
[WebLLM](https://github.com/mlc-ai/web-llm) in a web worker (`client/stylist.worker.ts`),
which `main.ts` starts on the first command. `client/Stylist.elm` decides everything: the
model, the styles, their examples, the instructions, the sampling and what is kept of the
answer.

- The model is `Qwen3-1.7B-q4f16_1-MLC` (984 MB, about 3 s a prompt on an M2 Max). On
  2026-11-13 it was measured against Qwen3.5 0.8B, 2B and 4B, Gemma 3 1B and Llama 3.2
  1B and 3B, on 24 ideas in every style. Smaller ones changed the idea (red sneakers came
  back black), Llama broke the format, Qwen3.5 2B looped and took twice as long, and 4B
  is 2.4 GB beside a 3.4 GB image model. Change `modelId` and `modelSize` together, and
  measure again.
- The answer is six labelled lines, subject first, as FLUX.2 weighs earlier words more:
  a person changes one line. Bonsai reads only 512 tokens; the answers are about 90 words.
- The examples steer it more than the instructions. Without them the format breaks.
  Two per style, as earlier chat turns with different subjects, so only the style words
  carry over: with one, its props turn up in every answer. A rule that forbids something
  changes nothing; an example that shows it does. A dressed person in an example dresses
  animals (a fisherman's sweater put a coat on the fox), so the Photo examples are a
  person described by face and an animal outdoors.
- Qwen3 answers with an empty `<think></think>` even with thinking off. `Stylist.tidy`
  drops it and keeps the first line of each label, also while the answer streams in.
- Another style rewrites the first idea while the prompt is still the last answer, so
  styles don't pile up. Undo gives the idea back.
- WebLLM keeps the model in the Cache API (`webllm/*`), apart from Bonsai's IndexedDB:
  "Remove downloaded models" leaves it, and the Prompt writer card removes it.
  `stylist` in `localStorage` holds the id of the model that finished loading.
- Writing leaves the page's thread free (no long task) and stalls the graphics card under
  8 ms: the readPixels probe above.
- The first time the dev server sees the worker, Vite optimizes `@mlc-ai/web-llm` and
  reloads the page.
