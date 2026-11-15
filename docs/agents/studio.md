# Studio

Read when touching the studio: the image pipeline, the prompt writer or their workers.

## Image pipeline

The studio runs
[bonsai-image-pipeline-lib](https://github.com/nishlumi/bonsai-image-pipeline-lib)
(WebGPU), pinned to a commit in `package.json`, in a web worker
(`client/pipeline.worker.ts`). `client/Pipeline.elm` decides everything; the server takes
no part.

- `patches/` holds a Bun patch of the library (`patchedDependencies` in `package.json`),
  measured on an M2 Max in Chrome 157, which has `subgroups` but no
  `chromium-experimental-subgroup-matrix`: a tiled flash attention for head dim 128,
  taller matmul tiles, a chunked matmul for the VAE attention, the text encoder's 4-bit
  weights expanded to f16 before its matmuls, and the last prompt's encoding kept for a
  repeat. A 1024 by 1024 picture went from 46 s to 24 s, and every preset takes 30 to
  48% less time. It also fixes the VAE attention of pictures of 512 by 512 or less on such
  GPUs: the library read V transposed. When the pinned commit moves, redo the patch with
  `bun patch` and measure again.
- `Main` keeps the pipeline in `model.pipeline`, not in a page: the model stays in the
  graphics card's memory across pages.
- An event is applied only in the state that waits for it, so one that arrives late
  changes nothing. The library runs one job at a time: a cancel waits for it to stop
  (`PipelineFailed` named `AbortError`), in `Cancelling` for a download and `Stopping`
  for a picture, and only then shows no toast. A graphics card that drops the device
  mid-picture rejects with the same name, and that one is shown: the model in memory is
  then unusable until released and loaded again. A model that finishes loading as its
  download is cancelled is kept, and its picture is not made.
- A label for work in progress uses the verb of what is actually happening: the button's
  own (Cancel, "Cancelling…"; Stop, "Stopping…"), or the real step where it differs
  ("Downloading" a part, but "Loading" one from the cache).
- `model.pipeline.device` says whether the browser has WebGPU: `Unchecked`, `Checking`,
  `Unsupported` with a `Storage` or `Supported` with the `Engine`, so the model can only
  load, generate or be released on a device that can run it. A browser without
  `navigator.gpu` starts `Unsupported` and is never checked. A failure before the check
  answers (the worker didn't start) makes it `Unsupported Unreachable` too: a check sent
  to that worker would never come back. Without WebGPU the worker still answers, so
  `Unsupported Kept` can remove the downloaded models. A worker that fails while nothing
  was asked of it makes it `Unsupported Unreachable`, which can't, and says to reload.
  `Supported` carries `Prompting` as well as the `Engine`. The check reports the
  adapter's features and Elm decides: the prompt model needs `shader-f16` (87 of the 106
  shaders in its wasm `enable f16`, though WebLLM's record doesn't say so), so
  `Supported` is `CanPrompt` or `NoShaderF16`; the image model runs either way. Check
  the wasm again when `modelId` or WebLLM changes.
- The library can't tell whether its cache holds a whole model, so
  `model.pipeline.downloaded` (in `localStorage`) lists the weights that finished
  loading. Models share one cache: "Remove downloaded models" removes them all, and
  works only with no model in memory.
- `Weights` lists only `-mlx-` models: `-gemlite-` ones are for PyTorch and answer 404.
  `Pipeline.weightsSize` was read from a real download: change it with the list.
- What the library takes: a `Dimension` in multiples of 16 (ours: 256 to 1024 in steps
  of 64, and a preset must be on that grid), a seed read as 32 bits, `guidanceScale` 1
  and nothing else. The studio offers only `Pipeline.presets`, and starts at Square
  (1:1, `Dimension.default`). Steps are typed, kept as text and read when Generate is
  pressed, as the seed is.
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
- The prompt, steps and seed are checked in Elm, not by the browser: the form is
  `novalidate`, and steps and seed are `type="number"`, which keeps out letters. Their
  `min` and `max` (from `Steps` and `Seed`) and `step` only keep the arrow keys in
  range: HTML can't stop a typed minus sign or a number too large, and `novalidate`
  keeps them from blocking Generate. `Pipeline.checks`
  (`Stylist.checks`) derive each problem from the current values, so each one hides as
  soon as its value is right. Steps and seed are checked as they are typed; an empty
  prompt is where every picture starts, so it is named only once Generate (or AI Style)
  is pressed without one, which sets `checked` (`needsIdea`). A picture (or rewrite)
  that starts resets it.
- The prompt card shows those problems in one band, each line opening and closing with
  its own transition. The outcomes of `Pipeline` and `Stylist` (downloads, pictures,
  removals, AI Style) are toasts, not lines in the card. A closed line is `inert` and drops its `role="alert"`, and
  an open one is a new node, so a screen reader hears it each time it opens: a test that
  a problem went away looks for it under `inert`.
- An empty seed field is the random seed (placeholder "-1"): typing a number fixes
  it and emptying the field makes it random again. A number input holding text the
  browser can't read (`1e`, `123-123`) reports `""` too, and hides the text from Elm,
  so `onReadableInput` sends `Unreadable` when `validity.badInput` is set instead of the
  value: the field is named as a problem and blocks the picture. While it is, the
  view gives the input the value `""`, what the browser reports: Elm writes `value`
  whenever it differs from the DOM's, and anything else would replace what was typed.
  The next readable value clears it.
- A random seed is drawn in Elm from the `entropy` flag, with no `Effect`: a test fixes
  it through the flags.
- `client/bonsai.d.ts` types the library, which ships no types: keep it to what the
  worker calls. The worker imports `Main.elm` for its types only: Elm never runs there.

## Prompt writer

AI Style, in the studio's prompt box, rewrites a short idea as a FLUX.2 prompt with
[WebLLM](https://github.com/mlc-ai/web-llm) in a web worker (`client/stylist.worker.ts`),
which `main.ts` starts on the first command. `client/Stylist.elm` decides everything: the
model, the styles, their examples, the instructions, the sampling and what is kept of the
answer.

- The model is `Qwen3-1.7B-q4f16_1-MLC` (984 MB, about 3 s a prompt on an M2 Max). On
  2026-11-13 it was measured against Qwen3.5 0.8B, 2B and 4B, Gemma 3 1B and Llama 3.2
  1B and 3B, on 24 ideas in every style. Smaller ones changed the idea (red sneakers came
  back black), Llama broke the format, Qwen3.5 2B looped and took twice as long, and 4B
  is 2.4 GB beside a 3.4 GB image model. On 2026-11-15 Gemma 4 E2B (q4f16) wrote more
  coherent prompts on 66 cases (12 ideas in 5 styles and 3 frames, 6 edgy ones), with the
  same format and no refusals, but took 10 s a prompt against 3 s and is a 2.8 GB
  download. Neither refused war, nudity in art, bribery or boudoir ideas. Qwen3 0.6B, on
  the same cases, was faster (2 s) but turned the yellow net red, looped ("a white
  office with a white table"), muddled the bribe and wrote "1:1 ratio" once. Change
  `modelId` and `modelSize` together, and measure again.
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
- While it writes, AI Style holds the prompt: `Stylist.update` hands it over (`Took`,
  then `Gave` with the answer or the idea) and `Pipeline.Draft` is `Styling`, so the
  studio has no prompt to make a picture from. The box shows `Stylist.shown` meanwhile.
- The instructions give the picture's orientation only (`Stylist.Frame`: square, tall
  or wide), and the Camera line asks for a composition that fills it. Given the size and
  ratio ("768 by 960 pixels, 4:5"), the model copied them into almost every answer, and
  FLUX.2 gets the size on its own. The examples name no ratio or widescreen framing of
  their own, which would contradict a square or tall picture.
- WebLLM keeps the model in the Cache API (`webllm/*`), apart from Bonsai's IndexedDB:
  "Remove downloaded models" leaves it, and the Prompt writer card removes it.
  `stylist` in `localStorage` holds the id of the model that finished loading.
- Writing leaves the page's thread free (no long task) and stalls the graphics card under
  8 ms: the readPixels probe above.
- The first time the dev server sees the worker, Vite optimizes `@mlc-ai/web-llm` and
  reloads the page.
