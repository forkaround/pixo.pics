import { MLCEngine, deleteModelAllInfoInCache } from "@mlc-ai/web-llm";
import type { FromElm, ToElm } from "./Main.elm";

export type Command = Extract<
  FromElm,
  { tag: "LoadStylist" | "WritePrompt" | "StopWriting" | "RemoveStylist" }
>;

export type Reply = Extract<
  ToElm,
  {
    tag:
      | "StylistProgress"
      | "StylistLoaded"
      | "PromptStreamed"
      | "PromptWritten"
      | "StylistRemoved"
      | "StylistFailed";
  }
>;

type LoadOptions = Extract<Command, { tag: "LoadStylist" }>["data"];
type WriteOptions = Extract<Command, { tag: "WritePrompt" }>["data"];
type RemoveOptions = Extract<Command, { tag: "RemoveStylist" }>["data"];

let engine: MLCEngine | null = null;

const reply = (msg: Reply) => self.postMessage(msg);

const failed = (error: unknown): Reply => ({
  tag: "StylistFailed",
  name: error instanceof Error ? error.name : "Error",
  message: error instanceof Error ? error.message : String(error),
});

const load = async ({ model }: LoadOptions): Promise<Reply> => {
  try {
    engine ??= new MLCEngine();
    engine.setInitProgressCallback(({ progress }) => reply({ tag: "StylistProgress", progress }));
    await engine.reload(model);
    return { tag: "StylistLoaded" };
  } catch (error) {
    return failed(error);
  }
};

const write = async ({
  messages,
  temperature,
  topP,
  maxTokens,
  thinking,
}: WriteOptions): Promise<Reply> => {
  try {
    if (!engine) {
      throw new Error("The prompt model is not in memory.");
    }
    const stream = await engine.chat.completions.create({
      messages,
      temperature,
      top_p: topP,
      max_tokens: maxTokens,
      stream: true,
      extra_body: { enable_thinking: thinking },
    });
    let text = "";
    for await (const chunk of stream) {
      text += chunk.choices[0]?.delta?.content ?? "";
      reply({ tag: "PromptStreamed", text });
    }
    return { tag: "PromptWritten", text };
  } catch (error) {
    return failed(error);
  }
};

const remove = async ({ model }: RemoveOptions): Promise<Reply> => {
  try {
    await engine?.unload();
    engine = null;
    await deleteModelAllInfoInCache(model);
    return { tag: "StylistRemoved" };
  } catch (error) {
    return failed(error);
  }
};

self.addEventListener("message", ({ data: msg }: MessageEvent<Command>) => {
  switch (msg.tag) {
    case "LoadStylist":
      return void load(msg.data).then(reply);

    case "WritePrompt":
      return void write(msg.data).then(reply);

    case "StopWriting":
      return engine?.interruptGenerate();

    case "RemoveStylist":
      return void remove(msg.data).then(reply);

    default:
      return msg satisfies never;
  }
});
