import { MLCEngine } from "@mlc-ai/web-llm";
import type { FromElm, ToElm } from "./Main.elm";

export type Command = Extract<FromElm, { tag: "LoadTagger" | "TagPicture" | "ReleaseTagger" }>;

export type Reply = Extract<
  ToElm,
  { tag: "TaggerLoaded" | "TagsWritten" | "TaggerReleased" | "TaggerFailed" }
>;

type LoadOptions = Extract<Command, { tag: "LoadTagger" }>["data"];
type TagOptions = Extract<Command, { tag: "TagPicture" }>["data"];

let engine: MLCEngine | null = null;

const reply = (msg: Reply) => self.postMessage(msg);

const failed = (error: unknown): Reply => ({
  tag: "TaggerFailed",
  name: error instanceof Error ? error.name : "Error",
  message: error instanceof Error ? error.message : String(error),
});

const load = async ({ model }: LoadOptions): Promise<Reply> => {
  try {
    engine ??= new MLCEngine();
    await engine.reload(model);
    return { tag: "TaggerLoaded" };
  } catch (error) {
    return failed(error);
  }
};

const tag = async ({
  messages,
  temperature,
  topP,
  maxTokens,
  thinking,
}: TagOptions): Promise<Reply> => {
  try {
    if (!engine) {
      throw new Error("The tagging model is not in memory.");
    }
    const answer = await engine.chat.completions.create({
      messages,
      temperature,
      top_p: topP,
      max_tokens: maxTokens,
      stream: false,
      extra_body: { enable_thinking: thinking },
    });
    return { tag: "TagsWritten", text: answer.choices[0]?.message?.content ?? "" };
  } catch (error) {
    return failed(error);
  }
};

const release = async (): Promise<Reply> => {
  try {
    await engine?.unload();
    engine = null;
    return { tag: "TaggerReleased" };
  } catch (error) {
    return failed(error);
  }
};

self.addEventListener("message", ({ data: msg }: MessageEvent<Command>) => {
  switch (msg.tag) {
    case "LoadTagger":
      return void load(msg.data).then(reply);

    case "TagPicture":
      return void tag(msg.data).then(reply);

    case "ReleaseTagger":
      return void release().then(reply);

    default:
      return msg satisfies never;
  }
});
