import { initializePaddle, type Paddle } from "@paddle/paddle-js";
import type { FromElm, ToElm } from "./Main.elm";

type Checkout = Extract<FromElm, { tag: "OpenCheckout" }>["data"];

let paddle: Promise<Paddle | undefined> | null = null;

export const openCheckout = async (checkout: Checkout, send: (msg: ToElm) => void) => {
  paddle ??= initializePaddle({
    token: checkout.clientToken,
    environment: checkout.environment === "production" ? "production" : "sandbox",
    eventCallback: (event) => {
      if (event.name) send({ tag: "CheckoutEvent", name: event.name });
    },
  });
  const ready = await paddle.catch(() => undefined);
  if (!ready) return send({ tag: "CheckoutEvent", name: "checkout.error" });
  ready.Checkout.open({
    transactionId: checkout.transactionId,
    settings: { displayMode: "overlay", variant: "one-page", allowLogout: false },
  });
};
