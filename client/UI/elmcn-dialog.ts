export class ElmcnDialog extends HTMLElement {
  #ready = false;
  static observedAttributes = ["open"];

  connectedCallback() {
    this.#connected();
    this.#ready = true;
  }

  attributeChangedCallback() {
    if (this.#ready) {
      this.#connected();
    }
  }

  #connected() {
    const dialog = this.querySelector("dialog");
    const open = this.getAttribute("open") !== null;
    const popover = dialog?.hasAttribute("popover") ?? false;
    const shown = popover ? dialog?.matches(":popover-open") : dialog?.open;

    if (dialog && open && !shown) {
      if (popover) dialog.showPopover();
      else dialog.showModal();
    } else if (dialog && shown) {
      const onTransitionEnd = (event: TransitionEvent) => {
        if (!(event.target instanceof Element) || event.target.parentElement !== dialog) return;
        dialog.removeEventListener("transitionend", onTransitionEnd);
        if (this.getAttribute("open") !== null) return;
        if (popover) dialog.hidePopover();
        else dialog.close();
      };

      dialog.addEventListener("transitionend", onTransitionEnd);
    }
  }
}

if (!customElements.get("elmcn-dialog")) customElements.define("elmcn-dialog", ElmcnDialog);
