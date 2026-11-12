const coordinate = /-?\d*\.?\d+/g;

class PixoLogo extends HTMLElement {
  #observer = new ResizeObserver(([entry]) => {
    const svg = this.querySelector("svg");
    const path = this.querySelector("path");
    if (!svg || !path) return;

    const { width, height } = svg.getBoundingClientRect();
    if (width <= height) return;
    const progress = Math.max(0, Math.min(1, (width - entry.contentRect.width) / (width - height)));
    let index = 0;
    path.setAttribute(
      "d",
      this.#expanded.replace(coordinate, (value) => {
        const start = Number(value);
        return String(start + (this.#collapsed[index++] - start) * progress);
      }),
    );
  });
  #expanded = "";
  #collapsed: number[] = [];

  connectedCallback() {
    const path = this.querySelector("path");
    if (!path) return;
    this.#expanded ||= path.getAttribute("d") ?? "";
    this.#collapsed = (path.getAttribute("data-collapsed")?.match(coordinate) ?? []).map(Number);
    this.#observer.observe(this);
  }

  disconnectedCallback() {
    this.#observer.disconnect();
  }
}

if (!customElements.get("pixo-logo")) customElements.define("pixo-logo", PixoLogo);
