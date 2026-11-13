# Browser app

The Elm browser app in `client/`: pages, the session, ports and UI.

## Structure

- The `Navigation.Key` is not in the model: `main` keeps it beside it and hands it to
  `perform`.
- `Main` is wiring, the shell and what every page shares: the session, the theme, the
  pipeline and the library. Pages never import `Main` or each other; what several share
  is a module beside `Main`.
- Only `Main` writes `model.session`. A page that changes who is signed in exposes
  `outcome`, which tells it from the message alone, and `Main` applies it whether or not
  the page is still open, so an answer that arrives late still signs in or out. Any
  other message for a page that is no longer open changes nothing and runs no effect.
- `open` starts a page's model anew, which is also what ends an unfinished passkey
  ceremony.
- A session check that fails other than with a 401 is `Unavailable`: the page says so and
  offers "Try again", and sends nobody to `/login`.
- The server is called only through `ApiClient.request` with an `Api.Endpoint`. On an
  error status it decodes `Api.errorDecoder`, then the endpoint's own response: the
  health report comes with a 503.
- Addresses are written only with `Path.toString`: links, `PushUrl`, the server's emails.
- `client/main.css` is the only stylesheet, linked from `index.html`: don't import CSS
  from TypeScript. Inter is served with the app; Explore's Unsplash photos are the only
  thing a page asks another site for.
- Inputs with a placeholder keep their label, `sr-only`: screen readers and the tests'
  `fillIn` need it.

## Adding a page

A page with state of its own is a module in `client/Page/`. A new one is:

1. That module.
2. A `Path` variant, with its line in `Path.parse`'s parser.
3. A `Main.Msg` variant.
4. Its branches in `open`, `update`, `title` and `viewBody`.

## Ports

- Port messages sent from one `update` reach TypeScript in the reverse of the order they
  were listed in, so no two may depend on their order.

## UI

- **UI**: use the elmcn registry component instead of writing one; Phosphor
  (`phosphor-icons/phosphor-elm`, `Regular`; `Bold` beside medium text: the sidebars'
  menus and trigger, the user menu, the theme toggle, the login and sign-up forms)
  instead of hand-written SVG; `sr-only` text on icon-only controls.
- **Skeletons faithfully represent the final element**: match its structure, dimensions,
  spacing and alignment, so replacing the skeleton with loaded content causes no layout
  shift. Update the skeleton whenever the final element changes. The same holds for what
  depends on the session: while it is `Unknown`, draw it `invisible` or as a skeleton of
  the same size.
- Text over a picture is white on a dark gradient whatever the theme, and never sits
  where the picture fades into the page (white on white in the light theme). Buttons
  over a picture take their looks from `client/Overlay.elm`.
- Explore's first screen for a signed-out visitor stays plain: no badge over the
  heading, no gradient text, no list of ticks, nothing that moves.
- A class of ours that must win over a component's own takes Tailwind's `!`
  (`bg-red-600!`): the order of two classes in the attribute decides nothing.

## Read when

| Touching                                                                                           | Read                                                |
| -------------------------------------------------------------------------------------------------- | --------------------------------------------------- |
| `client/UI/`, an elmcn component, the sidebar, the bar, `client/Logo.elm`, `favicon.svg`           | [docs/agents/ui.md](../docs/agents/ui.md)           |
| `Pipeline.elm`, `pipeline.worker.ts`, `bonsai.d.ts`, `Stylist.elm`, `stylist.worker.ts`, `/studio` | [docs/agents/studio.md](../docs/agents/studio.md)   |
| `Library.elm`, `Idb.elm`, `idb.ts`, `Bm25.elm`, `Share.elm`, `share.ts`, `/library`                | [docs/agents/library.md](../docs/agents/library.md) |
| Browser tests in `tests/`                                                                          | [tests/AGENTS.md](../tests/AGENTS.md)               |
