# UI components and layout

Read when touching `client/UI/`, an elmcn component, the sidebar, the bar, `client/Logo.elm` or
`favicon.svg`.

## elmcn components

shadcn/ui for Elm, copied into `client/UI/` and ours to change. Read a module for its API.

```sh
npx shadcn@latest search ricardocasares/elmcn -q dialog
npx shadcn@latest view ricardocasares/elmcn/dialog
npx shadcn@latest add ricardocasares/elmcn/dialog
mv -n src/UI/* client/UI/ && ls src/UI
```

- The registry installs into `src/UI/`. `mv -n` keeps existing (maybe edited) components;
  whatever `ls` still lists, merge by hand, then delete `src/`. `--diff` can't see
  `client/UI/`.
- `UI.Sidebar`, `UI.Select`, `UI.DropdownMenu`, `UI.Empty` and `UI.Slider` are changed from the registry's: adding
  them again would undo it, so merge by hand. `DropdownMenu.minHeight` adds up
  `itemHeight`, `menuPadding` and `separatorHeight`: change them together.
- Follow the CLI's notes: some items need an Elm package (`elm install` at the root) or
  a script import in `client/main.ts`. Strip their comments, keeping the `-- SECTION`
  headings.

## The bar

- The bar's sticky element is half the column wide and the `header` inside it twice
  that. iOS Safari covers the status bar with a solid colour when a sticky or fixed
  element at least 90% of the viewport wide sits at the top edge (WebKit's
  `LocalFrameView::fixedContainerEdges`), and lets the page show through otherwise. The
  bar's backdrop reaches 6rem above it, to blur what scrolls under the status bar.
  Safari clips a sticky element to the viewport unless a composited layer holds it
  (`RenderLayerCompositor::viewportConstrainedSublayers`): that is the rail around it,
  as tall as the page, with `will-change-transform`. The rail takes no clicks.

## Logo

- `client/Logo.elm` is the one SVG drawn by hand. Elm interpolates between its expanded
  and collapsed paths, which must have the same commands and coordinate count. Its paths
  run along the middle of a 5-unit stroke and the view box leaves room for it: change
  the stroke and both change. `public/favicon.svg` copies the X's path: a change goes in
  both. The sidebar's animation is Elm frames driving `--sidebar-progress`, with no CSS
  transition or TypeScript.
- The X is drawn apart from the other letters, as `glass`: while the app works in the
  background, generating or tagging (`Main.work`), it is an hourglass whose sand drains,
  then it turns 180° and runs again. A new run (idle to working) restarts the sand. Elm
  only counts the turns (`Time.every` in `Main`) and keys the sand by them, so each turn
  restarts its CSS animation; the turn itself and the fade back are CSS transitions.
  Elm frames would redraw the whole page for the minutes a picture takes. On `md` the
  glass follows `--sidebar-progress` by the X's 68.5-unit move, and the letters, which
  collapse onto the X's lines, fade out while it runs so they don't stay behind it.
- The logo's tooltip names that work and has its own anchor (`logoTooltip` in `Main`),
  since `Sidebar.tooltipClass` shows only while the sidebar is collapsed.
