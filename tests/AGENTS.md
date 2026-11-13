# Browser tests

Tests of the browser app and the shared modules; the server's are in `api/tests/`
(`api/AGENTS.md`).

- Browser: act as a user does: `fillIn` by id and label, `clickButton` by its text (the
  `sr-only` label on an icon-only control), `clickLink`, then look for text in the view.
- `clickButton` and `clickLink` fail when two elements match. A button repeated per row
  gets an `aria-label` that names its row; where a page and the sidebar both have one,
  as "Try again" and "Sign out" at `/settings/account`, click inside one:
  `ProgramTest.within (Query.find [ tag "section" ])`. At `/` there are two "Sign up".
- `/studio` sends `CheckWebGpu` and `/library` sends `IdbGetAll` as they open: a test
  that reads every port value starts at `/`, which sends none.
- A passing check of outgoing port values clears them: look at logs and responses in one
  `expectSent`, not one after the other.
- Rules a user can't reach from the page (an event out of turn) are tested on the
  `update` alone: `PipelineTest`, `LibraryTest`, `EnrolmentTest`.
