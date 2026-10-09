# Changelog

All notable changes to this project are documented here. Format follows
[Keep a Changelog](https://keepachangelog.com/en/1.1.0/). See AGENTS.md
("Change Discipline") for how entries and commits are written.

## [Unreleased]

### Tests
- **The test suite loads and runs again.** It had not compiled since the
  verbs were renamed with a leading dot (commit 9c62711): the test package
  `:use`d both FiveAM and cl-dplyr, which both export `!` (package
  conflict), and every test still called `filter`, `mutate`, `pull`, ...
  instead of `.filter`, `.mutate`, `.pull`. The test package now
  shadowing-imports `cl-dplyr:!` and the tests call the dotted verbs.
  Result: 64/67 checks pass; the 3 failures are genuine bugs (`t` in
  `case-when`, `.desc` in `arrange`, join columns), fixed in the next
  commits (topic: test-suite repair).

### Fixed
- **`!=` and `/=` were mapped to equality** in the DSL, so
  `(filter (!= :x 1))` kept exactly the rows where x = 1. They now map to
  `cl-vctrs-lite:v!=` (topic: DSL resolution).
- **`&` and `|` always signalled an error**: the local `v-and`/`v-or`
  called `col-map` with two columns, which it does not accept. The DSL now
  uses cl-vctrs-lite's NA-aware Kleene `v-and`/`v-or`/`v-not`, so
  `(! x)` keeps NA as NA instead of turning it into TRUE/FALSE.
- **`t` in `case-when` was read as a column** named "t"
  (`Column not found: "t"`), so the documented default clause
  `(t "Junior")` never worked. Constants (`t`, `nil`, `pi`, any
  `defconstant`) are no longer rewritten into column references.
- **Function names and quoted data inside verbs were read as columns**:
  `(mutate d (map 'vector #'ymd visit_date))` became
  `(tbl-col df :ymd)` / `(tbl-col df :vector)`. Forms inside `quote`,
  `function` and `lambda` are now left untouched.
- Operator and helper heads (`==`, `!=`, `mean`, `n`, ...) are matched by
  name, so the DSL also works from packages that do not `:use` cl-dplyr
  (previously `==` typed in `cl-user` was passed through as an unknown
  function).

### Added
- DSL escape `(.env expr)`: evaluate `expr` as ordinary Lisp (like R's
  `.env$x`), e.g. `(filter df (> :x (.env threshold)))`.
- `*earmuffed*` special variables are used as variables, not columns.
- `(is-na col)` / `(is.na col)` in the DSL: vectorised missing test.
