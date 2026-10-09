# Changelog

All notable changes to this project are documented here. Format follows
[Keep a Changelog](https://keepachangelog.com/en/1.1.0/). See AGENTS.md
("Change Discipline") for how entries and commits are written.

## [Unreleased]

### Fixed
- **Loading cl-dplyr broke standard Lisp syntax in all later code.** Its
  global `#c`/`#r` reader macros (for `#c"col"` / `#r"row"`) replaced CL's
  `#C(1 2)` complex numbers (read as `(:COL (1 2))`) and `#2r1010` radix
  numbers, because dispatch sub-characters are case-insensitive. The
  readers now delegate `#C(` and `#<n>r` to CL's standard reader, so both
  the DSL syntax and standard syntax work (topic: reader macros).
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
- **`(mutate df :label "abc")` treated the string as a character vector**:
  on a 3-row tibble the column became `#\a #\b #\c`, on other sizes it
  errored. Strings are now recycled as scalars (topic: mutate scalars).
- **`if-else` treated an NA condition as TRUE.** It now yields NA, as in R.
- **`(.arrange df (.desc :y))` failed** with `Invalid column specifier:
  ':Y`. `.desc`/`.asc` returned `('x :desc)`, i.e. the column wrapped in a
  `quote` form, and `.arrange` only recognised `desc`, not the exported
  `.desc`. Now `.desc`/`.asc` return `(col :desc)`/`(col :asc)`, and
  `.arrange` accepts `desc`/`.desc`/`asc`/`.asc`, `(col :desc)`, and any
  other form evaluating to a spec (topic: arrange).
- **Join results had reversed column order** (`set-difference` does not
  preserve order): `left-join` returned `site, note, sex, ..., id`. Output
  is now keys, then X's other columns, then Y's, each in original order
  (topic: joins).
- **Unmatched join cells were the string `"NA"`**, which then broke type
  inference and arithmetic. They are now `cl-vctrs-lite:*na*`.
- **Joining tables with a shared non-key column failed** with
  `Duplicate column name`. Such columns now get suffixes, `.x`/`.y` by
  default.

### Added
- DSL escape `(.env expr)`: evaluate `expr` as ordinary Lisp (like R's
  `.env$x`), e.g. `(filter df (> :x (.env threshold)))`.
- `*earmuffed*` special variables are used as variables, not columns.
- `(is-na col)` / `(is.na col)` in the DSL: vectorised missing test.
- Joins: `:suffix` argument for inner/left/right/full joins, e.g.
  `(.left-join a b :by :id :suffix '("_old" "_new"))`.
- Joins without `:by` join on all common columns (natural join); an error
  is signalled if there are none. `:by` also accepts strings.
- Summary statistics in the DSL: `median`, `var`, `sd` (sample, n-1) and
  `quantile` (R's default type 7, e.g. `(quantile :y 0.9)`), also exported
  as `.median`, `.var`, `.sd`, `.quantile` (topic: aggregates).
- All aggregates accept `:na-rm t`, e.g. `(mean :x :na-rm t)`.
- Grouped `mutate` and `filter`: on a `group-by` result, expressions are
  evaluated per group, so `(-> df (group-by g) (mutate c (- x (mean x))))`
  centres within groups and `(filter (> x (mean x)))` compares with the
  group mean. Row order is preserved and the result stays grouped
  (previously both signalled "no applicable method") (topic: grouping).

### Changed
- Removed emojis from README.md (maintainer preference: no emojis in
  documentation). Meaning is kept in words where an emoji carried it
  (topic: docs style).
- `group-by` orders groups by their key values (numbers, then strings,
  then logicals, NA last), so `summarise` output order is defined.
  Previously it followed hash-table iteration order, which is
  implementation-dependent.
- Aggregates follow R's NA rules: any NA makes `sum`/`mean`/`min`/`max`/...
  NA unless `:na-rm t` (previously they signalled a type error on NA).
  Logical T/NIL count as 1/0, so `(sum (is-na :x))` counts missing values.
- `mean` returns a double-float (was an exact rational such as `305/2`,
  printed as `<obj>`). Empty input: `sum` → 0, others → NA (was NIL).
- Stopped tracking `build/` (195 files, 4.6M): the ASDF/Roswell compile
  cache that `make test` writes via `XDG_CACHE_HOME=$(PWD)/build`. It held
  compiled fasls, including Quicklisp internals and absolute local paths.
  `build/` is now in `.gitignore`; local files are untouched. Old copies
  remain in history (topic: repo hygiene).

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
- `join-test`: replaced `(is (handler-case ...))`, which FiveAM cannot
  macroexpand, with `(signals cl-tibble:unknown-column-error ...)`.
- New `tests/test-joins.lisp` (6 tests). Suite: 95/95 pass.
- New `tests/test-aggregates.lisp` (6 tests; median/var/sd/quantile values
  checked against R). Suite: 114/114 pass.
- New `tests/test-mutate.lisp` (3 tests). Suite: 120/120 pass.
- New `tests/test-grouped.lisp` (5 tests). Suite: 128/128 pass.
- New `tests/test-reader.lisp` (3 tests). Suite: 134/134 pass.
