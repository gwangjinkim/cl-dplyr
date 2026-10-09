# cl-dplyr Implementation Agents

## Agent 1: Project Skeleton & Basic Manipulation
**Goal**: Initialize project and implement row/column subsetting.
- [ ] **Test**: Create/Update test suite (run with `ros run ...` or `make test`).
- [ ] Initialize `cl-dplyr` system with generic `cl-tibble` and `cl-vctrs-lite` dependencies.
- [ ] Implement `slice` (wrapper around `cl-tibble` row access).
- [ ] Implement `select` (wrapper around `cl-tibble` column subsetting).
- [ ] Implement `rename` (renaming columns).
- [ ] Implement `pull` (extract column vector).
- [x] **Verify**: Run tests via `make test` and pass.

## Agent 2: Transformation & Ordering
**Goal**: Implement mutation and sorting logic.
- [ ] **Test**: Write tests for `arrange` and `mutate`.
- [ ] Implement `arrange`: Sort rows based on one or more columns (supporting ascending/descending).
- [ ] Implement `mutate`:
    - Allow adding new columns based on functions of existing ones.
    - Support recycling rules via `cl-vctrs-lite`.
    - Support sequential updates (col B depends on new col A).
- [x] **Verify**: Run tests via `make test` and pass.

## Agent 3: Filtering & Logic
**Goal**: Implement row filtering.
- [ ] **Test**: Write tests for `filter` and `distinct`.
- [ ] Implement `filter`: Keep rows matching a predicate.
- [ ] Implement `distinct`: unique rows.
- [x] **Verify**: Run tests via `make test` and pass.

## Agent 4: Grouping & Aggregation (Complex)
**Goal**: Implement the split-apply-combine strategy.
- [ ] **Test**: Write tests for `group_by`, `ungroup`, `summarise`.
- [ ] Define `grouped-tibble` class/structure (stores data + group indices).
- [ ] Implement `group_by`: Calculate groups and return `grouped-tibble`.
- [ ] Implement `ungroup`.
- [ ] Implement `summarise`:
    - For regular tibble: collapse to single row.
    - For grouped tibble: collapse to one row per group.
- [x] **Verify**: Run tests via `make test` and pass.

## Agent 5: Joins
**Goal**: Implement relational joins.
- [ ] **Test**: Write tests for all join types.
- [ ] Implement `inner_join`, `left_join`, `right_join`, `full_join`.
- [ ] Implement `semi_join`, `anti_join`.
- [x] **Verify**: Run tests via `make test` and pass.

## Agent 6: DSL & Vectorized Notation
**Goal**: Implement the high-level DSL.
- [ ] **Test**: Write tests for DSL expressions in `filter`, `mutate`, etc.
- [ ] Implement reader macros `#c` and `#r` for explicit column/row naming.
- [ ] Implement DSL expansion logic:
    - Recognize unquoted symbols/keywords as column names.
    - Transform operators like `==`, `+`, etc. into vectorized `cl-vctrs-lite` calls.
- [ ] Refactor `filter`, `mutate`, `summarise` to be macros that handle both lambdas and DSL expressions.
- [ ] Update documentation and examples.
- [x] **Verify**: Run tests via `make test` and pass.

## Agent 7: Advanced Helpers & Logic
**Goal**: Implement missing `dplyr` helpers and vectorized logic.
- [ ] **Test**: Write tests for `if_else`, `case_when`, and vector accessors.
- [x] **Implemented**: `n`, `n-distinct`, `desc`, `asc`.
- [x] **Verified**: Run tests via `make test` and pass.
- [x] **Implemented**: `first`, `last`, `nth`, `row_number`.
- [x] **Implemented**: `if_else`, `case_when`.
- [x] **Implemented**: `sum`, `mean`, `min`, `max` (Aggregation).

## Change Discipline (git + documentation)

Every change to this repository must be reconstructible later: what changed,
why, and what it means for users. Agents and humans follow these rules.

1. **Branch, never commit straight to `main`.** Use a topic branch such as
   `fix/<topic>` or `feat/<topic>` (cross-package work in the tidystat effort
   uses `fix/tidystat-phase0`, `feat/tidystat-phase1`, ...).
2. **One logical change per commit.** Test-suite repairs, bug fixes, new
   features and documentation go in separate commits, so each can be
   reviewed, bisected or reverted on its own.
3. **Commit messages explain the why.** Format:
   ```
   <area>: <imperative summary, <= 72 chars>

   Problem:  what was wrong / missing, with the observable symptom.
   Cause:    the root cause (file:function), if it is a fix.
   Change:   what this commit does.
   Impact:   behaviour change for users; mark BREAKING if any.
   Tests:    which tests were added/changed, and the suite result.
   ```
4. **Every commit updates `CHANGELOG.md`** under `## [Unreleased]`, in the
   sections *Fixed*, *Added*, *Changed*, *Breaking*, *Tests*. Each entry
   says what users will notice and names the commit topic.
5. **Run the full test suite before committing** (`make test`, or from the
   umbrella repo `../cl-tidystat/scripts/run-tests.sh <this-package>`) and put
   the result in the commit message. Never commit with a red suite unless
   the commit message says so and why.
6. **Cross-package changes** (e.g. an API in `cl-vctrs-lite` used by
   `cl-dplyr`) are recorded in both repos' changelogs and in
   `../cl-tidystat/docs/CHANGES-phase*.md`, which links the commits.
7. **Push only after review**; do not rewrite history that has been pushed.
