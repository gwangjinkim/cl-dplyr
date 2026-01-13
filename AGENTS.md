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

## Agent 6: DSL & Sugar
**Goal**: Add the "Lisp" feel.
- [ ] **Test**: Write tests for macros.
- [ ] Define macros for core verbs allowing unquoted column references.
- [ ] Define threading helpers if standard `->` is insufficient.
- [ ] Ensure clear error messages for DSL usage.
- [x] **Verify**: Run tests via `make test` and pass.
