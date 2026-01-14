# Project Progress & Stations

This document reflects the journey ("stations") of building `cl-dplyr`, transitioning from a basic Lisp wrapper around `cl-tibble` to a full-fledged, R-like data manipulation DSL.

## Station 1: The Foundation
**Goal:** Establish the grid—basic row and column manipulations.
- [x] **Project Skeleton**: ASDF system setup.
- [x] **Core Dependencies**: Integration with `cl-tibble` and `cl-vctrs-lite`.
- [x] **Basic Verbs**:
    - `slice`: Row subsetting by index.
    - `select`: Column subsetting by name.
    - `rename`: Renaming columns.
    - `pull`: Extracting vectors.

## Station 2: Transformation & Order
**Goal:** Modify content and structure.
- [x] **Mutate**: Adding and modifying columns.
    - Initial functional implementation using lambdas.
- [x] **Arrange**: Sorting rows.
    - Implemented `order-by` logic.
    - Stable sort support with multiple columns and directions (`:asc`/`:desc`).

## Station 3: Logic & Filtering
**Goal:** Selecting data based on content.
- [x] **Filter**: Row subsetting via predicates.
- [x] **Distinct**: Unique row identification.

## Station 4: The Grouping Engine
**Goal:** Split-Apply-Combine.
- [x] **Group By**: Creating `grouped-tibble` structures with index-based groups.
- [x] **Ungroup**: Flattening back to regular tibbles.
- [x] **Summarise**: Aggregation logic.
    - Support for both ungrouped (single row) and grouped (N rows) aggregation.

## Station 5: Relational Algebra (Joins)
**Goal:** Combining datasets.
- [x] **Mutating Joins**: `inner_join`, `left_join`, `right_join`, `full_join`.
- [x] **Filtering Joins**: `semi_join`, `anti_join`.

## Station 6: Ecosystem Unification (NA Handling)
**Goal:** Harmonizing with `cl-vctrs-lite` standards.
- [x] **NA Standards**: Adopting `cl-vctrs-lite:*na*` alongside `nil`.
- [x] **Helper**: Implemented `is-missing-p`.
- [x] **Refactoring**:
    - `filter` now drops NA values.
    - `arrange` sorts NA values to the end.

## Station 7: The DSL (Vectorized Notation)
**Goal:** Bridging Lisp and R syntax.
- [x] **Reader Macros**: `#c"col"` for explicit column selection.
- [x] **Vectorized Operators**: `==`, `!=`, `>`, `<`, `+`, `*` etc. automatically map to `cl-vctrs-lite` vectorized functions.
- [x] **Verb Macros**: Refactoring core verbs (`filter`, `mutate`, etc.) to macros that:
    - Accept unquoted column names (keywords).
    - Parse standard Lisp expressions (e.g., `(> :age 25)`) into efficient lambda functions.
- [x] **Piping**: Implementation of `->` (thread-first) with DSL expansion support.

 ## Station 8: Advanced Helpers & DSL Refinement
 **Goal**: Completing the verb vocabulary.
 - [x] **Context Helpers**: `n` (count), `n-distinct`.
 - [x] **Order Helpers**: `desc`, `asc`.
 - [x] **Arrange Syntax**: Support for `(arrange df (desc :col))`.
 - [ ] **Vector Accessors**: `first`, `last`, `nth` (Lisp-safe versions).
 - [ ] **Ranking**: `row_number`.
 - [ ] **Logic**: `if_else`, `case_when` (vectorized).

---

**Current State**: The library is feature-complete for the core 80% of `dplyr` functionality, explicitly following the "Tidyverse in Lisp" philosophy. DSL now supports `n()` and rich `arrange` syntax.
