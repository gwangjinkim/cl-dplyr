# cl-dplyr Specification

## 1. Overview
`cl-dplyr` is a Common Lisp data manipulation library inspired by R's `dplyr`. It provides a grammar of data manipulation, offering a consistent set of verbs that help you solve the most common data manipulation challenges. It is built on top of `cl-tibble` and `cl-vctrs-lite`.

## 2. dependencies
- **cl-tibble**: Provides the dataframe structure (`tibble`) and fundamental column access/binding operations.
- **cl-vctrs-lite**: Provides the underlying vector operations, recycling rules, and type stability.
- **serapeum/alexandria**: General utilities.

## 3. Architecture

The library will consist of two layers:
1.  **Functional Layer**: Functions that perform operations. Arguments for column names must be explicitly quoted (keywords or strings) or passed as accessor functions.
2.  **DSL Layer (Sugar)**: Macros that allow for non-standard evaluation (NSE)-like behavior, allowing users to use unquoted column names and threading expressions.

### 3.1 Data Structures
- **Tibble**: The standard input/output for most verbs.
- **Grouped Tibble**: A subclass or wrapper around a standard tibble that includes grouping metadata (indices of groups).

## 4. Core Verbs (Functional Layer)

All functions take a `tibble` as their first argument and return a new `tibble`.

### 4.1 Row Operations
- **`slice(df, indices)`**: specific rows by index.
- **`filter(df, predicate)`**: Subset rows using a predicate function (or expression in DSL).
- **`arrange(df, &rest order-specs)`**: Order rows by variables.
- **`distinct(df, &rest variables)`**: Keep distinct rows.

### 4.2 Column Operations
- **`select(df, &rest selection)`**: Select, rename, and reorder columns.
- **`rename(df, &rest renaming)`**: Rename specific columns.
- **`mutate(df, &rest mutations)`**: Add or modify columns. Needs to handle:
    - Vector recycling (using `cl-vctrs-lite`).
    - Referring to specialized columns just created within the same `mutate` call.
- **`pull(df, var)`**: Extract a single column as a vector.

### 4.3 Grouping and Aggregation
- **`group_by(df, &rest variables)`**: Converts a tibble into a grouped tibble.
- **`ungroup(df)`**: Removes grouping metadata.
- **`summarise(df, &rest summaries)`**: Reduces groups to single values.
    - If grouped, returns one row per group.
    - If ungrouped, returns a 1-row independent tibble.

### 4.4 Joins
- **`inner_join(x, y, by)`**
- **`left_join(x, y, by)`**
- **`right_join(x, y, by)`**
- **`full_join(x, y, by)`**
- **`semi_join(x, y, by)`**
- **`anti_join(x, y, by)`**

## 5. DSL / Syntactic Sugar
The "Sugar" layer will provide macros to make the syntax "more Common Lisp like" while retaining the expressiveness of R's tidyverse.

- **`with-tibble`**: Macro to expose column names as variables within a scope (similar to `with-slots` but for columns).
- **Threading**: Use `arrow-macros` (or custom `%>`) to pipe data through verbs.
- **Verb Macros**: `select*`, `filter*`, etc., that allow unquoted symbols.

Example:
```lisp
(-> df
  (filter* (> :speed 100))
  (mutate* :density (/ :mass :volume))
  (select* :species :density))
```

## 6. Implementation roadmap
1.  **Foundation**: Setup package, `slice`, `select` (wrappers around `cl-tibble`).
2.  **Transformation**: `mutate` and `transmute` (requires topological sort or sequential evaluation context).
3.  **Filtering & Sorting**: `filter` and `arrange`.
4.  **Grouping**: `group_by` and `summarise` (the most complex logic).
5.  **Joins**: Relational algebra.
