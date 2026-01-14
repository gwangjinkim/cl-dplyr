# cl-dplyr: A Grammar of Data Manipulation for Common Lisp

`cl-dplyr` is a port of the popular R package `dplyr` to Common Lisp. It provides a consistent set of verbs that help you solve the most common data manipulation challenges.

- `mutate()` adds new variables that are functions of existing variables
- `select()` picks variables based on their names.
- `filter()` picks cases based on their values.
- `summarise()` reduces multiple values down to a single summary.
- `arrange()` changes the ordering of the rows.

## Quick Start

`cl-dplyr` brings the elegance of the Tidyverse to Common Lisp.

### The New DSL Notation
We provide a powerful DSL that allows you to write R-like code directly in Lisp.

- **Column References**: Use keywords (`:city`) or the `#c` reader macro (`#c"city"`).
- **Vectorized Ops**: Use `==`, `+`, `*`, `&`, `|`, etc. for element-wise operations.
- **Piping**: Use `->` to chain operations.

#### Example Pipeline
```lisp
(use-package :cl-dplyr)

(-> df
    (filter (== :city "London"))
    (mutate :double_age (* :age 2))
    (arrange :age :desc)
    (summarise :mean_age (mean :double_age)))
```

### Usage Guide

| R dplyr | cl-dplyr (DSL) | cl-dplyr (Lambda) |
|---------|----------------|-------------------|
| `filter(df, x > 5)` | `(filter df (> :x 5))` | `(filter df (lambda (d) (v> (tbl-col d :x) 5)))` |
| `mutate(df, z = x + y)` | `(mutate df :z (+ :x :y))` | `(mutate df :z (lambda (d) (v+ (tbl-col d :x) (tbl-col d :y))))` |
| `select(df, x, y)` | `(select df :x :y)` | `(select df :x :y)` (Functional is same) |
| `arrange(df, desc(x))` | `(arrange df (desc :x))` | `(arrange df '(:x :desc))` |
| `df %>% filter(...)` | `(-> df (filter ...))` | `(-> df (filter ...))` |

### Reader Macros
For those who prefer explicit column/row annotation:

- **`#c"name"`**: Selects a column (e.g., `#c"age"` is equivalent to `:age` in most contexts, but explicit).
- **`#r"name"`**: *(Reserved for future row-name support)*.

```lisp
(filter df (== #c"City" "Paris"))
```

## For Common Lisp Users: Why cl-dplyr?

Common Lisp is powerful, but manipulating tabular data can be verbose. `cl-dplyr` abstracts away the loops and vector mapping.

### Tutorial: Basic Verbs

First, let's create a "tibble":

```lisp
(use-package '(#:cl-dplyr #:cl-tibble))

(defparameter *df* 
  (tibble :name #("Alice" "Bob" "Charlie" "David")
          :age #(25 30 35 40)
          :city #("New York" "London" "London" "Tokyo")))
```

#### 1. Filter rows
Keep only people from London (using vectorized `==`):

```lisp
(filter *df* (== :city "London"))
;; => Returns a tibble with Bob and Charlie
```

#### 2. Mutate (Add/Modify columns)
Add a "double-age" column:

```lisp
(mutate *df* :double-age (* :age 2))
```

#### 3. Arrange (Sort)
Sort by age descending:

```lisp
(arrange *df* '(:age :desc))
```

#### 4. Summarise
Calculate the average age:

```lisp
(summarise *df* :avg-age (mean :age))
```

**Note:** Standard CL functions like `mean` (if implemented to handle vectors) work seamlessly. If not, use `cl-vctrs-lite` equivalents or ensure your summary function takes a vector.

#### 5. Advanced Helpers (DSL)
`cl-dplyr` provides rich helpers for common idioms:

```lisp
(-> *df*
    ;; Context helpers
    (summarise :count (n)
               :unique-cities (n-distinct :city)
               :first-name (first :name)
               :last-name (last :name))
    
    ;; Logic
    (mutate :category (case_when
                        ((> :age 35) "Senior")
                        ((> :age 28) "Mid")
                        (t "Junior")))
                        
    ;; Ranking and Sorting
    (arrange (desc :age))
    (mutate :rank (row_number)))
```

Note that `first`, `last`, and `nth` are automatically mapped to vectorized versions (`v-first`) inside DSL verbs to avoid conflicts with CL's list functions. `case_when` and `if_else` are fully vectorized.


## NA Handling Standards

`cl-dplyr` follows the `cl-vctrs-lite` ecosystem standard for missing values.

- **`cl-vctrs-lite:*na*`**: The official singleton representing a missing value.
- **`NIL`**: Also treated as a missing value for user convenience.

### The `is-missing-p` Function
Use `(is-missing-p x)` to check for either `*na*` or `nil`.

### Behavior in Verbs
- **`filter`**: Rows where the predicate results in `NA` or `NIL` are dropped.
- **`arrange`**: `NA` and `NIL` values are always sorted to the **end** of the tibble.
- **`summarise`/`mutate`**: These verbs propagate `NA` values according to the logic of the functions you provide.

---
Built with ❤️ for the Common Lisp Data Science community.
