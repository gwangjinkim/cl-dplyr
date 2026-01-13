# cl-dplyr: A Grammar of Data Manipulation for Common Lisp

`cl-dplyr` is a port of the popular R package `dplyr` to Common Lisp. It provides a consistent set of verbs that help you solve the most common data manipulation challenges.

- `mutate()` adds new variables that are functions of existing variables
- `select()` picks variables based on their names.
- `filter()` picks cases based on their values.
- `summarise()` reduces multiple values down to a single summary.
- `arrange()` changes the ordering of the rows.

## For R Tidyverse Users: Quick Start

If you are coming from R, `cl-dplyr` will feel very familiar. We use the same verb names and logic.

| R dplyr | cl-dplyr |
|---------|----------|
| `df %>% filter(x > 5)` | `(-> df (filter (lambda (d) (vec-p> (tbl-col d :x) 5))))` |
| `df %>% mutate(z = x + y)` | `(mutate df :z (lambda (d) (vec-p+ (tbl-col d :x) (tbl-col d :y))))` |
| `df %>% select(x, y)` | `(select df :x :y)` |
| `df %>% arrange(desc(x))` | `(arrange df '(:x :desc))` |
| `df %>% group_by(g) %>% summarise(m = mean(x))` | `(-> df (group-by :g) (summarise :m (lambda (d) (mean (tbl-col d :x)))))` |

**Key Differences:**
- **Piping:** We use `->` or `->>` from the `cl-dplyr` package (re-exported from `arrow-macros` or similar logic).
- **Lambdas:** Instead of unquoted expressions, we often use `lambda` for predicates and mutations to stay Lisp-idiomatic and performant.
- **Keywords:** Column names are typically keywords (e.g., `:column-name`).

## For Common Lisp Users: Why cl-dplyr?

Common Lisp is powerful, but manipulating tabular data (like CSVs or database exports) can often involve a lot of boilerplate `loop` or `map` calls. `cl-dplyr` provides a high-level DSL for these operations, making your code more readable and expressive.

### The "Grammar" Concept
The core idea is that data manipulation consists of a few basic "verbs". By combining these verbs, you can express complex data pipelines clearly.

### Tutorial: Basic Verbs

First, let's create a "tibble" (our data frame structure):

```lisp
(use-package '(#:cl-dplyr #:cl-tibble))

(defparameter *df* 
  (tibble :name #("Alice" "Bob" "Charlie" "David")
          :age #(25 30 35 40)
          :city #("New York" "London" "London" "Tokyo")))
```

#### 1. Filter rows
Keep only people from London:

```lisp
(filter *df* (lambda (d) (vec-p= (tbl-col d :city) "London")))
;; => Returns a tibble with Bob and Charlie
```

#### 2. Mutate (Add/Modify columns)
Add a "double-age" column:

```lisp
(mutate *df* :double-age (lambda (d) (vec-p* (tbl-col d :age) 2)))
```

#### 3. Arrange (Sort)
Sort by age descending:

```lisp
(arrange *df* '(:age :desc))
```

#### 4. Summarise
Calculate the average age:

```lisp
(summarise *df* :avg-age (lambda (d) (mean (tbl-col d :age))))
```

### Pipelines with `->`
You can chain operations beautifully:

```lisp
(-> *df*
    (filter (lambda (d) (vec-p/= (tbl-col d :city) "Tokyo")))
    (group-by :city)
    (summarise :mean-age (lambda (d) (mean (tbl-col d :age))))
    (arrange :mean-age))
```

## NA Handling Standards

`cl-dplyr` follows the `cl-vctrs-lite` ecosystem standard for missing values.

- **`cl-vctrs-lite:*na*`**: The official singleton representing a missing value.
- **`NIL`**: Also treated as a missing value for user convenience.

### The `is-missing-p` Function
Use `(is-missing-p x)` to check for either `*na*` or `nil`.

```lisp
(is-missing-p cl-vctrs-lite:*na*) ;; => T
(is-missing-p nil)               ;; => T
(is-missing-p 42)                ;; => NIL
```

### Behavior in Verbs
- **`filter`**: Rows where the predicate results in `NA` or `NIL` are dropped (matching R's behavior).
- **`arrange`**: `NA` and `NIL` values are always sorted to the **end** of the tibble, regardless of whether you sort ascending or descending.
- **`summarise`/`mutate`**: These verbs propagate `NA` values according to the logic of the functions you provide (e.g., using `vec-p+` from `cl-vctrs-lite` will typically propagate `NA`).

---
Built with ❤️ for the Common Lisp Data Science community.
