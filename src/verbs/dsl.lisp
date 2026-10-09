(in-package #:cl-dplyr)

;; 1. Vectorized Boolean Operators
;; V-AND, V-OR, V-NOT and V!= come from cl-vctrs-lite (NA-aware Kleene
;; logic). They used to be defined here with two-argument COL-MAP calls,
;; which COL-MAP does not accept, so & and | always signalled an error.

;; 2. DSL Operator Mapping. Heads are matched by SYMBOL-NAME so the DSL
;; works from any package, not only from one that uses CL-DPLYR.
(defparameter *dsl-operators*
  '(("=="  . cl-vctrs-lite:v=)
    ("%="  . cl-vctrs-lite:v=)
    ("/="  . cl-vctrs-lite:v!=)
    ("!="  . cl-vctrs-lite:v!=)
    (">"   . cl-vctrs-lite:v>)
    ("<"   . cl-vctrs-lite:v<)
    (">="  . cl-vctrs-lite:v>=)
    ("<="  . cl-vctrs-lite:v<=)
    ("+"   . cl-vctrs-lite:v+)
    ("-"   . cl-vctrs-lite:v-)
    ("*"   . cl-vctrs-lite:v*)
    ("/"   . cl-vctrs-lite:v/)
    ("&"   . cl-vctrs-lite:v-and)
    ("|"   . cl-vctrs-lite:v-or)
    ("!"   . cl-vctrs-lite:v-not)))

(defun dsl-operator (head)
  (and (symbolp head)
       (cdr (assoc (symbol-name head) *dsl-operators* :test #'string=))))

;; Aggregates: first argument is a column expression, any further
;; arguments (e.g. :na-rm t, or a probability for QUANTILE) are passed
;; through unchanged.
(defparameter *dsl-aggregates*
  '(("SUM" . aggregate-sum) ("MEAN" . aggregate-mean)
    ("MIN" . aggregate-min) ("MAX" . aggregate-max)
    ("MEDIAN" . aggregate-median) ("VAR" . aggregate-var)
    ("SD" . aggregate-sd) ("QUANTILE" . aggregate-quantile)))

(defun dsl-name (sym)
  "Symbol name without a leading dot, with _ read as -."
  (let ((name (substitute #\- #\_ (symbol-name sym))))
    (if (and (> (length name) 1) (char= (char name 0) #\.))
        (subseq name 1)
        name)))

;; 3. DSL Expansion Core
(defun unquote-col (c)
  "Convert symbol to keyword if not already keyword."
  (cond
    ((keywordp c) c)
    ((symbolp c) (values (intern (string-upcase c) :keyword)))
    (t c)))

(defun dsl-plain-symbol-p (sym)
  "Symbols the DSL leaves alone instead of reading as column names:
T, NIL and other constants, and *EARMUFFED* special variables."
  (and (symbolp sym)
       (not (keywordp sym))
       (or (constantp sym)
           (let ((name (symbol-name sym)))
             (and (> (length name) 2)
                  (char= (char name 0) #\*)
                  (char= (char name (1- (length name))) #\*))))))

(defun parse-dsl (expr df-sym)
  "Rewrite a DSL expression into ordinary Lisp evaluated against DF-SYM.
Keywords and plain symbols name columns. Left untouched: T, NIL and other
constants, *specials*, operator heads, and anything inside QUOTE,
FUNCTION, LAMBDA or (.env ...)."
  (cond
    ((null expr) nil)
    ((or (eq expr :n) (eq expr 'n)) `(cl-tibble:tbl-nrows ,df-sym))
    ((keywordp expr) `(cl-tibble:tbl-col ,df-sym ,expr))
    ((dsl-plain-symbol-p expr) expr)
    ((symbolp expr)
     `(cl-tibble:tbl-col ,df-sym ,(unquote-col expr)))
    ((consp expr)
     (let ((head (car expr)))
       (cond
         ((eq head :col) `(cl-tibble:tbl-col ,df-sym ,(unquote-col (second expr))))
         ((eq head :row) (error "Row access via #r is not supported by cl-tibble."))
         ((member head '(quote function lambda)) expr)
         ((not (symbolp head))
          (cons (parse-dsl head df-sym) (mapcar (lambda (e) (parse-dsl e df-sym)) (rest expr))))
         (t
          (let* ((name (dsl-name head))
                 (agg (cdr (assoc name *dsl-aggregates* :test #'string=))))
            (cond
              ((string= name "ENV") (second expr))
              (agg `(,agg ,(parse-dsl (second expr) df-sym) ,@(cddr expr)))
              ((member name '("N" "COUNT") :test #'string=)
               `(cl-tibble:tbl-nrows ,df-sym))
              ((member name '("IS-NA" "IS.NA") :test #'string=)
               `(cl-vctrs-lite:v-na-p ,(parse-dsl (second expr) df-sym)))
              ((string= name "FIRST")
               `(v-first ,(parse-dsl (second expr) df-sym)))
              ((string= name "LAST")
               `(v-last ,(parse-dsl (second expr) df-sym)))
              ((string= name "NTH")
               `(v-nth ,(parse-dsl (second expr) df-sym) ,(third expr)))
              ((string= name "ROW-NUMBER")
               `(alexandria:iota (cl-tibble:tbl-nrows ,df-sym) :start 1))
              ((string= name "IF-ELSE")
               `(if-else ,(parse-dsl (second expr) df-sym)
                         ,(parse-dsl (third expr) df-sym)
                         ,(parse-dsl (fourth expr) df-sym)))
              ((string= name "CASE-WHEN")
               `(case-when ,@(loop for (cond val) in (cdr expr)
                                   collect `(,(parse-dsl cond df-sym) ,(parse-dsl val df-sym)))))
              ((string= name "DESC")
               `(.desc ,(parse-dsl (second expr) df-sym)))
              ((string= name "ASC")
               `(.asc ,(parse-dsl (second expr) df-sym)))
              ((string= name "N-DISTINCT")
               `(.n-distinct ,(parse-dsl (second expr) df-sym)))
              (t
               (let ((op (dsl-operator head)))
                 (if op
                     `(,op ,@(mapcar (lambda (e) (parse-dsl e df-sym)) (rest expr)))
                     (cons head (mapcar (lambda (e) (parse-dsl e df-sym)) (rest expr))))))))))))
    (t expr)))

;; 4. DSL Verb transformation (for use inside ->)
(defun expand-dsl-form (form)
  "Expand DSL-specific verbs in a form."
  (if (consp form)
      (let ((name (symbol-name (first form))))
        (cond
          ((or (string-equal name "SELECT") (string-equal name ".SELECT"))
           (cons '.select (mapcar #'unquote-col (rest form))))
          ((or (string-equal name "RENAME") (string-equal name ".RENAME"))
           (cons '.rename (mapcar #'unquote-col (rest form))))
          ((or (string-equal name "ARRANGE") (string-equal name ".ARRANGE"))
           (cons '.arrange (rest form)))
          ((or (string-equal name "FILTER") (string-equal name ".FILTER"))
           (let ((df-sym (gensym "DF")))
             `(.filter (lambda (,df-sym) ,(parse-dsl (second form) df-sym)))))
          ((or (string-equal name "MUTATE") (string-equal name ".MUTATE"))
           (let ((df-sym (gensym "DF")))
             `(.mutate ,@(loop for (col expr) on (rest form) by #'cddr
                               append `(,(unquote-col col) (lambda (,df-sym) ,(parse-dsl expr df-sym)))))))
          ((or (string-equal name "SUMMARISE") (string-equal name ".SUMMARISE")
               (string-equal name "SUMMARIZE") (string-equal name ".SUMMARIZE"))
           (let ((df-sym (gensym "DF")))
             `(.summarise ,@(loop for (col expr) on (rest form) by #'cddr
                                  append `(,(unquote-col col) (lambda (,df-sym) ,(parse-dsl expr df-sym)))))))
          ((or (string-equal name "SLICE") (string-equal name ".SLICE"))
           (cons '.slice (rest form)))
          ((or (string-equal name "DISTINCT") (string-equal name ".DISTINCT"))
           (cons '.distinct (mapcar #'unquote-col (rest form))))
          ((or (string-equal name "GROUP_BY") (string-equal name ".GROUP_BY")
               (string-equal name "GROUP-BY") (string-equal name ".GROUP-BY"))
           (cons '.group-by (mapcar #'unquote-col (rest form))))
          ((or (string-equal name "UNGROUP") (string-equal name ".UNGROUP"))
           (cons '.ungroup (rest form)))
          ((or (string-equal name "PULL") (string-equal name ".PULL"))
           (cons '.pull (rest form)))
          ((or (string-equal name "INNER_JOIN") (string-equal name "INNER-JOIN") (string-equal name ".INNER-JOIN"))
           (cons '.inner-join (rest form)))
          ((or (string-equal name "LEFT_JOIN") (string-equal name "LEFT-JOIN") (string-equal name ".LEFT-JOIN"))
           (cons '.left-join (rest form)))
          ((or (string-equal name "RIGHT_JOIN") (string-equal name "RIGHT-JOIN") (string-equal name ".RIGHT-JOIN"))
           (cons '.right-join (rest form)))
          ((or (string-equal name "FULL_JOIN") (string-equal name "FULL-JOIN") (string-equal name ".FULL-JOIN"))
           (cons '.full-join (rest form)))
          ((or (string-equal name "SEMI_JOIN") (string-equal name "SEMI-JOIN") (string-equal name ".SEMI-JOIN"))
           (cons '.semi-join (rest form)))
          ((or (string-equal name "ANTI_JOIN") (string-equal name "ANTI-JOIN") (string-equal name ".ANTI-JOIN"))
           (cons '.anti-join (rest form)))
          (t form)))
      form))

;; 5. Threading Macros
(defun placeholder-p (x)
  (and (symbolp x) (string= (symbol-name x) "_")))

(defun contains-placeholder-p (tree)
  (cond
    ((placeholder-p tree) t)
    ((consp tree) (or (contains-placeholder-p (car tree))
                      (contains-placeholder-p (cdr tree))))
    (t nil)))

(defun replace-placeholder (tree value)
  (cond
    ((placeholder-p tree) value)
    ((consp tree) (cons (replace-placeholder (car tree) value)
                        (replace-placeholder (cdr tree) value)))
    (t tree)))

(defmacro -> (x &rest forms)
  (reduce (lambda (val form)
            (let ((expanded (expand-dsl-form form)))
              (if (contains-placeholder-p expanded)
                  (replace-placeholder expanded val)
                  (if (listp expanded)
                      `(,(first expanded) ,val ,@(rest expanded))
                      `(,expanded ,val)))))
          forms
          :initial-value x))

(defmacro ->> (x &rest forms)
  (reduce (lambda (val form)
            (let ((expanded (expand-dsl-form form)))
              (if (contains-placeholder-p expanded)
                  (replace-placeholder expanded val)
                  (if (listp expanded)
                      `(,@expanded ,val)
                      `(,expanded ,val)))))
          forms
          :initial-value x))
