(in-package #:cl-dplyr)

;;; Vector Accessors

(defun vector-not-string-p (x)
  (and (vectorp x) (not (stringp x))))

(defun v-first (x)
  "Return the first element of a vector or list X."
  (if (vector-not-string-p x)
      (and (> (length x) 0) (aref x 0))
      (if (listp x) (first x) x)))

(defun v-last (x)
  "Return the last element of a vector or list X."
  (if (vector-not-string-p x)
      (and (> (length x) 0) (aref x (1- (length x))))
      (if (listp x) (car (last x)) x)))

(defun v-nth (x n)
  "Return the Nth element of X (0-based)."
  (if (vector-not-string-p x)
      (and (> (length x) n) (aref x n))
      (if (listp x) (nth n x) nil)))

;;; Ranking

(defun aggregate-row-number ()
  "Return a vector of indices from 0 to N-1 for the current context.
   This is intended to be used within mutate/filter where context size is known."
  (error "row-number() must be used within a dplyr verb context."))

;;; Logic

(defun if-else (condition true-val false-val)
  "Vectorized if-else.
   CONDITION should be a vector of booleans (or something treated as boolean).
   TRUE-VAL and FALSE-VAL can be vectors or scalars.
   Returns a new vector where each element is chosen based on CONDITION."
  (let* ((n (length condition))
         (result (make-array n)))
    (dotimes (i n)
      (let ((c (aref condition i))
            (t-val (if (vector-not-string-p true-val) (aref true-val i) true-val))
            (f-val (if (vector-not-string-p false-val) (aref false-val i) false-val)))
        (setf (aref result i) (if c t-val f-val))))
    result))

(defmacro case-when (&rest clauses)
  "Vectorized multi-case conditional.
   Usage: (case-when (cond1 val1) (cond2 val2) (t default))
   Expands to nested if-else calls or a loop structure."
  (labels ((expand-clauses (clauses)
             (if (null clauses)
                 nil ; Default if no clauses match and no 't' clause
                 (let ((clause (car clauses)))
                   (destructuring-bind (cond expr) clause
                     (if (eq cond t)
                         expr
                         `(if-else ,cond ,expr ,(expand-clauses (cdr clauses)))))))))
    (expand-clauses clauses)))

;;; Aggregation Helpers
;;
;; Semantics follow R: an NA anywhere makes the result NA unless
;; :NA-RM T is given; logical T/NIL count as 1/0 (so (sum (is-na x))
;; counts missing values); MEAN, MEDIAN, VAR, SD and QUANTILE return
;; double-floats; an empty input gives 0 for SUM and NA otherwise.

(defun %agg-values (x na-rm name)
  "Return (values numbers na-seen-p) for column X: a list of reals with
logicals mapped to 1/0. Missing values are dropped when NA-RM, otherwise
reported via the second value."
  (let ((vals '()) (na-seen nil))
    (map nil (lambda (e)
               (cond ((cl-vctrs-lite:na-p e) (unless na-rm (setf na-seen t)))
                     ((eq e t) (push 1 vals))
                     ((null e) (push 0 vals))
                     ((realp e) (push e vals))
                     (t (error "~a: non-numeric value ~s" name e))))
         (if (listp x) x (coerce x 'list)))
    (values (nreverse vals) na-seen)))

(defmacro %with-agg-values ((vals x na-rm name) &body body)
  (let ((na (gensym "NA")))
    `(multiple-value-bind (,vals ,na) (%agg-values ,x ,na-rm ,name)
       (if ,na *na* (progn ,@body)))))

(defun %as-double (x) (coerce x 'double-float))

(defun aggregate-sum (x &key na-rm)
  "Sum of X. Returns 0 for empty."
  (%with-agg-values (v x na-rm "sum")
    (reduce #'+ v :initial-value 0)))

(defun aggregate-mean (x &key na-rm)
  "Arithmetic mean of X as a double-float."
  (%with-agg-values (v x na-rm "mean")
    (if v (%as-double (/ (reduce #'+ v) (length v))) *na*)))

(defun aggregate-min (x &key na-rm)
  "Minimum of X."
  (%with-agg-values (v x na-rm "min")
    (if v (reduce #'cl:min v) *na*)))

(defun aggregate-max (x &key na-rm)
  "Maximum of X."
  (%with-agg-values (v x na-rm "max")
    (if v (reduce #'cl:max v) *na*)))

(defun %quantile-sorted (sorted p)
  "Type-7 quantile (R's default) of a sorted simple-vector of reals."
  (let* ((n (length sorted))
         (h (* (1- n) p))
         (lo (floor h))
         (hi (min (1+ lo) (1- n))))
    (%as-double (+ (aref sorted lo)
                   (* (- h lo) (- (aref sorted hi) (aref sorted lo)))))))

(defun aggregate-quantile (x p &key na-rm)
  "Quantile of X at probability P (0 <= P <= 1), R type 7."
  (unless (and (realp p) (<= 0 p 1))
    (error "quantile: probability must be in [0, 1], got ~s" p))
  ;; RATIONALIZE recovers the intended decimal from a float literal:
  ;; 0.1 typed at a default (single-float) REPL is 0.100000001f0, which
  ;; would shift the result by ~5e-8; (rationalize 0.1f0) is exactly 1/10.
  (let ((p (if (floatp p) (rationalize p) p)))
    (%with-agg-values (v x na-rm "quantile")
      (if v (%quantile-sorted (sort (coerce v 'simple-vector) #'<) p) *na*))))

(defun aggregate-median (x &key na-rm)
  "Median of X as a double-float."
  (aggregate-quantile x 1/2 :na-rm na-rm))

(defun aggregate-var (x &key na-rm)
  "Sample variance (denominator n - 1). NA for fewer than two values."
  (%with-agg-values (v x na-rm "var")
    (let ((n (length v)))
      (if (< n 2)
          *na*
          (let* ((m (/ (reduce #'+ v) n))
                 (ss (reduce #'+ (mapcar (lambda (e) (expt (- e m) 2)) v))))
            (%as-double (/ ss (1- n))))))))

(defun aggregate-sd (x &key na-rm)
  "Sample standard deviation."
  (let ((var (aggregate-var x :na-rm na-rm)))
    (if (cl-vctrs-lite:na-p var) var (sqrt var))))
