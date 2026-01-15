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

(defun row-number ()
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

(defun aggregate-sum (x)
  "Sum of elements in X. Returns 0 for empty."
  (if (or (null x) (= (length x) 0))
      0
      (reduce #'+ x)))

(defun aggregate-mean (x)
  "Mean of elements in X."
  (if (or (null x) (= (length x) 0))
      nil ;; Or 0? R returns NA. nil is our NA.
      (/ (reduce #'+ x) (length x))))

(defun aggregate-min (x)
  "Min of elements in X."
  (if (or (null x) (= (length x) 0))
      nil
      (reduce #'cl:min x)))

(defun aggregate-max (x)
  "Max of elements in X."
  (if (or (null x) (= (length x) 0))
      nil
      (reduce #'cl:max x)))
