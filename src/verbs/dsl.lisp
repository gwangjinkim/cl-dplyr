(in-package #:cl-dplyr)

(defun make-keyword (name)
  "Create a keyword from a symbol or string."
  (values (intern (string-upcase name) :keyword)))

(defun unquote-col (c)
  "Convert symbol to keyword if not already keyword."
  (if (symbolp c) (make-keyword c) c))

(defun map-nested (fn tree)
  (cond
    ((null tree) nil)
    ((consp tree) (cons (map-nested fn (car tree))
                        (map-nested fn (cdr tree))))
    (t (funcall fn tree))))

(defun transform-filter-expr (expr)
  "Transform (numeric) expressions like (> a 2) to lambda."
  ;; Naive implementation: assume all symbols found in expr that are not standard CL functions are column lookup
  ;; Actually, we just need to wrap it: (lambda (d) (map 'vector (lambda (a) (> a 2)) (pull d :a)))
  ;; This is complex.
  ;; Simpler approach: (lambda (d) (let ((a (pull d :a))...) ...))
  ;; But map 'vector is needed for vectorized operations.
  ;; cl-tibble/dplyr in R is vectorized.
  ;; If the user writes (> a 2), they expect vector > 2.
  ;; CL's > works on numbers.
  ;; We need a vector-map wrapper.
  `(lambda (df)
     (let* ,(loop for s in (extract-symbols expr)
                  collect `(,s (cl-dplyr:pull df ,(make-keyword s))))
       (map 'vector (lambda ,(extract-symbols expr) ,expr)
            ,@(extract-symbols expr)))))

(defun extract-symbols (expr)
  "Extract free symbols from expression. Very naive."
  (let ((syms '()))
    (labels ((scan (e)
               (cond
                 ((null e) nil)
                 ((symbolp e) 
                  (let ((pkg (symbol-package e)))
                    (unless (or (keywordp e)
                                (eq pkg (find-package :common-lisp))
                                (eq pkg (find-package :cl-dplyr)))
                      (pushnew e syms))))
                 ((consp e)
                  (scan (car e))
                  (scan (cdr e))))))
      (scan expr))
    (nreverse syms)))

(defun expand-dsl-form (form)
  "Expand DSL-specific macros like select, arrange, filter, mutate in a form."
  (if (consp form)
      (case (first form)
        (select 
         `(select ,@(mapcar #'make-keyword (rest form))))
        (arrange
         `(arrange ,@(loop for arg in (rest form) collect
                           (if (and (consp arg) 
                                    (string= (string (first arg)) "DESC"))
                               `'(,(make-keyword (second arg)) :desc)
                               (make-keyword arg)))))
        (filter
         `(filter ,(transform-filter-expr (second form))))
        (mutate
         `(mutate ,@(loop for (col expr) in (rest form)
                          append `(,(make-keyword col) ,(transform-filter-expr expr)))))
        (t form))
      form))

(defun placeholder-p (x placeholder)
  (and (symbolp x)
       (string= (symbol-name x) (symbol-name placeholder))))

(defun contains-placeholder-p (tree placeholder)
  "Check if tree contains the placeholder symbol."
  (cond
    ((placeholder-p tree placeholder) t)
    ((consp tree) (or (contains-placeholder-p (car tree) placeholder)
                      (contains-placeholder-p (cdr tree) placeholder)))
    (t nil)))

(defun replace-placeholder (tree placeholder value)
  "Replace occurrences of placeholder with value in tree."
  (cond
    ((placeholder-p tree placeholder) value)
    ((consp tree) (cons (replace-placeholder (car tree) placeholder value)
                        (replace-placeholder (cdr tree) placeholder value)))
    (t tree)))

(defmacro -> (x &rest forms)
  "Thread-first macro with DSL expansion and placeholder support (_)."
  (let ((forms (mapcar #'expand-dsl-form forms)))
    (reduce (lambda (val form)
              (if (and (listp form) (contains-placeholder-p form '_))
                  (replace-placeholder form '_ val)
                  (if (listp form)
                      `(,(first form) ,val ,@(rest form))
                      `(funcall ,form ,val))))
            forms
            :initial-value x)))

(defmacro ->> (x &rest forms)
  "Thread-last macro with DSL expansion and placeholder support (_)."
  (let ((forms (mapcar #'expand-dsl-form forms)))
    (reduce (lambda (val form)
              (if (and (listp form) (contains-placeholder-p form '_))
                  (replace-placeholder form '_ val)
                  (if (listp form)
                      `(,@form ,val)
                      `(funcall ,form ,val))))
            forms
            :initial-value x)))
