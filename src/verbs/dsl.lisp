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

(defmacro chain (initial-arg &body forms)
  "Thread-first macro with DSL expansion."
  (let ((forms (loop for form in forms collect
                     (if (consp form)
                         (case (first form)
                           (select 
                            ;; (select a b) -> (select :a :b)
                            `(select ,@(mapcar #'make-keyword (rest form))))
                            (arrange
                             ;; (arrange (desc a)) -> (arrange '(:a :desc)) ? or (:a :desc)
                             ;; (arrange a) -> (arrange :a)
                             `(arrange ,@(loop for arg in (rest form) collect
                                               (if (and (consp arg) 
                                                        (string= (string (first arg)) "DESC"))
                                                   `'(,(make-keyword (second arg)) :desc)
                                                   (make-keyword arg)))))
                           (filter
                            ;; (filter (> a 1)) -> (filter (lambda (d) ...))
                            `(filter ,(transform-filter-expr (second form))))
                           (mutate
                            ;; (mutate (c (+ a b)))
                            `(mutate ,@(loop for (col expr) in (rest form)
                                             append `(,(make-keyword col) ,(transform-filter-expr expr)))))
                           (t form))
                         form))))
    `(progn
       (let* ((res ,initial-arg)
              ,@(loop for form in forms collect
                      `(res ,(if (consp form)
                                 `(,(first form) res ,@(rest form))
                                 `(funcall ,form res)))))
         res))))
