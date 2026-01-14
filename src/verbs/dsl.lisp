(in-package #:cl-dplyr)

;; 1. Vectorized Boolean Operators (missing from cl-vctrs-lite)
(defun v-and (&rest args)
  (if (null args)
      #(t)
      (reduce (lambda (a b) (cl-vctrs-lite:col-map (lambda (x y) (and x y)) a b)) args)))

(defun v-or (&rest args)
  (if (null args)
      #(nil)
      (reduce (lambda (a b) (cl-vctrs-lite:col-map (lambda (x y) (or x y)) a b)) args)))

(defun v-not (a)
  (cl-vctrs-lite:col-map #'not a))

;; 2. DSL Operator Mapping
(defparameter *dsl-operators*
  '((==  . cl-vctrs-lite:v=)
    (%=  . cl-vctrs-lite:v=)
    (/=  . cl-vctrs-lite:v=) 
    (!=  . cl-vctrs-lite:v=) 
    (>   . cl-vctrs-lite:v>)
    (<   . cl-vctrs-lite:v<)
    (>=  . cl-vctrs-lite:v>=)
    (<=  . cl-vctrs-lite:v<=)
    (+   . cl-vctrs-lite:v+)
    (-   . cl-vctrs-lite:v-)
    (*   . cl-vctrs-lite:v*)
    (/   . cl-vctrs-lite:v/)
    (&   . v-and)
    (|\||  . v-or)
    (!   . v-not)))

;; 3. DSL Expansion Core
(defun unquote-col (c)
  "Convert symbol to keyword if not already keyword."
  (cond
    ((keywordp c) c)
    ((symbolp c) (values (intern (string-upcase c) :keyword)))
    (t c)))

(defun parse-dsl (expr df-sym)
  (cond
    ((null expr) nil)
    ((or (eq expr :n) (eq expr 'n)) `(cl-tibble:tbl-nrows ,df-sym))
    ((and (consp expr) (eq (first expr) 'n)) `(cl-tibble:tbl-nrows ,df-sym))
    ((keywordp expr) `(cl-tibble:tbl-col ,df-sym ,expr))
    ((symbolp expr)
     (let ((pkg (symbol-package expr)))
       (if (or (null pkg) (eq pkg (find-package :cl-dplyr)))
           `(cl-tibble:tbl-col ,df-sym ,(unquote-col expr))
           expr)))
    ((consp expr)
     (let ((head (car expr)))
       (case head
         (:col `(cl-tibble:tbl-col ,df-sym ,(unquote-col (second expr))))
         (:row (error "Row access via #r is not supported by cl-tibble."))
         (n `(cl-tibble:tbl-nrows ,df-sym))
         (n-distinct `(length (remove-duplicates ,(parse-dsl (second expr) df-sym) :test #'equalp)))
         (t
          (cond
            ((string-equal (symbol-name head) "FIRST")
             `(v-first ,(parse-dsl (second expr) df-sym)))
            ((string-equal (symbol-name head) "LAST")
             `(v-last ,(parse-dsl (second expr) df-sym)))
            ((string-equal (symbol-name head) "NTH")
             `(v-nth ,(parse-dsl (second expr) df-sym) ,(third expr)))
            ((string-equal (symbol-name head) "ROW_NUMBER")
             `(row-number))
            ((string-equal (symbol-name head) "IF_ELSE")
             `(if-else ,(parse-dsl (second expr) df-sym) 
                       ,(parse-dsl (third expr) df-sym)
                       ,(parse-dsl (fourth expr) df-sym)))
            ((string-equal (symbol-name head) "CASE_WHEN")
             `(case-when ,@(loop for (cond val) in (cdr expr)
                                 collect `(,(parse-dsl cond df-sym) ,(parse-dsl val df-sym)))))
            (t 
             (let ((op (assoc head *dsl-operators*)))
               (if op
                   `(,(cdr op) ,@(mapcar (lambda (e) (parse-dsl e df-sym)) (rest expr)))
                   (cons head (mapcar (lambda (e) (parse-dsl e df-sym)) (rest expr)))))))))))
    (t expr)))

;; 4. DSL Verb transformation (for use inside ->)
(defun expand-dsl-form (form)
  "Expand DSL-specific verbs in a form."
  (if (consp form)
      (case (first form)
        (select (cons 'select (mapcar #'unquote-col (rest form))))
        (arrange (cons 'arrange (loop for arg in (rest form) collect
                                     (if (and (consp arg) (member (first arg) '(desc :desc)))
                                         `'(,(unquote-col (second arg)) :desc)
                                         (unquote-col arg)))))
        (filter (let ((df-sym (gensym "DF")))
                  `(filter (lambda (,df-sym) ,(parse-dsl (second form) df-sym)))))
        (mutate (let ((df-sym (gensym "DF")))
                  `(mutate ,@(loop for (col expr) on (rest form) by #'cddr
                                   append `(,(unquote-col col) (lambda (,df-sym) ,(parse-dsl expr df-sym)))))))
        (summarise (let ((df-sym (gensym "DF")))
                     `(summarise ,@(loop for (col expr) on (rest form) by #'cddr
                                         append `(,(unquote-col col) (lambda (,df-sym) ,(parse-dsl expr df-sym)))))))
        (t form))
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
