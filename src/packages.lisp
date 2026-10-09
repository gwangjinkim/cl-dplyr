(defpackage #:cl-dplyr
  (:use #:cl #:cl-vctrs-lite)
  (:nicknames #:dplyr)
  (:import-from #:cl-tibble
                #:tibble)
  (:export
   ;; Core Verbs (Dot-prefixed for zero-conflict)
   #:.slice
   #:.filter
   #:.arrange
   #:.distinct
   #:.select
   #:.rename
   #:.mutate
   #:.pull
   #:.group-by
   #:.ungroup
   #:.summarise
   #:.summarize

   ;; Joins (Dot-prefixed for zero-conflict)
   #:.inner-join
   #:.left-join
   #:.right-join
   #:.full-join
   #:.semi-join
   #:.anti-join
   
   ;; DSL
   #:->
   #:->>
   #:==
   #:%=
   #:!=
   #:>
   #:<
   #:>=
   #:<=
   #:&
   #:|\||
   #:!

   ;; Helpers (Dot-prefixed for zero-conflict)
   #:.n
   #:.n-distinct
   #:.desc
   #:.asc
   #:v-first
   #:v-last
   #:v-nth
   #:.row-number
   #:if-else
   #:case-when
   
   ;; DSL Internal Names (Matched by string in DSL)
   #:.sum
   #:.mean
   #:.min
   #:.max
   #:.count
   #:.median
   #:.var
   #:.sd
   #:.quantile
   
   ;; NA handling
   #:is-missing-p))

(in-package #:cl-dplyr)

(defun is-missing-p (x)
  "Check if X is missing (nil or cl-vctrs-lite:*na*)."
  (or (null x) (cl-vctrs-lite:na-p x)))

;; Reader macros for DSL: #c"name" (column) and #r"name" (row).
;;
;; The Lisp reader treats #c/#C and #r/#R as the same dispatch character,
;; and both are standard syntax: #C(1 2) is a complex number and #2r1010
;; is a number in radix 2. These readers therefore delegate the standard
;; forms to CL's own reader functions, so loading cl-dplyr does not change
;; how other code is read:
;;   #c followed by "(" -> standard complex reader
;;   #r with a numeric argument (#2r...) -> standard radix reader
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *standard-sharp-c*
    (get-dispatch-macro-character #\# #\c (copy-readtable nil)))
  (defparameter *standard-sharp-r*
    (get-dispatch-macro-character #\# #\r (copy-readtable nil)))

  (defun column-reader (stream subchar arg)
    (if (eql (peek-char nil stream t nil t) #\()
        (funcall *standard-sharp-c* stream subchar arg)
        (let ((name (read stream t nil t)))
          `(:col ,name))))

  (defun row-reader (stream subchar arg)
    (if arg ; numeric infix argument, e.g. the 2 in #2r1010
        (funcall *standard-sharp-r* stream subchar arg)
        (let ((name (read stream t nil t)))
          `(:row ,name)))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (set-dispatch-macro-character #\# #\c #'column-reader)
  (set-dispatch-macro-character #\# #\r #'row-reader))
