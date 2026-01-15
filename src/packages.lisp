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
   
   ;; NA handling
   #:is-missing-p))

(in-package #:cl-dplyr)

(defun is-missing-p (x)
  "Check if X is missing (nil or cl-vctrs-lite:*na*)."
  (or (null x) (cl-vctrs-lite:na-p x)))

;; Reader macros for DSL
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun column-reader (stream char subchar)
    (declare (ignore char subchar))
    (let ((name (read stream t nil t)))
      `(:col ,name)))

  (defun row-reader (stream char subchar)
    (declare (ignore char subchar))
    (let ((name (read stream t nil t)))
      `(:row ,name))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (set-dispatch-macro-character #\# #\c #'column-reader)
  (set-dispatch-macro-character #\# #\r #'row-reader))
