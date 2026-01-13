(defpackage #:cl-dplyr
  (:use #:cl #:cl-vctrs-lite)
  (:import-from #:cl-tibble
                #:tibble)
  (:export
   ;; Row operations
   #:slice
   #:filter
   #:arrange
   #:distinct

   ;; Column operations
   #:select
   #:rename
   #:mutate
   #:pull
   
   ;; Grouping
   #:group-by
   #:ungroup
   #:summarise
   #:summarize

   ;; Joins
   #:inner-join
   #:left-join
   #:right-join
   #:full-join
   #:semi-join
   #:anti-join
   
   ;; DSL
   #:->
   #:->>
   
   ;; NA handling
   #:is-missing-p))

(in-package #:cl-dplyr)

(defun is-missing-p (x)
  "Check if X is missing (nil or cl-vctrs-lite:*na*)."
  (or (null x) (cl-vctrs-lite:na-p x)))
