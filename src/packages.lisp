(defpackage #:cl-dplyr
  (:use #:cl)
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
   #:anti-join))
