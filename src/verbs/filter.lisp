(in-package #:cl-dplyr)

(defmethod filter ((data tibble) predicate)
  "Keep rows matching a predicate.
   PREDICATE is a function that takes the tibble and returns a boolean sequence
   (or a sequence of 0/1, or indices).
   
   Usage: (filter df (lambda (d) (vctrs:> (pull d :x) 5)))"
  
  (let* ((mask (funcall predicate data))
         (n (cl-tibble:nrow data))
         (indices 
          (loop for i below n
                for val = (elt mask i)
                when (and val (not (eq val :null))) ;; treating nil/:null as false
                collect i)))
    (cl-tibble:slice-rows data indices)))
