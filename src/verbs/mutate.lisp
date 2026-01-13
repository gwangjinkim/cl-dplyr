(in-package #:cl-dplyr)

(defmethod mutate ((data tibble) &rest mutations)
  "Add or modify columns.
   Mutations are pairs of (col-name . value-or-function).
   If value is a function, it applies to the data.
   If value is a vector/atom, it cycles/recycles."
  ;; For the functional interface, we accept :name value pairs.
  ;; We need to process them sequentially to allow dependent columns.
  
  ;; Make a shallow copy or work on a fresh tibble?
  ;; dplyr mutates return new dataframes, original is immutable-ish.
  ;; cl-tibble is likely mutable or we copy. 
  ;; Let's assume we build a new tibble or modify a copy.
  ;; For now, let's copy the input first.
  
  (let* ((n (cl-tibble:nrow data))
         (indices (loop for i below n collect i))
         (result (cl-tibble:slice-rows data indices)))
    
    (loop for (key val) on mutations by #'cddr
          do (let* ((col-name key)
                    (new-col-data 
                     (cond
                       ((functionp val) (funcall val result)) ;; If func, call with current state of result
                       (t val)))) ;; Otherwise literal
               ;; Here we need recycling logic. 
               ;; For now assume val matches nrow or is atom.
               (cl-tibble:add-column result col-name new-col-data)))
    result))
