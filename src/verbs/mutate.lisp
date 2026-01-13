(in-package #:cl-dplyr)

(defmethod mutate ((data cl-tibble:tbl) &rest mutations)
  "Add or modify columns."
  ;; 1. Copy data by slicing all rows
  (let* ((n (cl-tibble:tbl-nrows data))
         (indices (loop for i below n collect i))
         (result (cl-tibble:slice data :rows indices)))
    
    (loop for (key val) on mutations by #'cddr
          do (let* ((col-name key)
                    (new-col-data 
                     (cond
                       ((functionp val) (funcall val result)) 
                       (t val))))
               (setf result (cl-tibble:bind-cols result col-name new-col-data))))
    result))
