(in-package #:cl-dplyr)

(defmethod distinct ((data tibble) &rest variables)
  "Select distinct/unique rows.
   VARIABLES are column names to consider for uniqueness. 
   If VARIABLES is nil, use all columns."
  
  (let* ((cols (if variables
                   variables
                   (cl-tibble:column-names data)))
         (n (cl-tibble:nrow data))
         (seen (make-hash-table :test 'equal))
         (keep-indices '()))
    
    (loop for i below n
          do (let ((row-signature 
                    (loop for col in cols
                          collect (aref (cl-tibble:column data col) i))))
               (unless (gethash row-signature seen)
                 (setf (gethash row-signature seen) t)
                 (push i keep-indices))))
    
    (cl-tibble:slice-rows data (nreverse keep-indices))))
