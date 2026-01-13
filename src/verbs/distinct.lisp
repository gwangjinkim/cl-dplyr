(in-package #:cl-dplyr)

(defmethod distinct ((data cl-tibble:tbl) &rest variables)
  "Select distinct/unique rows."
  (let* ((cols (if variables
                   variables
                   (coerce (cl-tibble:tbl-names data) 'list)))
         (n (cl-tibble:tbl-nrows data))
         (seen (make-hash-table :test 'equal))
         (keep-indices '()))
    
    (loop for i below n
          do (let ((row-signature 
                    (loop for col in cols
                          collect (aref (cl-tibble:tbl-col data col) i))))
               (unless (gethash row-signature seen)
                 (setf (gethash row-signature seen) t)
                 (push i keep-indices))))
    
    (cl-tibble:slice data :rows (nreverse keep-indices))))
