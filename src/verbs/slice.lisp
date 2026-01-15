(in-package #:cl-dplyr)

(defmethod %slice ((data cl-tibble:tbl) &rest indices)
  "Slice rows by index."
  ;; Assuming indices are 0-based integers
  ;; In dplyr slice takes generic arguments, often integers.
  (let ((idx (if (and (= (length indices) 1)
                      (typep (first indices) 'sequence))
                 (first indices)
                 indices)))
    (cl-tibble:slice data :rows idx)))

(defun .slice (data &rest indices)
  (apply #'%slice data indices))
