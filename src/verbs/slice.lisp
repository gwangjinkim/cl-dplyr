(in-package #:cl-dplyr)

(defmethod slice ((data tibble) &rest indices)
  "Slice rows by index."
  ;; Assuming indices are 0-based integers
  ;; In dplyr slice takes generic arguments, often integers.
  (let ((idx (if (and (length= 1 indices)
                      (typep (first indices) 'sequence))
                 (first indices)
                 indices)))
    (cl-tibble:slice-rows data idx)))
