(in-package #:cl-dplyr)

(defun order-by (df vars)
  "Helper to compute sort permutation indices.
   VARS is a list of (column-name &optional direction).
   Direction is :asc (default) or :desc."
  (let* ((n (cl-tibble:nrow df))
         (indices (loop for i below n collect i)))
    (dolist (spec (reverse vars)) ;; Stable sort from last key to first
      (let* ((col-name (if (consp spec) (first spec) spec))
             (direction (if (consp spec) (second spec) :asc))
             (col-data (cl-tibble:column df col-name))
             (predicate (if (eq direction :desc) #'> #'<)))
        ;; This assumes column data is comparable with < and >.
        ;; Real implementation needs generic comparison from cl-vctrs-lite.
        (setf indices (stable-sort indices predicate
                                   :key (lambda (i) (aref col-data i))))))
    indices))

(defmethod arrange ((data tibble) &rest order-specs)
  "Order rows by variables."
  ;; order-specs can be keywords :col or lists (:col :desc)
  (let ((indices (order-by data order-specs)))
    (cl-tibble:slice-rows data indices)))
