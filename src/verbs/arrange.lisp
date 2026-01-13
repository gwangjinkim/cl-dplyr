(in-package #:cl-dplyr)

(defun order-by (df vars)
  "Helper to compute sort permutation indices.
   VARS is a list of (column-name &optional direction)."
  (let* ((n (cl-tibble:tbl-nrows df))
         (indices (loop for i below n collect i)))
    (dolist (spec (reverse vars)) 
      (let* ((col-name (if (consp spec) (first spec) spec))
             (direction (if (consp spec) (second spec) :asc))
             (col-data (cl-tibble:tbl-col df col-name)))
        (setf indices (stable-sort indices 
                                   (lambda (a b)
                                     (cond
                                       ((is-missing-p a) nil)
                                       ((is-missing-p b) t)
                                       ((eq direction :desc)
                                        (cond
                                          ((and (numberp a) (numberp b)) (> a b))
                                          ((and (stringp a) (stringp b)) (string> a b))
                                          (t (string> (format nil "~a" a) (format nil "~a" b)))))
                                       (t
                                        (cond
                                          ((and (numberp a) (numberp b)) (< a b))
                                          ((and (stringp a) (stringp b)) (string< a b))
                                          (t (string< (format nil "~a" a) (format nil "~a" b)))))))
                                   :key (lambda (i) (aref col-data i))))))
    indices))

(defmethod arrange ((data cl-tibble:tbl) &rest order-specs)
  "Order rows by variables."
  (let ((indices (order-by data order-specs)))
    (cl-tibble:slice data :rows indices)))
