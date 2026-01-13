(in-package #:cl-dplyr)

(defmethod %mutate ((data cl-tibble:tbl) &rest mutations)
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
               (setf result (cl-tibble:bind-cols result (cl-tibble:tibble col-name new-col-data)))))
    result))

(defmacro mutate (df &rest mutations)
  (let ((df-sym (gensym "DF")))
    `(%mutate ,df 
              ,@(loop for (col expr) on mutations by #'cddr
                      collect (unquote-col col)
                      collect (if (and (consp expr) (eq (first expr) 'lambda))
                                  expr
                                  `(lambda (,df-sym) ,(parse-dsl expr df-sym)))))))
