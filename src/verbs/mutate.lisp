(in-package #:cl-dplyr)

(defmethod %mutate ((data cl-tibble:tbl) &rest mutations)
  "Add or modify columns."
  ;; 1. Copy data by slicing all rows
  (let* ((n (cl-tibble:tbl-nrows data))
         (indices (loop for i below n collect i))
         (result (cl-tibble:slice data :rows indices)))
    
    (loop for (key val) on mutations by #'cddr
          do (let* ((col-name key)
                    (raw-val (cond
                               ((functionp val) (funcall val result)) 
                               (t val)))
                    (new-col-data (if (vectorp raw-val) 
                                      raw-val
                                      (make-array (cl-tibble:tbl-nrows result) :initial-element raw-val))))
               
               ;; Check if column exists, if so drop it first to allow overwrite
               (let ((existing-names (coerce (cl-tibble:tbl-names result) 'list)))
                 (when (member (string-downcase (symbol-name col-name)) existing-names :test #'string-equal)
                   ;; Reconstruct tibble without this column using %select
                   (let ((keep-cols (remove (string-downcase (symbol-name col-name)) existing-names :test #'string-equal)))
                     (setf result (apply #'%select result (mapcar (lambda (s) (intern (string-upcase s) :keyword)) keep-cols))))))

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
