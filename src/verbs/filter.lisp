(in-package #:cl-dplyr)

(defmethod %filter ((data cl-tibble:tbl) predicate)
  "Keep rows matching a predicate function."
  (let* ((mask (funcall predicate data))
         (n (cl-tibble:tbl-nrows data))
         (indices 
          (loop for i below n
                for val = (elt mask i)
                when (not (is-missing-p val)) 
                collect i)))
    (cl-tibble:slice data :rows indices)))

(defmacro filter (df expr)
  (let ((df-sym (gensym "DF")))
    (if (and (consp expr) (eq (first expr) 'lambda))
        `(%filter ,df ,expr)
        `(%filter ,df (lambda (,df-sym) ,(parse-dsl expr df-sym))))))
