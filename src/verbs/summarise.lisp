(in-package #:cl-dplyr)

(defmethod %summarise ((data cl-tibble:tbl) &rest summaries)
  "Summarise ungrouped data (collapse to 1 row)."
  (let ((result-alist '()))
    (loop for (key val) on summaries by #'cddr
          do (let ((res (if (functionp val) (funcall val data) val)))
               (push (cons key (vector res)) result-alist)))
    
    (setf result-alist (nreverse result-alist))
    
    (let ((cols (mapcar #'cdr result-alist))
          (names (mapcar #'car result-alist)))
      (apply #'cl-tibble:tibble 
             (loop for n in names for c in cols nconc (list n c))))))


(defmethod %summarise ((data grouped-tibble) &rest summaries)
  "Summarise grouped data."
  (let* ((keys (group-keys data))
         (groups (group-groups data))
         (result-rows '()))
    
    (loop for (g-vals . g-indices) in groups
          do (let* ((sub-df (cl-tibble:slice (group-data data) :rows g-indices))
                    (summary-vals 
                     (loop for (key val) on summaries by #'cddr
                           collect (if (functionp val) 
                                       (funcall val sub-df) 
                                       val))))
               (push (append g-vals summary-vals) result-rows)))
    
    (setf result-rows (nreverse result-rows))
    
    (let* ((all-col-names (append keys (loop for (k v) on summaries by #'cddr collect k)))
           (num-cols (length all-col-names))
           (col-vectors (make-array num-cols :initial-element nil)))
      
      (loop for i below num-cols do
            (setf (aref col-vectors i) 
                  (map 'vector (lambda (row) (nth i row)) result-rows)))
      
      (apply #'cl-tibble:tibble
             (loop for name in all-col-names
                   for i from 0
                   nconc (list name (aref col-vectors i)))))))

(defmacro .summarise (df &rest summaries)
  (let ((df-sym (gensym "DF")))
    `(%summarise ,df 
                 ,@(loop for (col expr) on summaries by #'cddr
                         collect (unquote-col col)
                         collect (if (and (consp expr) (eq (first expr) 'lambda))
                                     expr
                                     `(lambda (,df-sym) ,(parse-dsl expr df-sym)))))))

(defmacro .summarize (df &rest summaries)
  `(.summarise ,df ,@summaries))
