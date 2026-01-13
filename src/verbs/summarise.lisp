(in-package #:cl-dplyr)

(defmethod summarise ((data cl-tibble:tbl) &rest summaries)
  "Summarise ungrouped data (collapse to 1 row)."
  (let ((result-alist '()))
    (loop for (key val) on summaries by #'cddr
          do (let ((res (if (functionp val) (funcall val data) val)))
               (push (cons key (vector res)) result-alist)))
    
    ;; Create tibble from alist
    ;; alist is ((:c . #(val)) (:b . #(val)) ...)
    ;; We need to respect order or just reverse.
    (setf result-alist (nreverse result-alist))
    
    (let ((cols (mapcar #'cdr result-alist))
          (names (mapcar #'car result-alist)))
      ;; Ideally use cl-tibble constructor properly
      ;; If cl-tibble:tibble takes key val key val...
      (apply #'cl-tibble:tibble 
             (loop for n in names for c in cols nconc (list n c))))))


(defmethod summarise ((data grouped-tibble) &rest summaries)
  "Summarise grouped data."
  ;; For each group, slice, apply, then combine.
  
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
               ;; Construct a "row" for this group: `g-vals` + `summary-vals`
               ;; But wait, tibbles are columnar.
               ;; It's efficient to collect columns then build tibble.
               ;; But g-vals is a list. summary-vals is a list.
               (push (append g-vals summary-vals) result-rows)))
    
    ;; result-rows is list of (val1 val2 ... sum1 sum2 ...)
    ;; We need to transpose to columns.
    (setf result-rows (nreverse result-rows))
    
    (let* ((all-col-names (append keys (loop for (k v) on summaries by #'cddr collect k)))
           (num-cols (length all-col-names))
           (col-vectors (make-array num-cols :initial-element nil)))
      
      (loop for i below num-cols do
            (setf (aref col-vectors i) 
                  (map 'vector (lambda (row) (nth i row)) result-rows)))
      
      ;; Build result tibble
      (apply #'cl-tibble:tibble
             (loop for name in all-col-names
                   for i from 0
                   nconc (list name (aref col-vectors i)))))))
