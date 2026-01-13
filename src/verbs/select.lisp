(in-package #:cl-dplyr)

(defmethod select ((data cl-tibble:tbl) &rest selection)
  "Select columns by name."
  ;; selection is list of column names (keywords)
  (let ((cols (loop for name in selection
                    collect (cl-tibble:tbl-col data name))))
    (apply #'cl-tibble:tibble
           (loop for name in selection
                 for col in cols
                 nconc (list name col)))))
