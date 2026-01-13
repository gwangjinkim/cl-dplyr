(in-package #:cl-dplyr)

(defmethod select ((data tibble) &rest selection)
  "Select columns by name."
  ;; Selection can be keywords equivalent to column names.
  (cl-tibble:select-cols data selection))
