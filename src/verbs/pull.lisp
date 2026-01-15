(in-package #:cl-dplyr)

(defmethod %pull ((data cl-tibble:tbl) var)
  "Extract a single column."
  (cl-tibble:tbl-col data var))

(defun .pull (data var)
  (%pull data var))
