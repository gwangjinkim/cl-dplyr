(in-package #:cl-dplyr)

(defmethod pull ((data tibble) var)
  "Extract a single column."
  (cl-tibble:column data var))
