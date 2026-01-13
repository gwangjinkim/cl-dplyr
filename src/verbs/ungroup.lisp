(in-package #:cl-dplyr)

(defmethod ungroup ((data grouped-tibble))
  "Remove grouping."
  (group-data data))

(defmethod ungroup ((data cl-tibble:tbl))
  "Ungrouping a regular tibble does nothing."
  data)
