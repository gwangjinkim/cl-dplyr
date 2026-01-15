(in-package #:cl-dplyr)

(defun .n ()
  "Returns the number of observations in the current group.
   This function can only be used from within a verb like summarise, mutate, or filter."
  (error "n() must be used within a dplyr verb context."))

(defun .n-distinct (x)
  "Count the number of unique values in X."
  (length (remove-duplicates x :test #'equalp)))

(defun .desc (x)
  "Helper to specify descending order for arrange.
   Returns a specifier that arrange understands."
  `(',x :desc))

(defun .asc (x)
  "Helper to specify ascending order for arrange.
   Returns x as is, or a specifier."
  `(',x :asc))
