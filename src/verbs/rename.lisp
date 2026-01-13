(in-package #:cl-dplyr)

(defmethod rename ((data tibble) &rest renaming)
  "Rename columns. Arguments are typically new-name=old-name pairs, but here we expect (new-name old-name) pairs or keywords."
  ;; renaming list of (new . old)
  ;; To be robust, we need to handle how arguments are passed. 
  ;; We'll assume a list of pairs for now: :new :old
  (loop for (new old) on renaming by #'cddr
        collect (cons new old) into pairs
        finally (return (cl-tibble:rename-cols data pairs))))
