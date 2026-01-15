(in-package #:cl-dplyr)

(defmethod %rename ((data cl-tibble:tbl) &rest renaming)
  "Rename columns."
  (let* ((old-names (coerce (cl-tibble:tbl-names data) 'list))
         (new-names (copy-list old-names))
         (renaming-alist (loop for (new old) on renaming by #'cddr
                               collect (cons old new))))
    ;; Update names in place in the list
    (loop for cell on new-names
          for name = (car cell)
          for entry = (assoc name renaming-alist :test #'string-equal)
          when entry
          do (setf (car cell) (cdr entry)))
    
    (apply #'cl-tibble:tibble
           (loop for name in new-names
                 for old-name in old-names
                 nconc (list name (cl-tibble:tbl-col data old-name))))))

(defmacro .rename (df &rest renaming)
  `(%rename ,df ,@(mapcar #'unquote-col renaming)))
