(in-package #:cl-dplyr)

(defstruct (grouped-tibble (:conc-name group-))
  data       ;; The original tibble
  groups     ;; A list of (group-values . row-indices)
  keys       ;; A list of grouping column names
  )

(defmethod group-by ((data cl-tibble:tbl) &rest variables)
  "Group tibble by variables."
  (let* ((n (cl-tibble:tbl-nrows data))
         (groups (make-hash-table :test 'equal))
         (vars (if variables variables (cl-tibble:tbl-names data))))
    
    ;; 1. Scan data and build groups map
    (loop for i below n
          do (let ((signature (loop for v in vars
                                    collect (aref (cl-tibble:tbl-col data v) i))))
               (push i (gethash signature groups))))
    
    ;; 2. Convert hash to list of (values . indices) and reverse indices
    (let ((group-list 
           (loop for k being the hash-keys of groups using (hash-value v)
                 collect (cons k (nreverse v)))))
      
      (make-grouped-tibble :data data
                           :groups group-list
                           :keys vars))))

(defun n-groups (g)
  (length (group-groups g)))
