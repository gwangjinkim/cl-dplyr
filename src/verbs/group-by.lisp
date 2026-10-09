(in-package #:cl-dplyr)

(defstruct (grouped-tibble (:conc-name group-))
  data       ;; The original tibble
  groups     ;; A list of (group-values . row-indices)
  keys       ;; A list of grouping column names
  )

;;; Ordering of group keys: numbers < strings < logicals < other values,
;;; NA last; numbers by <, strings by STRING<, NIL before T, others by
;;; their printed form.
(defun %value-rank (v)
  (cond ((cl-vctrs-lite:na-p v) 4)
        ((realp v) 0)
        ((stringp v) 1)
        ((or (eq v t) (null v)) 2)
        (t 3)))

(defun value< (a b)
  (let ((ra (%value-rank a)) (rb (%value-rank b)))
    (if (/= ra rb)
        (< ra rb)
        (case ra
          (0 (< a b))
          (1 (string< a b))
          (2 (and (null a) (eq b t)))
          (3 (string< (prin1-to-string a) (prin1-to-string b)))
          (t nil)))))

(defun signature< (a b)
  "Lexicographic VALUE< on two equal-length lists of key values."
  (loop for x in a
        for y in b
        do (cond ((value< x y) (return t))
                 ((value< y x) (return nil)))
        finally (return nil)))

(defmethod %group-by ((data cl-tibble:tbl) &rest variables)
  "Group tibble by variables."
  (let* ((n (cl-tibble:tbl-nrows data))
         (groups (make-hash-table :test 'equal))
         (vars (if variables variables (cl-tibble:tbl-names data))))
    
    ;; 1. Scan data and build groups map
    (loop for i below n
          do (let ((signature (loop for v in vars
                                    collect (aref (cl-tibble:tbl-col data v) i))))
               (push i (gethash signature groups))))
    
    ;; 2. Convert hash to list of (values . indices), reverse indices, and
    ;;    sort groups by their key values (like dplyr), so output order
    ;;    does not depend on hash-table iteration order.
    (let ((group-list
           (sort (loop for k being the hash-keys of groups using (hash-value v)
                       collect (cons k (nreverse v)))
                 #'signature< :key #'car)))
      
      (make-grouped-tibble :data data
                           :groups group-list
                           :keys vars))))

(defun .group-by (data &rest variables)
  (apply #'%group-by data variables))

(defun n-groups (g)
  (length (group-groups g)))

;;; Grouped verbs. MUTATE and FILTER evaluate their expressions once per
;;; group (so (mean x) is the group mean) and keep the original row order;
;;; the result stays grouped by the same keys.

(defun %regroup (tbl keys)
  (apply #'%group-by tbl keys))

(defmethod %mutate ((data grouped-tibble) &rest mutations)
  (let* ((base (group-data data))
         (n (cl-tibble:tbl-nrows base))
         (names (loop for (key nil) on mutations by #'cddr collect key))
         (columns (loop repeat (length names) collect (make-array n))))
    (loop for (nil . indices) in (group-groups data)
          do (let ((sub (apply #'%mutate (cl-tibble:slice base :rows indices) mutations)))
               (loop for name in names
                     for out in columns
                     do (let ((col (cl-tibble:tbl-col sub name)))
                          (loop for i in indices
                                for k from 0
                                do (setf (aref out i) (aref col k)))))))
    (%regroup (apply #'%mutate base
                     (loop for name in names
                           for col in columns
                           nconc (list name col)))
              (group-keys data))))

(defmethod %filter ((data grouped-tibble) predicate)
  (let* ((base (group-data data))
         (kept (loop for (nil . indices) in (group-groups data)
                     nconc (let ((mask (funcall predicate (cl-tibble:slice base :rows indices))))
                             (loop for i in indices
                                   for k from 0
                                   for val = (elt mask k)
                                   unless (is-missing-p val)
                                   collect i)))))
    (%regroup (cl-tibble:slice base :rows (sort kept #'<))
              (group-keys data))))
