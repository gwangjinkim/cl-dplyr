(in-package #:cl-dplyr)

(defun ensure-key-list (k)
  (if (listp k) k (list k)))

(defun hash-rows (data keys)
  "Build a hash mapping signature -> list of row indices."
  (let* ((groups (make-hash-table :test 'equal))
         (n (cl-tibble:tbl-nrows data))
         (keys (ensure-key-list keys)))
    (loop for i below n
          do (let ((sig (loop for k in keys collect (aref (cl-tibble:tbl-col data k) i))))
               (push i (gethash sig groups))))
    ;; Reverse indices to maintain stability if needed
    (maphash (lambda (k v) (setf (gethash k groups) (nreverse v))) groups)
    groups))

(defun get-col-names (df)
  (coerce (cl-tibble:tbl-names df) 'list))

(defun join-cols (x y by type)
  (declare (ignore type))
  "Determine output columns.
   BY is list of keys.
   For inner/left/right/full: keys + (x-cols - keys) + (y-cols - keys).
   But we need to handle name collisions (suffix).
   For now, assume no collisions other than keys."
  (let* ((x-names (get-col-names x))
         (y-names (get-col-names y))
         (by (ensure-key-list by))
         (x-rest (set-difference x-names by :test #'string-equal))
         (y-rest (set-difference y-names by :test #'string-equal)))
    (append by x-rest y-rest)))

(defun build-join-result (x y x-indices y-indices by output-cols)
  (declare (ignore by))
  "Construct result tibble from indices."
  (let* ((n (length x-indices))
         (col-data-list '()))
    
    (loop for col in output-cols do
          (let* ((from-x (member col (get-col-names x) :test #'string-equal))
                 (from-y (member col (get-col-names y) :test #'string-equal))
                 ;; If in BY, take from X (since they matched, or check X/Y for nulls in full join)
                 ;; For now take from X if available, else Y.
                 ;; In Full Join, if X is null, take from Y.
                 (res-vec (make-array n :initial-element nil)))
            
           (loop for i below n
                 for xi = (aref x-indices i)
                 for yi = (aref y-indices i)
                 do (let ((val 
                           (cond
                             ((and xi from-x) (aref (cl-tibble:tbl-col x col) xi))
                             ((and yi from-y) (aref (cl-tibble:tbl-col y col) yi))
                             (t "NA")))) ;; TODO: Proper NA handling
                      (setf (aref res-vec i) val)))
            (push col col-data-list)
            (push res-vec col-data-list)))
    
    (apply #'cl-tibble:tibble (nreverse col-data-list))))

(defmethod inner-join ((x cl-tibble:tbl) (y cl-tibble:tbl) &key by)
  (let* ((y-map (hash-rows y by))
         (keys (ensure-key-list by))
         (x-indices '())
         (y-indices '())
         (n (cl-tibble:tbl-nrows x)))
    
    (loop for i below n
          do (let ((sig (loop for k in keys collect (aref (cl-tibble:tbl-col x k) i))))
               (let ((matches (gethash sig y-map)))
                 ;; (format t "DEBUG: Row ~d Sig ~a Matches ~a~%" i sig matches)
                 (dolist (match matches)
                   (push i x-indices)
                   (push match y-indices)))))
    
    (let ((out-cols (join-cols x y keys :inner)))
      (build-join-result x y 
                         (coerce (nreverse x-indices) 'vector) 
                         (coerce (nreverse y-indices) 'vector)
                         keys out-cols))))

(defmethod left-join ((x cl-tibble:tbl) (y cl-tibble:tbl) &key by)
  (let* ((y-map (hash-rows y by))
         (keys (ensure-key-list by))
         (x-indices '())
         (y-indices '())
         (n (cl-tibble:tbl-nrows x)))
    
    (loop for i below n
          do (let* ((sig (loop for k in keys collect (aref (cl-tibble:tbl-col x k) i)))
                    (matches (gethash sig y-map)))
               (if matches
                   (dolist (match matches)
                     (push i x-indices)
                     (push match y-indices))
                   (progn
                     (push i x-indices)
                     (push nil y-indices)))))
    
    (let ((out-cols (join-cols x y keys :left)))
      (build-join-result x y 
                         (coerce (nreverse x-indices) 'vector) 
                         (coerce (nreverse y-indices) 'vector)
                         keys out-cols))))

(defmethod right-join ((x cl-tibble:tbl) (y cl-tibble:tbl) &key by)
  ;; Symetric to left-join. Swap X and Y, but remember to reorder columns potentially?
  ;; Or just implement directly.
  (let* ((x-map (hash-rows x by))
         (keys (ensure-key-list by))
         (x-indices '())
         (y-indices '())
         (n (cl-tibble:tbl-nrows y)))
    
    (loop for i below n
          do (let* ((sig (loop for k in keys collect (aref (cl-tibble:tbl-col y k) i)))
                    (matches (gethash sig x-map)))
               (if matches
                   (dolist (match matches)
                     (push match x-indices)
                     (push i y-indices))
                   (progn
                     (push nil x-indices)
                     (push i y-indices)))))
    
    (let ((out-cols (join-cols x y keys :right)))
      (build-join-result x y 
                         (coerce (nreverse x-indices) 'vector) 
                         (coerce (nreverse y-indices) 'vector)
                         keys out-cols))))

(defmethod full-join ((x cl-tibble:tbl) (y cl-tibble:tbl) &key by)
  ;; 1. Do left join
  ;; 2. Find rows in Y not matched
  (let* ((y-map (hash-rows y by))
         (keys (ensure-key-list by))
         (x-indices '())
         (y-indices '())
         (y-matched (make-hash-table)) ;; Set of Y indices matched
         (n (cl-tibble:tbl-nrows x)))
    
    (loop for i below n
          do (let* ((sig (loop for k in keys collect (aref (cl-tibble:tbl-col x k) i)))
                    (matches (gethash sig y-map)))
               (if matches
                   (dolist (match matches)
                     (push i x-indices)
                     (push match y-indices)
                     (setf (gethash match y-matched) t))
                   (progn
                     (push i x-indices)
                     (push nil y-indices)))))
    
    ;; Add unmatched Y rows
    (let ((ny (cl-tibble:tbl-nrows y)))
      (loop for i below ny
            unless (gethash i y-matched)
            do (progn
                 (push nil x-indices)
                 (push i y-indices))))
    
    (let ((out-cols (join-cols x y keys :full)))
      (build-join-result x y 
                         (coerce (nreverse x-indices) 'vector) 
                         (coerce (nreverse y-indices) 'vector)
                         keys out-cols))))

(defmethod semi-join ((x cl-tibble:tbl) (y cl-tibble:tbl) &key by)
  (let* ((y-map (hash-rows y by))
         (keys (ensure-key-list by))
         (indices '())
         (n (cl-tibble:tbl-nrows x)))
    
    (loop for i below n
          do (let ((sig (loop for k in keys collect (aref (cl-tibble:tbl-col x k) i))))
               (when (gethash sig y-map)
                 (push i indices))))
    
    (cl-tibble:slice x :rows (nreverse indices))))

(defmethod anti-join ((x cl-tibble:tbl) (y cl-tibble:tbl) &key by)
  (let* ((y-map (hash-rows y by))
         (keys (ensure-key-list by))
         (indices '())
         (n (cl-tibble:tbl-nrows x)))
    
    (loop for i below n
          do (let ((sig (loop for k in keys collect (aref (cl-tibble:tbl-col x k) i))))
               (unless (gethash sig y-map)
                 (push i indices))))
    
    (cl-tibble:slice x :rows (nreverse indices))))
