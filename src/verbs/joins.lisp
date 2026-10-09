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

(defun %col-name (k)
  "Column name as the string cl-tibble stores (keywords are downcased)."
  (typecase k
    (string k)
    (symbol (string-downcase (symbol-name k)))
    (t (princ-to-string k))))

(defun join-keys (x y by)
  "Key column names as strings. With no BY, join on all common columns
(a natural join, like dplyr)."
  (if by
      (mapcar #'%col-name (ensure-key-list by))
      (let* ((y-names (get-col-names y))
             (common (remove-if-not (lambda (n) (member n y-names :test #'string=))
                                    (get-col-names x))))
        (unless common
          (error "join: X and Y have no common columns; supply :BY."))
        common)))

(defun join-cols (x y by &optional (suffix '(".x" ".y")))
  "Output column plan: a list of (OUT-NAME SOURCE COL) with SOURCE one of
:KEY, :X or :Y. Order is keys, then X's other columns, then Y's, each in
their original order. Non-key names present in both get SUFFIX."
  (let* ((x-rest (remove-if (lambda (n) (member n by :test #'string=)) (get-col-names x)))
         (y-rest (remove-if (lambda (n) (member n by :test #'string=)) (get-col-names y)))
         (clash (intersection x-rest y-rest :test #'string=)))
    (flet ((out (n suf) (if (member n clash :test #'string=)
                            (concatenate 'string n suf)
                            n)))
      (append (mapcar (lambda (k) (list k :key k)) by)
              (mapcar (lambda (n) (list (out n (first suffix)) :x n)) x-rest)
              (mapcar (lambda (n) (list (out n (second suffix)) :y n)) y-rest)))))

(defun build-join-result (x y x-indices y-indices plan)
  "Construct the result tibble from matched row indices and a JOIN-COLS
plan. Unmatched sides are filled with cl-vctrs-lite:*na*."
  (let ((n (length x-indices))
        (col-data-list '()))
    (loop for (out-name source col) in plan do
          (let ((res-vec (make-array n))
                (x-col (unless (eq source :y) (cl-tibble:tbl-col x col)))
                (y-col (unless (eq source :x) (cl-tibble:tbl-col y col))))
            (loop for i below n
                  for xi = (aref x-indices i)
                  for yi = (aref y-indices i)
                  do (setf (aref res-vec i)
                           (cond ((and xi x-col) (aref x-col xi))
                                 ((and yi y-col) (aref y-col yi))
                                 (t *na*))))
            (push out-name col-data-list)
            (push res-vec col-data-list)))
    (apply #'cl-tibble:tibble (nreverse col-data-list))))

(defmethod %inner-join ((x cl-tibble:tbl) (y cl-tibble:tbl) &key by (suffix '(".x" ".y")))
  (let* ((keys (join-keys x y by))
         (y-map (hash-rows y keys))
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
    
    (build-join-result x y
                       (coerce (nreverse x-indices) 'vector)
                       (coerce (nreverse y-indices) 'vector)
                       (join-cols x y keys suffix))))

(defmethod %left-join ((x cl-tibble:tbl) (y cl-tibble:tbl) &key by (suffix '(".x" ".y")))
  (let* ((keys (join-keys x y by))
         (y-map (hash-rows y keys))
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
    
    (build-join-result x y
                       (coerce (nreverse x-indices) 'vector)
                       (coerce (nreverse y-indices) 'vector)
                       (join-cols x y keys suffix))))

(defmethod %right-join ((x cl-tibble:tbl) (y cl-tibble:tbl) &key by (suffix '(".x" ".y")))
  (let* ((keys (join-keys x y by))
         (x-map (hash-rows x keys))
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
    
    (build-join-result x y
                       (coerce (nreverse x-indices) 'vector)
                       (coerce (nreverse y-indices) 'vector)
                       (join-cols x y keys suffix))))

(defmethod %full-join ((x cl-tibble:tbl) (y cl-tibble:tbl) &key by (suffix '(".x" ".y")))
  ;; 1. Do left join
  ;; 2. Find rows in Y not matched
  (let* ((keys (join-keys x y by))
         (y-map (hash-rows y keys))
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
    
    (build-join-result x y
                       (coerce (nreverse x-indices) 'vector)
                       (coerce (nreverse y-indices) 'vector)
                       (join-cols x y keys suffix))))

(defmethod %semi-join ((x cl-tibble:tbl) (y cl-tibble:tbl) &key by)
  (let* ((keys (join-keys x y by))
         (y-map (hash-rows y keys))
         (indices '())
         (n (cl-tibble:tbl-nrows x)))
    
    (loop for i below n
          do (let ((sig (loop for k in keys collect (aref (cl-tibble:tbl-col x k) i))))
               (when (gethash sig y-map)
                 (push i indices))))
    
    (cl-tibble:slice x :rows (nreverse indices))))

(defmethod %anti-join ((x cl-tibble:tbl) (y cl-tibble:tbl) &key by)
  (let* ((keys (join-keys x y by))
         (y-map (hash-rows y keys))
         (indices '())
         (n (cl-tibble:tbl-nrows x)))
    
    (loop for i below n
          do (let ((sig (loop for k in keys collect (aref (cl-tibble:tbl-col x k) i))))
               (unless (gethash sig y-map)
                 (push i indices))))
    
    (cl-tibble:slice x :rows (nreverse indices))))

(defun .inner-join (x y &key by (suffix '(".x" ".y"))) (%inner-join x y :by by :suffix suffix))
(defun .left-join (x y &key by (suffix '(".x" ".y"))) (%left-join x y :by by :suffix suffix))
(defun .right-join (x y &key by (suffix '(".x" ".y"))) (%right-join x y :by by :suffix suffix))
(defun .full-join (x y &key by (suffix '(".x" ".y"))) (%full-join x y :by by :suffix suffix))
(defun .semi-join (x y &key by) (%semi-join x y :by by))
(defun .anti-join (x y &key by) (%anti-join x y :by by))

