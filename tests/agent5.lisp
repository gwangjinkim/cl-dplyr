(in-package #:cl-dplyr/tests)

(in-suite :cl-dplyr)

(test join-test
  (let ((bd (cl-tibble:tibble :id #(1 2 3) :val_x #("x1" "x2" "x3")))
        (ad (cl-tibble:tibble :id #(1 2 4) :val_y #("y1" "y2" "y4"))))
    
    ;; Inner Join
    (let ((res (inner-join bd ad :by :id)))
      ;; Match 1, 2. (2 rows)
      (is (= 2 (cl-tibble:tbl-nrows res)))
      (is (equalp (pull res :id) #(1 2)))
      (is (equalp (pull res :val_x) #("x1" "x2")))
      (is (equalp (pull res :val_y) #("y1" "y2"))))
    
    ;; Left Join
    (let ((res (left-join bd ad :by :id)))
      ;; Match 1, 2. 3 is kept but val_y is nil/null.
      (is (= 3 (cl-tibble:tbl-nrows res)))
      (is (equalp (pull res :id) #(1 2 3)))
      (is (equalp (pull res :val_x) #("x1" "x2" "x3")))
      ;; TODO: Check how cl-tibble handles missing values or NAs.
      ;; Assuming nil for now if using standard vectors, or :null keyword?
      ;; Just check presence.
      (is (string= "y1" (elt (pull res :val_y) 0)))
      (is (string= "y2" (elt (pull res :val_y) 1)))
      ;; 3rd element should be missing.
      )

    ;; Right Join
    (let ((res (right-join bd ad :by :id)))
      ;; Match 1, 2. 4 is kept.
      (is (= 3 (cl-tibble:tbl-nrows res)))
      (is (equalp (pull res :id) #(1 2 4))))

    ;; Full Join
    (let ((res (full-join bd ad :by :id)))
      ;; 1, 2, 3, 4 -> 4 rows
      (is (= 4 (cl-tibble:tbl-nrows res))))

    ;; Semi Join (keep rows in bd that match ad)
    (let ((res (semi-join bd ad :by :id)))
      ;; 1, 2 (keep logic) but don't add columns from ad
      (is (= 2 (cl-tibble:tbl-nrows res)))
      (is (equalp (pull res :id) #(1 2)))
      ;; Should NOT have val_y
      (is (handler-case (progn (pull res :val_y) nil)
            (condition () t)))) ;; Assuming pull errors on missing col? Or returns nil?
      ;; If pull returns nil/error, verify columns explicitly.
      
    ;; Anti Join (keep rows in bd that DO NOT match ad)
    (let ((res (anti-join bd ad :by :id)))
      ;; 3
      (is (= 1 (cl-tibble:tbl-nrows res)))
      (is (equalp (pull res :id) #(3))))))
