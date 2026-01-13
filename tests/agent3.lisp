(in-package #:cl-dplyr/tests)

(in-suite :cl-dplyr)

(test filter-test
  (let ((df (cl-tibble:tibble :a #(1 2 3 4) :b #(10 20 30 40))))
    ;; Filter rows where a > 2
    (let ((res (filter df (lambda (d) 
                            (map 'vector (lambda (x) (> x 2)) (pull d :a))))))
      (is (equalp (pull res :a) #(3 4)))
      (is (equalp (pull res :b) #(30 40))))
    
    ;; Filter rows where b = 20
    (let ((res (filter df (lambda (d)
                            (map 'vector (lambda (x) (= x 20)) (pull d :b))))))
      (is (equalp (pull res :a) #(2)))
      (is (equalp (pull res :b) #(20))))))

(test distinct-test
  (let ((df (cl-tibble:tibble 
             :x #(1 1 2 2 3) 
             :y #(1 1 2 3 3))))
    ;; Distinct all cols
    (let ((res (distinct df)))
      ;; 1,1; 2,2; 2,3; 3,3 -> 4 rows
      (is (= 4 (cl-tibble:tbl-nrows res)))
      (is (equalp (pull res :x) #(1 2 2 3)))
      (is (equalp (pull res :y) #(1 2 3 3))))
    
    ;; Distinct by x
    (let ((res (distinct df :x)))
      ;; 1, 2, 3 -> 3 rows
      (is (= 3 (cl-tibble:tbl-nrows res)))
      (is (equalp (pull res :x) #(1 2 3))))))
