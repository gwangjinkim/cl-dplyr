(in-package #:cl-dplyr/tests)

(in-suite :cl-dplyr)

(test group-by-test
  (let ((df (cl-tibble:tibble :a #(1 1 2 2) :b #(10 20 30 40))))
    (let ((g (group-by df :a)))
      (is (typep g 'cl-dplyr::grouped-tibble))
      ;; Check groups: 1->(0,1), 2->(2,3)
      ;; We need an accessor to check groups or infer from behaviour
      (is (= 2 (cl-dplyr::n-groups g)))
      
      (let ((ung (ungroup g)))
        (is (not (typep ung 'cl-dplyr::grouped-tibble)))
        (is (typep ung 'cl-tibble:tbl))))))

(test summarise-test
  ;; Ungrouped
  (let ((df (cl-tibble:tibble :a #(1 2 3))))
    (let ((res (summarise df :mean-a (lambda (d) 
                                       ;; d is the tibble (or subset)
                                       ;; simple mean of vector
                                       (let ((v (pull d :a)))
                                         (/ (reduce #'+ v) (length v)))))))
      (is (= 1 (cl-tibble:tbl-nrows res)))
      (is (= 2 (aref (pull res :mean-a) 0)))))

  ;; Grouped
  (let ((df (cl-tibble:tibble 
             :g #(1 1 2 2 3) 
             :x #(10 20 50 60 100))))
    (let* ((g (group-by df :g))
           (res (summarise g :mean-x (lambda (d)
                                       (let ((v (pull d :x)))
                                         (/ (reduce #'+ v) (length v)))))))
      ;; Groups: 1 (15), 2 (55), 3 (100)
      (is (= 3 (cl-tibble:tbl-nrows res)))
      (is (equalp (pull res :g) #(1 2 3)))
      (is (equalp (pull res :mean-x) #(15 55 100))))))
