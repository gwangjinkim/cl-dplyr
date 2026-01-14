(in-package #:cl-dplyr/tests)

(in-suite :cl-dplyr)

(test n-desc-test
  (let ((df (cl-tibble:tibble :x #(1 1 2 2 2) :y #(10 20 30 40 50))))
    
    ;; Test n() in summarise
    (let ((res (-> df
                   (group-by :x)
                   (summarise :cnt (n)))))
      (is (= 2 (cl-tibble:tbl-nrows res)))
      (is (equalp #(2 3) (pull res :cnt))))
    
    ;; Test n() in mutate (should be total rows)
    (let ((res (-> df
                   (mutate :total (n)))))
      (is (equalp (make-array 5 :initial-element 5) (pull res :total))))
    
    ;; Test n-distinct
    (let ((res (-> df
                   (summarise :distinct_x (n-distinct :x)
                              :distinct_y (n-distinct :y)))))
      (is (equalp #(2) (pull res :distinct_x)))
      (is (equalp #(5) (pull res :distinct_y))))

    ;; Test arrange desc
    (let ((res (-> df
                   (arrange (desc :y)))))
      (is (equalp #(50 40 30 20 10) (pull res :y))))

    ;; Test arrange asc
    (let ((res (-> df
                   (arrange (desc :y))
                   (arrange (asc :y)))))
      (is (equalp #(10 20 30 40 50) (pull res :y))))

    ;; Test arrange mixed
    (let ((df2 (cl-tibble:tibble :g #(1 1 2 2) :v #(10 20 40 30))))
      (let ((res (-> df2
                     (arrange :g (desc :v)))))
        (is (equalp #(20 10 40 30) (pull res :v)))
        (is (equalp #(1 1 2 2) (pull res :g)))))))
