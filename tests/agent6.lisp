(in-package #:cl-dplyr/tests)

(in-suite :cl-dplyr)

(test chain-test
  (let ((df (cl-tibble:tibble :a #(1 2 3 4) :b #(10 20 30 40))))
    
    ;; Test threading with unquoted select
    ;; (chain df (select a b)) -> (select df :a :b)
    (let ((res (chain df
                 (select a)
                 (filter (> a 2)))))
      (is (= 2 (cl-tibble:tbl-nrows res)))
      (is (equalp (pull res :a) #(3 4))))

    ;; Test mutate expression
    ;; (mutate (c (+ a b)))
    (let ((res (chain df 
                 (mutate (c (+ a b))))))
      (is (equalp (pull res :c) #(11 22 33 44))))
    
    ;; Test arrange with desc
    (let ((res (chain df
                 (arrange (desc a)))))
      (is (equalp (pull res :a) #(4 3 2 1))))))
