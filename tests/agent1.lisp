(in-package #:cl-dplyr/tests)

(in-suite :cl-dplyr)

(test slice-test
  (let ((df (cl-tibble:tibble 
             :x #(1 2 3)
             :y #("a" "b" "c"))))
    (is (equalp (pull (slice df 0) :x) #(1)))
    (is (equalp (pull (slice df 1 2) :x) #(2 3)))
    (is (equalp (pull (slice df 0 2) :y) #("a" "c")))))

(test select-test
  (let ((df (cl-tibble:tibble
             :a #(1) :b #(2) :c #(3))))
    (let ((res (select df :a :c)))
      (is (equal (map 'list #'string-downcase (cl-tibble:tbl-names res)) '("a" "c")))
      (is (equalp (pull res :a) #(1)))
      (is (equalp (pull res :c) #(3))))))

(test rename-test
  (let ((df (cl-tibble:tibble :a #(1))))
    (let ((res (rename df :new-a :a)))
      (is (equal (map 'list #'string-downcase (cl-tibble:tbl-names res)) '("new-a")))
      (is (equalp (pull res :new-a) #(1))))))

(test pull-test
  (let ((df (cl-tibble:tibble :x #(10) :y #(20))))
    (is (equalp (pull df :x) #(10)))
    (is (equalp (pull df :y) #(20)))))
