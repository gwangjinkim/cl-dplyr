(in-package #:cl-dplyr/tests)

(in-suite :cl-dplyr)

(test arrange-test
  (let ((df (cl-tibble:tibble :a #(3 1 2) :b #(1 2 3))))
    (let ((sorted (arrange df :a)))
      (is (equalp (pull sorted :a) #(1 2 3)))
      (is (equalp (pull sorted :b) #(2 3 1)))))
  
  ;; Descending
  (let ((df (cl-tibble:tibble :a #(1 2 3))))
    (let ((sorted (arrange df '(:a :desc))))
      (is (equalp (pull sorted :a) #(3 2 1)))))
  
  ;; Multiple columns
  (let ((df (cl-tibble:tibble 
             :x #(1 1 2 2) 
             :y #(2 1 4 3))))
    (let ((sorted (arrange df :x :y)))
      (is (equalp (pull sorted :x) #(1 1 2 2)))
      (is (equalp (pull sorted :y) #(1 2 3 4))))))

(test mutate-test
  (let ((df (cl-tibble:tibble :a #(1 2 3))))
    ;; Constant value (recycling)
    ;; TODO: Implement recycling if not present. 
    ;; My mutate currently does: (setf result (bind-cols ...))
    ;; bind-cols in cl-tibble likely handles recycling or errors.
    ;; For now test valid vector extension.
    (let ((res (mutate df :b (lambda (d) (map 'vector #'1+ (pull d :a))))))
      (is (equalp (pull res :b) #(2 3 4)))
      (is (equalp (pull res :a) #(1 2 3))))
    
    ;; Sequential mutation
    (let ((res (mutate df 
                       :b (lambda (d) (map 'vector #'1+ (pull d :a)))
                       :c (lambda (d) (map 'vector #'* (pull d :b) #(2 2 2))))))
      (is (equalp (pull res :b) #(2 3 4)))
      (is (equalp (pull res :c) #(4 6 8))))))
