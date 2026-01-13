(in-package #:cl-dplyr/tests)

(in-suite :cl-dplyr)

(test arrows-test
  (let ((df (cl-tibble:tibble :a #(1 2 3 4) :b #(10 20 30 40))))
    
    ;; Test -> (thread-first) with DSL
    (let ((res (-> df
                 (select a b)
                 (filter (> a 2)))))
      (is (= 2 (cl-tibble:tbl-nrows res)))
      (is (equalp (pull res :a) #(3 4))))

    ;; Test -> with dot placeholder (_)
    ;; (pull df :a) -> (-> df (pull _ :a))
    (let ((res (-> df
                 (pull _ :a))))
      (is (equalp res #(1 2 3 4))))
      
    ;; Test ->> (thread-last)
    ;; (->> :a (pull df)) -> (pull df :a)
    (let ((res (->> :a
                 (pull df))))
      (is (equalp res #(1 2 3 4))))))
