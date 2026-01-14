(in-package #:cl-dplyr/tests)

(in-suite :cl-dplyr)

(test helpers-test
  (let ((df (cl-tibble:tibble :a #(1 2 3 4 5) :b #(10 20 30 40 50))))
    
    (let ((res (-> df
                   (summarise :f (first :a)
                              :l (last :a)
                              :n (nth :a 2)))))
      (is (= 1 (elt (pull res :f) 0)))
      (is (= 5 (elt (pull res :l) 0)))
      (is (= 3 (elt (pull res :n) 0))))

    ;; Test if_else
    (let ((res (-> df
                   (mutate :cat (if_else (> :a 3) "high" "low")))))
      (is (equalp #("low" "low" "low" "high" "high") (pull res :cat))))

    ;; Test case_when
    (let ((res (-> df
                   (mutate :label (case_when 
                                   ((== :a 1) "one")
                                   ((> :a 3) "big")
                                   (t "mid"))))))
      (is (equalp #("one" "mid" "mid" "big" "big") (pull res :label))))))
