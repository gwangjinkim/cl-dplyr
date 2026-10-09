(in-package #:cl-dplyr/tests)

(in-suite :cl-dplyr)

(test mutate-recycles-string-scalar
  (let ((df (tibble :x #(1 2 3))))
    (is (equalp #("abc" "abc" "abc") (.pull (.mutate df :s "abc") :s))))
  ;; previously a 2-row tibble errored and a 3-row one got #\a #\b #\c
  (let ((df (tibble :x #(1 2))))
    (is (equalp #("k" "k") (.pull (.mutate df :s "k") :s)))))

(test if-else-na-condition-gives-na
  (let ((r (if-else (vector t nil cl-vctrs-lite:*na*) "y" "n")))
    (is (equal "y" (aref r 0)))
    (is (equal "n" (aref r 1)))
    (is (cl-vctrs-lite:na-p (aref r 2)))))

(test mutate-na-propagates-through-arithmetic
  (let ((df (tibble :a (vector 1 cl-vctrs-lite:*na* 3) :b #(10 20 30))))
    (is (cl-vctrs-lite:na-p (aref (.pull (.mutate df :c (- :b :a)) :c) 1)))))
