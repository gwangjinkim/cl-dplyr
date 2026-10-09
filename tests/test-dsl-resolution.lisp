(in-package #:cl-dplyr/tests)

(in-suite :cl-dplyr)

;;; How the DSL decides what is a column and what is ordinary Lisp,
;;; and NA-aware logical operators.

(defparameter *threshold* 2)

(test dsl-not-equal-is-not-equal
  (let ((df (tibble :x #(1 2 3))))
    (is (equalp #(2 3) (.pull (.filter df (!= :x 1)) :x)))
    (is (equalp #(2 3) (.pull (.filter df (/= :x 1)) :x)))))

(test dsl-and-or-not
  (let ((df (tibble :x #(1 2 3 4) :y #("a" "b" "a" "b"))))
    (is (equalp #(3) (.pull (.filter df (& (> :x 1) (== :y "a"))) :x)))
    (is (equalp #(1 2 4) (.pull (.filter df (\| (< :x 3) (== :x 4))) :x)))
    (is (equalp #(2 4) (.pull (.filter df (! (== :y "a"))) :x)))))

(test dsl-is-na-and-na-propagation
  (let ((df (tibble :x (vector 1 cl-vctrs-lite:*na* 3))))
    (is (equalp #(1 3) (.pull (.filter df (! (is-na :x))) :x)))
    ;; a comparison with NA is NA, and filter drops NA rows (like R)
    (is (equalp #(3) (.pull (.filter df (> :x 1)) :x)))))

(test dsl-constants-and-specials-are-not-columns
  (let ((df (tibble :x #(1 2 3))))
    (is (equalp #(3) (.pull (.filter df (> :x *threshold*)) :x)))
    (is (equalp #("yes" "yes" "yes")
                (.pull (.mutate df :y (if-else (> :x 0) "yes" nil)) :y)))))

(test dsl-env-escape
  (let ((df (tibble :x #(1 2 3)))
        (k 1))
    (is (equalp #(2 3) (.pull (.filter df (> :x (.env k))) :x)))))

(test dsl-quote-and-function-untouched
  (let ((df (tibble :s #("a" "b"))))
    (is (equalp #("A" "B")
                (.pull (.mutate df :u (map 'vector #'string-upcase :s)) :u)))
    (is (equalp #("A" "B")
                (.pull (.mutate df :u (map 'vector (lambda (e) (string-upcase e)) :s)) :u)))))

(test dsl-operators-work-from-any-package
  (let ((df (tibble :x #(1 2 3))))
    (is (equalp #(1)
                (.pull (eval `(.filter ,df (,(intern "==" :cl-user) :x 1))) :x)))))
