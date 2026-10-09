(in-package #:cl-dplyr/tests)

(in-suite :cl-dplyr)

;;; Group ordering and grouped mutate/filter.

(defparameter *grp-df*
  (tibble :g #("b" "a" "b" "a" "c")
          :x #(10 1 30 3 100)))

(test group-order-is-sorted-not-insertion
  (is (equalp #("a" "b" "c")
              (.pull (-> *grp-df* (group-by g) (summarise n (n))) :g))))

(test group-order-numbers-and-na
  (let ((df (tibble :k (vector 3 cl-vctrs-lite:*na* 1 2) :v #(1 1 1 1))))
    (let ((ks (.pull (-> df (group-by k) (summarise n (n))) :k)))
      (is (equalp #(1 2 3) (subseq ks 0 3)))
      (is (cl-vctrs-lite:na-p (aref ks 3))))))

(test grouped-mutate-uses-group-statistics
  ;; centre x within group; row order is preserved
  (let ((res (.ungroup (-> *grp-df* (group-by g) (mutate c (- x (mean x)))))))
    (is (equalp #("b" "a" "b" "a" "c") (.pull res :g)))
    (is (equalp #(-10 -1 10 1 0) (.pull res :c)))))

(test grouped-mutate-n-is-group-size
  (let ((res (.ungroup (-> *grp-df* (group-by g) (mutate size (n))))))
    (is (equalp #(2 2 2 2 1) (.pull res :size)))))

(test grouped-filter-per-group
  ;; keep rows above their group's mean; original order preserved
  (let ((res (-> *grp-df* (group-by g) (filter (> x (mean x))))))
    (is (typep res 'cl-dplyr::grouped-tibble))
    (is (equalp #(30 3) (.pull (.ungroup res) :x)))))
