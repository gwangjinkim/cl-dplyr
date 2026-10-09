(in-package #:cl-dplyr/tests)

(in-suite :cl-dplyr)

;;; Summary statistics: NA handling, types, and values checked against R.

(defparameter *agg-df*
  (tibble :g #("a" "a" "b" "b" "b")
          :x (vector 1 2 3 4 cl-vctrs-lite:*na*)
          :y #(2 4 4 4 5)
          :ok (vector t nil t t nil)))

(defun pull1 (df col) (aref (.pull df col) 0))

(test mean-is-double
  (let ((m (pull1 (.summarise *agg-df* :m (mean :y)) :m)))
    (is (typep m 'double-float))
    (is (= 3.8d0 m))))

(test na-propagates-unless-na-rm
  (is (cl-vctrs-lite:na-p (pull1 (.summarise *agg-df* :s (sum :x)) :s)))
  (is (= 10 (pull1 (.summarise *agg-df* :s (sum :x :na-rm t)) :s)))
  (is (= 2.5d0 (pull1 (.summarise *agg-df* :m (mean :x :na-rm t)) :m))))

(test logicals-count-as-0-1
  (is (= 3 (pull1 (.summarise *agg-df* :k (sum :ok)) :k)))
  (is (= 1 (pull1 (.summarise *agg-df* :k (sum (is-na :x))) :k))))

(test median-var-sd-quantile-match-r
  ;; R: median(c(2,4,4,4,5)) = 4; var = 1.2; sd = 1.095445;
  ;;    quantile(c(2,4,4,4,5), 0.25, type = 7) = 4; 0.1 -> 2.8
  (let ((r (.summarise *agg-df* :md (median :y) :v (var :y) :s (sd :y)
                                :q1 (quantile :y 0.25) :q10 (quantile :y 0.1))))
    (is (= 4.0d0 (pull1 r :md)))
    (is (< (abs (- 1.2d0 (pull1 r :v))) 1d-12))
    (is (< (abs (- 1.0954451150103321d0 (pull1 r :s))) 1d-12))
    (is (= 4.0d0 (pull1 r :q1)))
    (is (< (abs (- 2.8d0 (pull1 r :q10))) 1d-12))))

(test grouped-summaries
  (let ((r (-> *agg-df* (group-by g) (summarise n (n) m (mean y) md (median x :na-rm t)))))
    (is (equalp #(2 3) (.pull r :n)))
    (is (= 3.0d0 (aref (.pull r :m) 0)))
    (is (< (abs (- (/ 13d0 3) (aref (.pull r :m) 1))) 1d-12))
    (is (equalp #(1.5d0 3.5d0) (.pull r :md)))))

(test empty-and-short-inputs
  (is (= 0 (cl-dplyr::aggregate-sum #())))
  (is (cl-vctrs-lite:na-p (cl-dplyr::aggregate-mean #())))
  (is (cl-vctrs-lite:na-p (cl-dplyr::aggregate-var #(1)))))
