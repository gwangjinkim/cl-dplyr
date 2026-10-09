(in-package #:cl-dplyr/tests)

(in-suite :cl-dplyr)

;;; Join result shape: column order, NA fill, suffixes, natural joins.

(defparameter *people* (tibble :id #(1 2 3) :name #("Ann" "Bo" "Cy") :age #(30 40 50)))
(defparameter *cities* (tibble :id #(1 2 4) :city #("Basel" "Bern" "Chur")))

(test join-column-order-is-keys-x-y
  (is (equalp #("id" "name" "age" "city")
              (cl-tibble:tbl-names (.left-join *people* *cities* :by :id)))))

(test join-unmatched-is-na-not-string
  (let ((res (.left-join *people* *cities* :by :id)))
    (is (cl-vctrs-lite:na-p (aref (.pull res :city) 2)))))

(test full-join-fills-both-sides-with-na
  (let ((res (.full-join *people* *cities* :by :id)))
    (is (equalp #(1 2 3 4) (.pull res :id)))
    (is (cl-vctrs-lite:na-p (aref (.pull res :name) 3)))
    (is (cl-vctrs-lite:na-p (aref (.pull res :city) 2)))))

(test join-suffixes-clashing-columns
  (let ((res (.inner-join *cities* *cities* :by :id)))
    (is (equalp #("id" "city.x" "city.y") (cl-tibble:tbl-names res))))
  (let ((res (.inner-join *cities* *cities* :by :id :suffix '("_a" "_b"))))
    (is (equalp #("id" "city_a" "city_b") (cl-tibble:tbl-names res)))))

(test natural-join-on-common-columns
  (let ((res (.inner-join *people* *cities*)))
    (is (equalp #(1 2) (.pull res :id)))
    (is (equalp #("Basel" "Bern") (.pull res :city)))))

(test join-by-string-key
  (is (equalp #(1 2) (.pull (.inner-join *people* *cities* :by "id") :id))))
