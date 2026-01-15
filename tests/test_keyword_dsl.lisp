(ql:quickload :cl-dplyr)

(defpackage #:natural-dsl-test
  (:use #:cl #:cl-dplyr))

(in-package #:natural-dsl-test)

(defparameter *df*
  (cl-tibble:tibble :name #("A" "B" "C") :revenue #(100 200 300) :age #(20 30 40)))

(format t "--- Testing Natural & Zero-Conflict DSL ---~%")

;; 1. Natural Symbol-Based Syntax (No quotes, no keywords needed!)
;; This matches R's summarise(total = sum(revenue), avg = mean(age), count = n())
(format t "Testing Natural Symbol-Based Syntax...~%")
(defparameter *res_natural*
  (-> *df*
      (summarise total (sum revenue)
                 avg (mean age)
                 n (n))))
(print *res_natural*)

;; 2. Mixed Notation (Keywords for columns, dot-prefix for functions)
(format t "Testing Mixed Notation...~%")
(defparameter *res_mixed*
  (-> *df*
      (summarise :total (.sum :revenue)
                 :avg (.mean :age)
                 :count (.n))))
(print *res_mixed*)

;; 3. Keyword-Only Notation (Safest for package discipline)
(format t "Testing Keyword-Only Notation...~%")
(defparameter *res_keywords*
  (-> *df*
      (summarise :total (:sum :revenue)
                 :avg (:mean :age)
                 :count (:n))))
(print *res_keywords*)

;; 4. Verify no conflicts with CL:MIN/MAX
(format t "Verifying no conflicts with cl:min/max...~%")
(defparameter *res_conflict_check*
  (-> *df*
      (summarise min_age (min age)   ; min is cl:min in this package
                 max_rev (max revenue)))) ; max is cl:max
(print *res_conflict_check*)

(format t "--- Natural DSL Verification Complete ---~%")
