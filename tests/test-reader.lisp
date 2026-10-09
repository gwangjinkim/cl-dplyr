(in-package #:cl-dplyr/tests)

(in-suite :cl-dplyr)

;;; cl-dplyr installs #c and #r; standard CL syntax must keep working.

(test reader-keeps-standard-complex-syntax
  (is (eql #C(1 2) (read-from-string "#C(1 2)")))
  (is (eql (complex 1 2) (read-from-string "#c(1 2)"))))

(test reader-keeps-standard-radix-syntax
  (is (eql 10 (read-from-string "#2r1010")))
  (is (eql 1295 (read-from-string "#36rZZ"))))

(test reader-column-syntax
  (is (equal '(:col "City") (read-from-string "#c\"City\"")))
  (is (equal '(:col city) (read-from-string "#c city"))))
