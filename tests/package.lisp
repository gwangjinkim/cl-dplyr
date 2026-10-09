(defpackage #:cl-dplyr/tests
  (:use #:cl #:fiveam #:cl-dplyr)
  ;; FiveAM and cl-dplyr both export !; the DSL's ! is what tests mean.
  (:shadowing-import-from #:cl-dplyr #:!)
  (:import-from #:cl-tibble #:tibble #:tribble))
