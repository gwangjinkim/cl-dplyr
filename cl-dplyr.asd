(defsystem "cl-dplyr"
  :version "0.1.0"
  :author "Antigravity"
  :license "MIT"
  :depends-on ("cl-tibble"
               "cl-vctrs-lite"
               "serapeum"
               "alexandria")
  :components ((:module "src"
                :components
                ((:file "packages")
                 (:file "generics" :depends-on ("packages"))
                 (:module "verbs"
                  :depends-on ("packages" "generics")
                  :components
                  ((:file "slice")
                   (:file "select")
                   (:file "pull")
                   (:file "rename")
                   (:file "arrange")
                   (:file "mutate")
                   (:file "filter")
                   (:file "distinct")
                   (:file "group-by")
                   (:file "ungroup")
                   (:file "summarise")
                   (:file "n-desc")
                   (:file "helpers")
                   (:file "joins")
                   (:file "dsl"))))))
  :in-order-to ((test-op (test-op "cl-dplyr/tests")))
  :description "A Grammar of Data Manipulation for Common Lisp")

(defsystem "cl-dplyr/tests"
  :depends-on ("cl-dplyr" "fiveam")
  :components ((:module "tests"
                :components
                ((:file "package")
                 (:file "main" :depends-on ("package"))
                 (:file "agent1" :depends-on ("package" "main"))
                 (:file "agent2" :depends-on ("package" "main"))
                 (:file "agent3" :depends-on ("package" "main"))
                 (:file "agent4" :depends-on ("package" "main"))
                 (:file "agent5" :depends-on ("package" "main"))
                 (:file "agent6" :depends-on ("package" "main"))
                 (:file "test-n-desc" :depends-on ("package" "main"))
                 (:file "test-helpers" :depends-on ("package" "main")))))
  :perform (test-op (op c) (symbol-call :fiveam :run! :cl-dplyr)))
