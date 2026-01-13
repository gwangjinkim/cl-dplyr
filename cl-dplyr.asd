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
                   (:file "distinct"))))))
  :description "A Grammar of Data Manipulation for Common Lisp")
