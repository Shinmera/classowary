(asdf:defsystem classowary
  :version "1.0.0"
  :license "zlib"
  :author "Yukari Hafner <shinmera@tymoon.eu>"
  :maintainer "Yukari Hafner <shinmera@tymoon.eu>"
  :description "An implementation of the Cassowary linear constraint solver toolkit"
  :homepage "https://github.com/Shinmera/classowary"
  :serial T
  :components ((:file "package")
               (:file "toolkit")
               (:file "conditions")
               (:file "symbol")
               (:file "expression")
               (:file "constraint")
               (:file "cassowary")
               (:file "documentation"))
  :depends-on (:documentation-utils)
  :in-order-to ((asdf:test-op (asdf:test-op :classowary-test))))
