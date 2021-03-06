#|
 This file is a part of Classowary
 (c) 2019 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(asdf:defsystem classowary
  :version "1.0.0"
  :license "zlib"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
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
