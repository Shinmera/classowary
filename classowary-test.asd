#|
 This file is a part of Classowary
 (c) 2019 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(asdf:defsystem classowary-test
  :version "1.0.0"
  :license "zlib"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :description "Unit tests for Classowary"
  :homepage "https://github.com/Shinmera/classowary"
  :serial T
  :components ((:file "test"))
  :depends-on (:classowary
               :parachute)
  :perform (asdf:test-op (op c) (uiop:symbol-call :parachute :test :org.shirakumo.classowary.test)))
