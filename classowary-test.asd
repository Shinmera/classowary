(asdf:defsystem classowary-test
  :version "1.0.0"
  :license "zlib"
  :author "Yukari Hafner <shinmera@tymoon.eu>"
  :maintainer "Yukari Hafner <shinmera@tymoon.eu>"
  :description "Unit tests for Classowary"
  :homepage "https://shinmera.com/project/classowary"
  :serial T
  :components ((:file "test"))
  :depends-on (:classowary
               :parachute)
  :perform (asdf:test-op (op c) (uiop:symbol-call :parachute :test :org.shirakumo.classowary.test)))
