#|
 This file is a part of Classowary
 (c) 2019 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.classowary)

(define-condition cassowary-condition (condition)
  ())

(define-condition assertion-violated (cassowary-condition error)
  ())

(define-condition expression-unsatisfied (cassowary-condition error)
  ((solver :initarg :solver)
   (expression :initarg :expression)))

(define-condition expression-unbound (cassowary-condition error)
  ((solver :initarg :solver)
   (expression :initarg :expression)))

(defmacro when-ok (form &body body)
  `(handler-case ,form
     (cassowary-condition (e)
       (declare (ignore e)))
     (:no-error (e)
       (declare (ignore e))
       ,@body)))
