(in-package #:org.shirakumo.classowary)

(define-condition classowary-condition (condition)
  ())

(define-condition assertion-violated (classowary-condition error)
  ())

(define-condition expression-unsatisfied (classowary-condition error)
  ((solver :initarg :solver :reader solver)
   (expression :initarg :expression :reader expression)))

(define-condition expression-unbound (classowary-condition error)
  ((solver :initarg :solver :reader solver)
   (expression :initarg :expression :reader expression)))

(define-condition variable-not-suggestable (classowary-condition error)
  ((solver :initarg :solver :reader solver)
   (variable :initarg :variable :reader variable)))

(defmacro when-ok (form &body body)
  `(handler-case ,form
     (classowary-condition (e)
       (declare (ignore e)))
     (:no-error (e)
       (declare (ignore e))
       ,@body)))
