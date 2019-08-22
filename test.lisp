#|
 This file is a part of Classowary
 (c) 2019 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(defpackage #:org.shirakumo.classowary.test
  (:use #:cl #:parachute)
  (:local-nicknames
   (#:cass #:org.shirakumo.classowary)))
(in-package #:org.shirakumo.classowary.test)

(define-test classowary)

(define-test all
  :parent classowary
  (let* ((solver (finish (cass:make-solver)))
         (xl (finish (cass:make-variable solver)))
         (xm (finish (cass:make-variable solver)))
         (xr (finish (cass:make-variable solver))))
    (false (cass:constrained-p NIL))
    (false (cass:edited-p NIL))
    (false (cass:edited-p xl))
    (false (cass:edited-p xm))
    (false (cass:edited-p xr))
    (finish (cass:delete-variable (cass:make-variable solver)))
    (fail (setf (cass:relation NIL) '>=) 'cass:assertion-violated)
    (let ((c1 (finish (cass:make-constraint solver cass:+REQUIRED+))))
      (finish (cass:add-term c1 xl 1f0))
      (finish (setf (cass:relation c1) '>=))
      (finish (cass:add-constraint c1))
      (describe solver)

      (fail (setf (cass:relation c1) '>=) 'cass:assertion-violated)
      (finish (setf (cass:strength c1) (- cass:+REQUIRED+ 10)))
      (finish (setf (cass:strength c1) cass:+REQUIRED+))

      (true (cass:constrained-p c1))
      (false (cass:edited-p xl))

      (let ((c2 (finish (cass:make-constraint solver cass:+REQUIRED+))))
        (finish (cass:add-term c2 xl 1f0))
        (finish (setf (cass:relation c2) '=))
        (finish (cass:add-constraint c2))
        (describe solver)

        (finish (cass:reset-solver solver T))
        (finish (cass:delete-constraint c1))
        (finish (cass:delete-constraint c2))
        (describe solver)))))
