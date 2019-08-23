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
  (setf cass::*symbol-ids* 0)
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
    (flet ((dump ()
             (format T "~&-------------------------------~%")
             (describe solver)
             (format T "~&-------------------------------~%")))
      (let ((c1 (finish (cass:make-constraint solver cass:+REQUIRED+))))
        (finish (cass:add-term c1 xl 1f0))
        (finish (setf (cass:relation c1) '>=))
        (finish (cass:add-constraint c1))
        (dump)

        (fail (setf (cass:relation c1) '>=) 'cass:assertion-violated)
        (finish (setf (cass:strength c1) (- cass:+REQUIRED+ 10)))
        (finish (setf (cass:strength c1) cass:+REQUIRED+))

        (true (cass:constrained-p c1))
        (false (cass:edited-p xl))

        (let ((c2 (finish (cass:make-constraint solver cass:+REQUIRED+))))
          (finish (cass:add-term c2 xl 1f0))
          (finish (setf (cass:relation c2) '=))
          (finish (cass:add-constraint c2))
          (dump)

          (finish (cass:reset-solver solver T))
          (finish (cass:delete-constraint c1))
          (finish (cass:delete-constraint c2))
          (dump)))
      
      (let ((c1 (finish (cass:make-constraint solver cass:+REQUIRED+))))
        (finish (cass:add-term c1 xm 2f0))
        (finish (setf (cass:relation c1) '=))
        (finish (cass:add-term c1 xl 1f0))
        (finish (cass:add-term c1 xr 1f0))
        (finish (cass:add-constraint c1))
        (dump)

        (let ((c2 (finish (cass:make-constraint solver cass:+REQUIRED+))))
          (finish (cass:add-term c2 xl 1f0))
          (finish (cass:add-constant c2 10f0))
          (finish (setf (cass:relation c2) '<=))
          (finish (cass:add-term c2 xr 1f0))
          (finish (cass:add-constraint c2))
          (dump)

          (let ((c3 (finish (cass:make-constraint solver cass:+REQUIRED+))))
            (finish (cass:add-term c3 xr 1f0))
            (finish (setf (cass:relation c3) '<=))
            (finish (cass:add-constant c3 100f0))
            (finish (cass:add-constraint c3))
            (dump)

            (let ((c4 (finish (cass:make-constraint solver cass:+REQUIRED+))))
              (finish (cass:add-term c4 xl 1f0))
              (finish (setf (cass:relation c4) '>=))
              (finish (cass:add-constant c4 0f0))
              (finish (cass:add-constraint c4))
              (dump)

              (let ((c5 (finish (cass:clone-constraint c4 cass:+REQUIRED+))))
                (finish (cass:add-constraint c5))
                (dump)
                (finish (cass:remove-constraint c5)))
              
              (let ((c5 (finish (cass:make-constraint solver cass:+REQUIRED+))))
                (finish (cass:add-term c5 xl 1f0))
                (finish (setf (cass:relation c5) '=))
                (finish (cass:add-constant c5 0f0))
                (finish (cass:add-constraint c5)))

              (let ((c6 (finish (cass:clone-constraint c4 cass:+REQUIRED+))))
                (finish (cass:add-constraint c6))
                (dump)
                (finish (cass:reset-constraint c6))
                (finish (cass:delete-constraint c6)))

              (finish (cass:remove-constraint c1))
              (finish (cass:remove-constraint c2))
              (finish (cass:remove-constraint c3))
              (finish (cass:remove-constraint c4))
              (dump)

              (finish (cass:add-constraint c4))
              (finish (cass:add-constraint c3))
              (finish (cass:add-constraint c2))
              (finish (cass:add-constraint c1))
              (finish (cass:reset-solver solver NIL))
              (finish (cass:reset-solver solver T))
              (format T "~&After Reset~%")
              (dump)
              
              (finish (cass:add-constraint c1))
              (finish (cass:add-constraint c2))
              (finish (cass:add-constraint c3))
              (finish (cass:add-constraint c4))
              (format T "~&After Initialize~%")
              (dump)
              
              (flet ((printvars ()
                       (format T "~&xl: ~f, xm: ~f, xr: ~f~%"
                               (cass:value xl) (cass:value xm) (cass:value xr))))
                (finish (cass:update-variables solver))
                (printvars)

                (finish (cass:add-edit xm cass:+MEDIUM+))
                (dump)
                (finish (cass:update-variables solver))
                (printvars)
                (true (cass:edited-p xm))

                (format T "~&suggest to 0.0~%")
                (finish (cass:suggest xm 0f0))
                (dump)
                (finish (cass:update-variables solver))
                (printvars)

                (format T "~&suggest to 70.0~%")
                (finish (cass:suggest xm 70f0))
                (finish (cass:update-variables solver))
                (dump)
                (printvars)

                (finish (cass:delete-edit xm))
                (finish (cass:update-variables solver))
                (dump)
                (printvars)))))))))
