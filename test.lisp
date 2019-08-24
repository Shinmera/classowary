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
    (let ((c1 (finish (cass:make-constraint solver))))
      (finish (cass:add-term c1 xl 1f0))
      (finish (setf (cass:relation c1) '>=))
      (finish (cass:add-constraint c1))

      (fail (setf (cass:relation c1) '>=) 'cass:assertion-violated)
      (finish (setf (cass:strength c1) (- cass:+REQUIRED+ 10)))
      (finish (setf (cass:strength c1) :required))

      (true (cass:constrained-p c1))
      (false (cass:edited-p xl))

      (let ((c2 (finish (cass:make-constraint solver))))
        (finish (cass:add-term c2 xl 1f0))
        (finish (setf (cass:relation c2) '=))
        (finish (cass:add-constraint c2))

        (finish (cass:reset-solver solver T))
        (finish (cass:delete-constraint c1))
        (finish (cass:delete-constraint c2))))
    
    (let ((c1 (finish (cass:make-constraint solver))))
      (finish (cass:add-term c1 xm 2f0))
      (finish (setf (cass:relation c1) '=))
      (finish (cass:add-term c1 xl 1f0))
      (finish (cass:add-term c1 xr 1f0))
      (finish (cass:add-constraint c1))

      (let ((c2 (finish (cass:make-constraint solver))))
        (finish (cass:add-term c2 xl 1f0))
        (finish (cass:add-constant c2 10f0))
        (finish (setf (cass:relation c2) '<=))
        (finish (cass:add-term c2 xr 1f0))
        (finish (cass:add-constraint c2))

        (let ((c3 (finish (cass:make-constraint solver))))
          (finish (cass:add-term c3 xr 1f0))
          (finish (setf (cass:relation c3) '<=))
          (finish (cass:add-constant c3 100f0))
          (finish (cass:add-constraint c3))

          (let ((c4 (finish (cass:make-constraint solver))))
            (finish (cass:add-term c4 xl 1f0))
            (finish (setf (cass:relation c4) '>=))
            (finish (cass:add-constant c4 0f0))
            (finish (cass:add-constraint c4))

            (let ((c5 (finish (cass:clone-constraint c4 :strength :required))))
              (finish (cass:add-constraint c5))
              (finish (cass:remove-constraint c5)))
            
            (let ((c5 (finish (cass:make-constraint solver))))
              (finish (cass:add-term c5 xl 1f0))
              (finish (setf (cass:relation c5) '=))
              (finish (cass:add-constant c5 0f0))
              (finish (cass:add-constraint c5)))

            (let ((c6 (finish (cass:clone-constraint c4 :strength :required))))
              (finish (cass:add-constraint c6))
              (finish (cass:reset-constraint c6))
              (finish (cass:delete-constraint c6)))

            (finish (cass:remove-constraint c1))
            (finish (cass:remove-constraint c2))
            (finish (cass:remove-constraint c3))
            (finish (cass:remove-constraint c4))

            (finish (cass:add-constraint c4))
            (finish (cass:add-constraint c3))
            (finish (cass:add-constraint c2))
            (finish (cass:add-constraint c1))
            (finish (cass:reset-solver solver NIL))
            (finish (cass:reset-solver solver T))
            
            (finish (cass:add-constraint c1))
            (finish (cass:add-constraint c2))
            (finish (cass:add-constraint c3))
            (finish (cass:add-constraint c4))
            
            (macrolet ((check-variables (l m r)
                         `(progn
                            (is = ,l (cass:value xl))
                            (is = ,m (cass:value xm))
                            (is = ,r (cass:value xr)))))
              (finish (cass:update-variables solver))
              (check-variables 90 95 100)

              (finish (cass:add-edit xm cass:+MEDIUM+))
              (finish (cass:update-variables solver))
              (check-variables 90 95 100)
              (true (cass:edited-p xm))

              (finish (cass:suggest xm 0f0))
              (finish (cass:update-variables solver))
              (check-variables 0 5 10)

              (finish (cass:suggest xm 70f0))
              (finish (cass:update-variables solver))
              (check-variables 65 70 75)

              (finish (cass:delete-edit xm))
              (finish (cass:update-variables solver))
              (check-variables 90 95 100))))))))

(defun make-constraint (solver strength term1 factor1 relation constant &rest vars)
  (let ((constraint (cass:make-constraint solver :strength strength)))
    (cass:add-term constraint term1 factor1)
    (setf (cass:relation constraint) relation)
    (when constant
      (cass:add-constant constraint constant))
    (loop for (var factor) on vars by #'cddr
          do (cass:add-term constraint var factor))
    (cass:add-constraint constraint)))

(define-test binary-tree
  :parent classowary
  :depends-on (all)
  (let* ((solver (cass:make-solver))
         (current-row-points-count 1)
         (current-row-first-point-index 0)
         (arrx (make-array 1024))
         (arry (make-array 1024)))
    (setf (aref arrx 0) (cass:make-variable solver))
    (setf (aref arry 0) (cass:make-variable solver))
    (cass:add-edit (aref arrx 0) cass:+STRONG+)
    (cass:add-edit (aref arry 0) cass:+STRONG+)
    (cass:suggest (aref arrx 0) 500f0)
    (cass:suggest (aref arry 0) 10f0)

    (loop for row from 1 below 9
          for previous-row-first-point-index = current-row-first-point-index
          for point = 0 and parent-point = 0
          do (incf current-row-first-point-index current-row-points-count)
             (setf current-row-points-count (* 2 current-row-points-count))
             (loop for point from 0 below current-row-points-count
                   do (setf (aref arrx (+ current-row-first-point-index point)) (cass:make-variable solver))
                      (setf (aref arry (+ current-row-first-point-index point)) (cass:make-variable solver))
                      (let ((pc (cass:make-constraint solver)))
                        (cass:add-term pc (aref arry (+ current-row-first-point-index point)) 1f0)
                        (setf (cass:relation pc) '=)
                        (cass:add-term pc (aref arry (- current-row-first-point-index 1)) 1f0)
                        (cass:add-constant pc 15f0)
                        (finish (cass:add-constraint pc)))
                      (cond ((< 0 point)
                             (let ((pc (cass:make-constraint solver)))
                               (cass:add-term pc (aref arrx (+ current-row-first-point-index point)) 1f0)
                               (setf (cass:relation pc) '>=)
                               (cass:add-term pc (aref arrx (+ current-row-first-point-index point -1)) 1f0)
                               (cass:add-constant pc 5f0)
                               (finish (cass:add-constraint pc))))
                            (T
                             (let ((pc (cass:make-constraint solver)))
                               (cass:add-term pc (aref arrx (+ current-row-first-point-index point)) 1f0)
                               (setf (cass:relation pc) '>=)
                               (cass:add-constant pc 0f0)
                               (finish (cass:add-constraint pc)))))
                      (when (= 1 (mod point 2))
                        (let ((pc (cass:make-constraint solver)))
                          (cass:add-term pc (aref arrx (+ previous-row-first-point-index parent-point)) 1f0)
                          (setf (cass:relation pc) '=)
                          (cass:add-term pc (aref arrx (+ current-row-first-point-index point)) 0.5f0)
                          (cass:add-term pc (aref arrx (+ current-row-first-point-index point -1)) 0.5f0)
                          (finish (cass:add-constraint pc))
                          (incf parent-point)))))
    ;; (loop for i from 0 below (+ current-row-first-point-index current-row-points-count)
    ;;       do (format T "Point ~d: (~f, ~f)~%" i (cass:value (aref arrx i)) (cass:value (aref arry i))))
    ))

(define-test unbound
  :parent classowary
  (let* ((solver (cass:make-solver))
         (x (cass:make-variable solver))
         (y (cass:make-variable solver)))
    (let ((c (cass:make-constraint solver)))
      (cass:add-constant c 10)
      (setf (cass:relation c) '=)
      (fail (cass:add-constraint c) 'cass:expression-unsatisfied))
    (let ((c (cass:make-constraint solver)))
      (cass:add-constant c 0)
      (setf (cass:relation c) '=)
      (finish (cass:add-constraint c)))
    (cass:reset-solver solver T)
    (let ((c (cass:make-constraint solver)))
      (cass:add-term c x 1)
      (setf (cass:relation c) '>=)
      (cass:add-constant c 10)
      (finish (cass:add-constraint c)))
    (let ((c (cass:make-constraint solver)))
      (cass:add-term c x 1)
      (setf (cass:relation c) '=)
      (cass:add-term c y 2)
      (finish (cass:add-constraint c)))
    (let ((c (cass:make-constraint solver)))
      (cass:add-term c y 1)
      (setf (cass:relation c) '=)
      (cass:add-term c x 3)
      (fail (cass:add-constraint c) 'cass:expression-unbound))
    (cass:reset-solver solver T)
    (let ((c (cass:make-constraint solver)))
      (cass:add-term c x 1)
      (setf (cass:relation c) '>=)
      (cass:add-constant c 10)
      (finish (cass:add-constraint c)))
    (let ((c (cass:make-constraint solver)))
      (cass:add-term c x 1)
      (setf (cass:relation c) '<=)
      (fail (cass:add-constraint c) 'cass:expression-unbound)
      (is = 10 (cass:value x)))
    (cass:reset-solver solver T)
    (let ((c (cass:make-constraint solver)))
      (cass:add-term c x 1)
      (setf (cass:relation c) '=)
      (cass:add-constant c 10)
      (finish (cass:add-constraint c)))
    (let ((c (cass:make-constraint solver)))
      (cass:add-term c x 1)
      (setf (cass:relation c) '=)
      (cass:add-constant c 20)
      (fail (cass:add-constraint c) 'cass:expression-unsatisfied))
    (let ((c (cass:make-constraint solver)))
      (cass:add-term c x 1)
      (setf (cass:relation c) '=)
      (cass:add-constant c 10)
      (finish (cass:add-constraint c)))))

(define-test strength
  :parent classowary
  (let* ((solver (cass:make-solver :auto-update T))
         (x (cass:make-variable solver))
         (y (cass:make-variable solver)))
    (make-constraint solver :strong x 1 '<= 0 y 1)
    (make-constraint solver :medium x 1 '= 50)
    (let ((c (make-constraint solver (- cass:+MEDIUM+ 10) y 1 '= 40)))
      (is = 50 (cass:value x))
      (is = 50 (cass:value y))
      (setf (cass:strength c) (+ cass:+MEDIUM+ 10))
      (is = 40 (cass:value x))
      (is = 40 (cass:value y))
      (setf (cass:strength c) (- cass:+MEDIUM+ 10))
      (is = 50 (cass:value x))
      (is = 50 (cass:value y)))))

(define-test suggest
  :parent classowary
  (let* ((strength1 cass:+REQUIRED+)
         (strength2 cass:+REQUIRED+)
         (width 76)
         (delta 0)
         (pos)
         (solver (cass:make-solver))
         (splitter-l (cass:make-variable solver))
         (splitter-w (cass:make-variable solver))
         (splitter-r (cass:make-variable solver))
         (left-child-l (cass:make-variable solver))
         (left-child-w (cass:make-variable solver))
         (left-child-r (cass:make-variable solver))
         (splitter-bar-l (cass:make-variable solver))
         (splitter-bar-w (cass:make-variable solver))
         (splitter-bar-r (cass:make-variable solver))
         (right-child-l (cass:make-variable solver))
         (right-child-w (cass:make-variable solver))
         (right-child-r (cass:make-variable solver)))
    (make-constraint solver :required splitter-r 1 '= 0
                     splitter-l 1 splitter-w 1)
    (make-constraint solver :required left-child-r 1 '= 0
                     left-child-l 1 left-child-w 1)
    (make-constraint solver :required splitter-bar-r 1 '= 0
                     splitter-bar-l 1 splitter-bar-w 1)
    (make-constraint solver :required right-child-r 1 '= 0
                     right-child-l 1 right-child-w 1)
    
    (make-constraint solver :required splitter-bar-w 1 '= 6)
    (make-constraint solver :required splitter-bar-l 1 '>=
                     delta splitter-l 1)
    (make-constraint solver :required splitter-bar-r 1 '<=
                     (- delta) splitter-r 1)
    (make-constraint solver :required left-child-r 1 '= 0
                     splitter-bar-l 1)
    (make-constraint solver :required right-child-l 1 '= 0
                     splitter-bar-r 1)

    (make-constraint solver strength1 right-child-r 1 '>= 1
                     splitter-r 1)
    (make-constraint solver strength2 left-child-w 1 '= 256)

    (make-constraint solver :required splitter-l 1 '= 0)
    (make-constraint solver :required splitter-r 1 '= width)

    (loop for pos from -10 below 86
          do (cass:suggest splitter-bar-l pos)
             (cass:update-variables solver)
             (format T "pos: ~4d | splitter_l l=~2d, w=~2d, r=~2d ~
                                 | left_child_l l=~2d, w=~2d, r=~2d ~
                                 | splitter_bar_l l=~2d, w=~2d, r=~2d ~
                                 | right_child_l l=~2d, w=~2d, r=~2d | ~%"
                     pos
                     (round (cass:value splitter-l)) (round (cass:value splitter-w)) (round (cass:value splitter-r))
                     (round (cass:value left-child-l)) (round (cass:value left-child-w)) (round (cass:value left-child-r))
                     (round (cass:value splitter-bar-l)) (round (cass:value splitter-bar-w)) (round (cass:value splitter-bar-r))
                     (round (cass:value right-child-l)) (round (cass:value right-child-w)) (round (cass:value right-child-r))))))

(define-test cycling
  :parent classowary
  (let* ((solver (cass:make-solver))
         (va (cass:make-variable solver))
         (vb (cass:make-variable solver))
         (vc (cass:make-variable solver))
         (vd (cass:make-variable solver)))
    (cass:add-edit va :strong)
    (let ((c (cass:make-constraint solver)))
      (cass:add-term c vb 1)
      (setf (cass:relation c) '=)
      (cass:add-term c va 1)
      (finish (cass:add-constraint c)))
    (let ((c (cass:make-constraint solver)))
      (cass:add-term c vb 1)
      (setf (cass:relation c) '=)
      (cass:add-term c vc 1)
      (finish (cass:add-constraint c)))
    (let ((c (cass:make-constraint solver)))
      (cass:add-term c vc 1)
      (setf (cass:relation c) '=)
      (cass:add-term c vd 1)
      (finish (cass:add-constraint c)))
    (let ((c (cass:make-constraint solver)))
      (cass:add-term c vd 1)
      (setf (cass:relation c) '=)
      (cass:add-term c va 1)
      (finish (cass:add-constraint c)))
    (is = 0 (cass:value va))
    (is = 0 (cass:value vb))
    (is = 0 (cass:value vc))
    (is = 0 (cass:value vd))))
