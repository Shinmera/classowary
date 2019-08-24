#|
 This file is a part of Classowary
 (c) 2019 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.classowary)

(defstruct (solver (:constructor %make-solver (&key auto-update)))
  (objective (%make-expression) :type expression)
  (variables (make-hash-table :test 'eq) :type hash-table)
  (constraints (make-hash-table :test 'eq) :type hash-table)
  (expressions (make-hash-table :test 'eq) :type hash-table)
  (infeasible-expressions () :type list)
  (dirty-variables () :type list)
  (auto-update NIL :type boolean))

(defmethod print-object ((solver solver) stream)
  (print-unreadable-object (solver stream :type T :identity T)))

(defun find-variable (symbol solver)
  (gethash symbol (solver-variables solver)))

(defun (setf find-variable) (variable symbol solver)
  (if variable
      (setf (gethash symbol (solver-variables solver)) variable)
      (remhash symbol (solver-variables solver)))
  variable)

(defmacro do-variables ((variable solver &optional result) &body body)
  `(loop for ,variable being the hash-values of (solver-variables ,solver)
         do (progn ,@body)
         finally (return ,result)))

(defun find-expression (symbol solver)
  (gethash symbol (solver-expressions solver)))

(defun (setf find-expression) (expression symbol solver)
  (if expression
      (setf (gethash symbol (solver-expressions solver)) expression)
      (remhash symbol (solver-expressions solver)))
  expression)

(defun ensure-expression (symbol solver)
  (or (gethash symbol (solver-expressions solver))
      (setf (find-expression symbol solver) (%make-expression symbol))))

(defmacro do-expressions ((expression solver &optional result) &body body)
  `(loop for ,expression being the hash-values of (solver-expressions ,solver)
         do (progn ,@body)
         finally (return ,result)))

(defun find-constraint (symbol solver)
  (gethash symbol (solver-constraints solver)))

(defun (setf find-constraint) (constraint symbol solver)
  (if constraint
      (setf (gethash symbol (solver-constraints solver)) constraint)
      (remhash symbol (solver-constraints solver)))
  constraint)

(defmacro do-constraints ((constraint solver &optional result) &body body)
  `(loop for ,constraint being the hash-values of (solver-constraints ,solver)
         do (progn ,@body)
         finally (return ,result)))

(defmethod describe-object ((solver solver) stream)
  (format stream "~&~s" solver)
  (format stream "~%Objective:~%  ")
  (describe-object (solver-objective solver) stream)
  (format stream "~%Expressions:")
  (let ((i 0))
    (do-expressions (expression solver)
      (format stream "~%~2d. " (incf i))
      (describe-object expression stream)))
  (fresh-line stream))

(defstruct (variable (:constructor %make-variable (symbol solver)))
  (symbol NIL :type symbol)
  (dirty-p NIL :type boolean)
  (constraint NIL :type (or null constraint))
  (edit-value 0f0 :type single-float)
  (value 0f0 :type single-float)
  (solver NIL :type solver)
  (use-count 1 :type (unsigned-byte 32)))

(defmethod print-object ((variable variable) stream)
  (print-unreadable-object (variable stream :type T)
    (format stream "~s ~f~:[~; DIRTY~]"
            (variable-symbol variable) (variable-value variable)
            (variable-dirty-p variable))))

(defmethod describe-object ((variable variable) stream)
  (format stream "~s ~f~@[ (~a)~]~@[ (constrained by ~s)~]~:[~; DIRTY~]"
          (variable-symbol variable)
          (variable-value variable) (variable-edit-value variable)
          (when (variable-constraint variable)
            (expression-key (constraint-expression (variable-constraint variable))))
          (variable-dirty-p variable)))

(defstruct (constraint (:constructor %make-constraint (strength solver)))
  (expression (%make-expression (mksym 'external 'constraint)) :type expression)
  (marker NIL :type symbol)
  (other NIL :type symbol)
  (relation NIL :type symbol)
  (strength 0f0 :type single-float)
  (solver NIL :type solver))

(defmethod print-object ((constraint constraint) stream)
  (print-unreadable-object (constraint stream :type T)
    (format stream "~s ~a ~s ~f"
            (constraint-marker constraint)
            (constraint-relation constraint)
            (constraint-other constraint)
            (constraint-strength constraint))))

(defmethod describe-object ((constraint constraint) stream)
  (describe-object (constraint-expression constraint) stream)
  (format stream "~%    ~s ~a ~s ~f"
          (constraint-marker constraint)
          (constraint-relation constraint)
          (constraint-other constraint)
          (constraint-strength constraint)))

(defun use-variable (variable)
  (when variable
    (incf (variable-use-count variable))))

(defun value (variable)
  (if variable
      (variable-value variable)
      0f0))

(defun make-variable (solver)
  (let ((variable (%make-variable (mksym 'external 'variable) solver)))
    (setf (find-variable (variable-symbol variable) solver) variable)))

(defun delete-variable (variable)
  (when (and variable (<= (decf (variable-use-count variable)) 0))
    (setf (find-variable (variable-symbol variable) (variable-solver variable)) NIL)
    (when (variable-constraint variable)
      (remove-constraint (variable-constraint variable)))))

(defun make-constraint (solver &key (strength :required))
  (let ((constraint (%make-constraint (->strength strength) solver)))
    (setf (find-constraint (expression-key (constraint-expression constraint)) solver) constraint)))

(defun delete-constraint (constraint)
  (when constraint
    (let ((solver (constraint-solver constraint)))
      (remove-constraint constraint)
      (setf (find-constraint (expression-key (constraint-expression constraint)) solver) NIL)
      (do-terms (term (constraint-expression constraint))
        (delete-variable (find-variable (term-key term) solver))))))

(defun clone-constraint (other &key strength)
  (when other
    (let ((constraint (make-constraint
                       (constraint-solver other)
                       :strength (if strength
                                     (->strength strength)
                                     (constraint-strength other)))))
      (merge-constraint-into constraint other 1f0)
      (setf (constraint-relation constraint) (constraint-relation other))
      constraint)))

(defun merge-constraint-into (constraint other multiplier)
  (assert (and (not (null constraint)) (not (null other))
               (null (constraint-marker constraint))
               (eq (constraint-solver constraint) (constraint-solver other)))
          () 'assertion-violated)
  (when (eq '>= (constraint-relation constraint))
    (setf multiplier (- multiplier)))
  (do-terms (term (constraint-expression other) constraint)
    (use-variable (find-variable (term-key term) (constraint-solver constraint)))
    (add-variable (constraint-expression constraint) (term-key term) (* (term-multiplier term) multiplier))))

(defun reset-constraint (constraint)
  (when constraint
    (remove-constraint constraint)
    (setf (constraint-relation constraint) NIL)
    (do-terms (term (constraint-expression constraint))
      (delete-variable (find-variable (term-key term) (constraint-solver constraint))))
    (reset-expression (constraint-expression constraint))
    constraint))

(defun add-term (constraint variable multiplier)
  (assert (and (not (null constraint)) (not (null variable))
               (null (constraint-marker constraint))
               (not (null (variable-symbol variable)))
               (eq (constraint-solver constraint) (variable-solver variable)))
          () 'assertion-violated)
  (when (eq '>= (constraint-relation constraint))
    (setf multiplier (- multiplier)))
  (add-variable (constraint-expression constraint) (variable-symbol variable) multiplier)
  (use-variable variable)
  constraint)

(defun add-constant (constraint constant)
  (assert (and (not (null constraint))
               (null (constraint-marker constraint)))
          () 'assertion-violated)
  (case (constraint-relation constraint)
    (>= (decf (expression-constant (constraint-expression constraint)) constant))
    (T (incf (expression-constant (constraint-expression constraint)) constant)))
  constraint)

(defun relation (constraint)
  (constraint-relation constraint))

(defun (setf relation) (relation constraint)
  (assert (member relation '(<= = >=)))
  (assert (and (not (null constraint))
               (null (constraint-marker constraint))
               (null (constraint-relation constraint)))
          () 'assertion-violated)
  (unless (eq relation '>=)
    (multiply (constraint-expression constraint) -1f0))
  (setf (constraint-relation constraint) relation)
  relation)
