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

(defun auto-update (solver)
  (solver-auto-update solver))

(defun (setf auto-update) (value solver)
  (setf (solver-auto-update solver) value))

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
  (expression (%make-expression (mksym 'external)) :type expression)
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

(defun value (variable)
  (variable-value variable))

(defun make-variable (solver &key name strength)
  (let ((variable (%make-variable (mksym 'external name) solver)))
    (setf (find-variable (variable-symbol variable) solver) variable)
    (if strength
        (make-suggestable variable (if (eq strength T) :strong strength))
        variable)))

(defun use-variable (variable)
  (when variable
    (incf (variable-use-count variable))))

(defun unuse-variable (variable)
  (when (<= (decf (variable-use-count variable)) 0)
    (setf (find-variable (variable-symbol variable) (variable-solver variable)) NIL)
    (when (variable-constraint variable)
      (remove-constraint (variable-constraint variable)))
    T))

(defun delete-variable (variable)
  (setf (find-variable (variable-symbol variable) (variable-solver variable)) NIL)
  (when (variable-constraint variable)
    (remove-constraint (variable-constraint variable)))
  (do-expressions (expression (variable-solver variable))
    (setf (find-term (variable-symbol variable) expression) NIL)))

(defun make-constraint (solver &key (strength :required))
  (let ((constraint (%make-constraint (->strength strength) solver)))
    (setf (find-constraint (expression-key (constraint-expression constraint)) solver) constraint)))

;; FIXME: rethink what this means compared to REMOVE-CONSTRAINT
(defun delete-constraint (constraint)
  (let ((solver (constraint-solver constraint)))
    (remove-constraint constraint)
    (setf (find-constraint (expression-key (constraint-expression constraint)) solver) NIL)
    (do-terms (term (constraint-expression constraint))
      (unuse-variable (find-variable (term-key term) solver)))))

(defun clone-constraint (other &key strength)
  (let ((constraint (make-constraint
                     (constraint-solver other)
                     :strength (if strength
                                   (->strength strength)
                                   (constraint-strength other)))))
    (merge-constraint-into constraint other 1f0)
    (setf (constraint-relation constraint) (constraint-relation other))
    constraint))

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
  (remove-constraint constraint)
  (setf (constraint-relation constraint) NIL)
  (do-terms (term (constraint-expression constraint))
    (unuse-variable (find-variable (term-key term) (constraint-solver constraint))))
  (reset-expression (constraint-expression constraint))
  constraint)

(defun add-term (constraint variable multiplier)
  (assert (and (null (constraint-marker constraint))
               (not (null (variable-symbol variable)))
               (eq (constraint-solver constraint) (variable-solver variable)))
          () 'assertion-violated)
  (when (eq '>= (constraint-relation constraint))
    (setf multiplier (- multiplier)))
  (add-variable (constraint-expression constraint) (variable-symbol variable) multiplier)
  (use-variable variable)
  constraint)

(defun add-constant (constraint constant)
  (assert (null (constraint-marker constraint))
          () 'assertion-violated)
  (case (constraint-relation constraint)
    (>= (decf (expression-constant (constraint-expression constraint)) constant))
    (T (incf (expression-constant (constraint-expression constraint)) constant)))
  constraint)

(defun relation (constraint)
  (constraint-relation constraint))

(defun (setf relation) (relation constraint)
  (assert (member relation '(<= = >=)))
  (assert (and (null (constraint-marker constraint))
               (null (constraint-relation constraint)))
          () 'assertion-violated)
  (unless (eq relation '>=)
    (multiply (constraint-expression constraint) -1f0))
  (setf (constraint-relation constraint) relation)
  relation)

(defmacro with-variables (vars solver &body body)
  (let ((solverg (gensym "SOLVER")))
    `(let ((,solverg ,solver)
           ,@(loop for var in vars
                   collect (destructuring-bind (var &optional strength) (if (listp var) var (list var))
                             `(,var (make-variable ,solverg :name ,(string var)
                                                            :strength ,strength)))))
       ,@body)))

(defun extract-terms (thing mult)
  (etypecase thing
    (real
     (list (* mult thing)))
    (symbol
     (list (list thing mult)))
    (cons
     (destructuring-bind (op . args) thing
       (ecase op
         (* (destructuring-bind (a b) args
              (cond ((and (realp a) (realp b))
                     (extract-terms (* a b) mult))
                    ((realp a)
                     (extract-terms b (* a mult)))
                    (T
                     (extract-terms a (* b mult))))))
         (/ (destructuring-bind (a b) args
              (cond ((and (realp a) (realp b))
                     (extract-terms (/ a b) mult))
                    ((realp a)
                     (error "Cannot simulate the constraint ~a" thing))
                    (T
                     (extract-terms a (* b mult))))))
         (+ (loop for term in args
                  append (extract-terms term (* +1 mult))))
         (- (loop for term in args
                  append (extract-terms term (* -1 mult)))))))))

(defmacro constrain (solver constraint &rest args &key strength)
  (declare (ignore strength))
  (destructuring-bind (relation lhs rhs) constraint
    (let ((constraint (gensym "CONSTRAINT")))
      (flet ((expand-terms (terms)
               (loop for thing in (extract-terms terms 1)
                     collect (etypecase thing
                               (real `(add-constant ,constraint ,thing))
                               (symbol `(add-term ,constraint ,thing 1f0))
                               (cons (destructuring-bind (var mult) thing
                                       `(add-term ,constraint ,var ,mult)))))))
        `(let ((,constraint (make-constraint ,solver ,@args)))
           ,@(expand-terms lhs)
           (setf (relation ,constraint) ',relation)
           ,@(expand-terms rhs)
           (add-constraint ,constraint))))))
