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
      (progn
        (setf (gethash symbol (solver-variables solver)) variable)
        (when (symbolp (get symbol 'name ""))
          (setf (gethash (get symbol 'name) (solver-variables solver)) variable)))
      (let ((variable (gethash symbol (solver-variables solver))))
        (when variable
          (let ((symbol (variable-symbol variable)))
            (when (symbolp (get symbol 'name ""))
              (remhash (get symbol 'name) (solver-variables solver)))
            (remhash symbol (solver-variables solver))))))
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
      (progn
        (setf (gethash symbol (solver-constraints solver)) constraint)
        (when (symbolp (get symbol 'name ""))
          (setf (gethash (get symbol 'name) (solver-constraints solver)) constraint)))
      (let ((constraint (gethash symbol (solver-constraints solver))))
        (when constraint
          (let ((symbol (expression-key (constraint-expression constraint))))
            (when (symbolp (get symbol 'name ""))
              (remhash (get symbol 'name) (solver-constraints solver)))
            (remhash symbol (solver-constraints solver))))))
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
  (solver NIL :type solver))

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

(defmethod solver ((variable variable))
  (variable-solver variable))

(defstruct (constraint (:constructor %make-constraint (strength solver &optional expression)))
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

(defmethod solver ((constraint constraint))
  (constraint-solver constraint))

(defun value (variable)
  (variable-value variable))

(defun make-variable (solver &key name strength)
  (let ((variable (%make-variable (mksym 'external name) solver)))
    (setf (find-variable (variable-symbol variable) solver) variable)
    (if strength
        (make-suggestable variable (if (eq strength T) :strong strength))
        variable)))

(defun delete-variable (variable)
  (setf (find-variable (variable-symbol variable) (variable-solver variable)) NIL)
  (when (variable-constraint variable)
    (remove-constraint (variable-constraint variable)))
  (do-expressions (expression (variable-solver variable) T)
    (setf (find-term (variable-symbol variable) expression) NIL)))

(defun make-constraint (solver &key name (strength :required))
  (let ((constraint (%make-constraint (->strength strength) solver
                                      (%make-expression (mksym 'external name)))))
    (setf (find-constraint (expression-key (constraint-expression constraint)) solver) constraint)))

(defun delete-constraint (constraint)
  (remove-constraint constraint)
  (setf (find-constraint (expression-key (constraint-expression constraint))
                         (constraint-solver constraint))
        NIL))

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
    (add-variable (constraint-expression constraint) (term-key term) (* (term-multiplier term) multiplier))))

(defun reset-constraint (constraint)
  (remove-constraint constraint)
  (setf (constraint-relation constraint) NIL)
  (reset-expression (constraint-expression constraint))
  constraint)

(defun remove-term (constraint multiplier &optional (variable NIL v-p))
  (if v-p
      (add-variable-term constraint variable (- multiplier))
      (add-constant constraint (- multiplier))))

(define-compiler-macro remove-term (constraint multiplier &optional variable)
  (if variable
      `(add-variable-term ,constraint ,variable (- ,multiplier))
      `(add-constant ,constraint (- ,multiplier))))

(defun add-term (constraint multiplier &optional (variable NIL v-p))
  (if v-p
      (add-variable-term constraint variable multiplier)
      (add-constant constraint multiplier)))

(define-compiler-macro add-term (constraint multiplier &optional variable)
  (if variable
      `(add-variable-term ,constraint ,variable ,multiplier)
      `(add-constant ,constraint ,multiplier)))

(defun add-variable-term (constraint variable multiplier)
  (assert (and (null (constraint-marker constraint))
               (not (null (variable-symbol variable)))
               (eq (constraint-solver constraint) (variable-solver variable)))
          () 'assertion-violated)
  (when (eq '>= (constraint-relation constraint))
    (setf multiplier (- multiplier)))
  (add-variable (constraint-expression constraint) (variable-symbol variable) multiplier)
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
    `(let* ((,solverg ,solver)
            ,@(loop for var in vars
                    collect (destructuring-bind (var &rest args) (if (listp var) var (list var))
                              (let ((name var) (strength NIL))
                                (ecase (length args)
                                  (0)
                                  (1 (etypecase (first args)
                                       ((and symbol (not keyword)) (setf name (first args)))
                                       (T (setf strength (first args)))))
                                  (2 (setf name (first args))
                                   (setf strength (second args))))
                                `(,var (or (find-variable ',name ,solverg)
                                           (make-variable ,solverg :name ',name
                                                                   :strength ,strength)))))))
       ,@body)))

(defun reduce-expression (thing)
  (etypecase thing
    (real
     (list (float thing 0f0)))
    (symbol
     (list 0f0 (list thing 1f0)))
    (cons
     (destructuring-bind (op . args) thing
       (ecase op
         (quote (list 0f0 (list thing 1f0)))
         (* (let ((vars NIL) (const 1f0))
              (dolist (arg args)
                (destructuring-bind (inner . terms) (reduce-expression arg)
                  (if terms
                      (if vars
                          (error "This is not a linear expression.~%  ~a" thing)
                          (setf vars (cons inner terms)))
                      (setf const (* const inner)))))
              (list* (* const (or (car vars) 1f0))
                     (loop for (var mult) in (rest vars)
                           collect (list var (* mult const))))))
         (/ (destructuring-bind (a &rest rest) args
              (destructuring-bind (aconst . avars) (reduce-expression a)
                (destructuring-bind (const . vars) (reduce-expression (list* '+ rest))
                  (when vars
                    (error "This is not a linear expression.~%  ~a" thing))
                  (append (list (/ aconst const))
                          (loop for (var mult) in avars
                                collect (list var (/ mult const))))))))
         (+ (let ((vars ()) (const 0f0))
              (dolist (arg args)
                (destructuring-bind (inner . terms) (reduce-expression arg)
                  (incf const inner)
                  (setf vars (append vars terms))))
              (list* const vars)))
         (- (destructuring-bind (a &rest rest) args
              (destructuring-bind (aconst . avars) (reduce-expression a)
                (destructuring-bind (const . vars) (reduce-expression (list* '+ rest))
                  (append (list (- aconst const))
                          avars
                          (loop for (var mult) in vars
                                collect (list var (- mult)))))))))))))

(defmacro constrain (solver constraint &rest args &key name strength)
  (declare (ignore name strength))
  (destructuring-bind (relation lhs rhs) constraint
    (let ((solverg (gensym "SOLVERG"))
          (constraint (gensym "CONSTRAINT")))
      (labels ((expand-terms (terms)
                 (destructuring-bind (const . terms) (reduce-expression terms)
                   (list* `(add-constant ,constraint ,const)
                          (loop for (var mult) in terms
                                for variable = (if (consp var)
                                                   `(or (find-variable ,(second var) ,solverg)
                                                        (make-variable ,solverg :name ',(second var)))
                                                   var)
                                collect `(add-variable-term ,constraint ,variable ,mult))))))
        `(let* ((,solverg ,solver)
                (,constraint (make-constraint ,solverg ,@args)))
           ,@(expand-terms lhs)
           (setf (relation ,constraint) ',relation)
           ,@(expand-terms rhs)
           (add-constraint ,constraint))))))
