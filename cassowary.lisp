(in-package #:org.shirakumo.classowary)

(defmacro with-protection (unwind &body protected)
  `(unwind-protect (progn ,@protected)
     ,unwind))

(defun suggestable-p (variable)
  (variable-constraint variable))

(defun constrained-p (constraint)
  (constraint-marker constraint))

(defun mark-infeasible (solver expression)
  (unless (expression-infeasible-p expression)
    (setf (expression-infeasible-p expression) T)
    (push expression (solver-infeasible-expressions solver))
    solver))

(defun mark-dirty (solver variable)
  (unless (variable-dirty-p variable)
    (setf (variable-dirty-p variable) T)
    (push variable (solver-dirty-variables solver))
    solver))

(defun substitute-expressions (solver variable to-substitute)
  (do-expressions (expression solver)
    (substitute expression variable to-substitute)
    (cond ((external-p (expression-key expression))
           (mark-dirty solver (find-variable (expression-key expression) solver)))
          ((< (expression-constant expression) 0f0)
           (mark-infeasible solver expression))))
  (substitute (solver-objective solver) variable to-substitute))

(defun get-expression (solver symbol destination)
  (let ((expression (find-expression symbol solver)))
    (setf (expression-key destination) NIL)
    (assert (not (null expression))
            () 'assertion-violated)
    (setf (find-expression symbol solver) NIL)
    (setf (expression-constant destination) (expression-constant expression))
    (setf (expression-terms destination) (expression-terms expression))
    destination))

(defun put-expression (solver symbol source)
  (let ((expression (ensure-expression symbol solver)))
    (setf (expression-constant expression) (expression-constant source))
    (setf (expression-terms expression) (expression-terms source))
    expression))

(defun merge-expression (solver expression variable multiplier)
  (let ((old-expression (find-expression variable solver)))
    (if old-expression
        (add-expression expression old-expression multiplier)
        (add-variable expression variable multiplier))))

(defun optimize-for (objective solver)
  (loop (let ((min-ratio MOST-POSITIVE-SINGLE-FLOAT)
              (tmp (%make-expression))
              (enter NIL)
              (exit NIL))
          (assert (null (solver-infeasible-expressions solver)))
          (do-terms (term objective)
            (when (and (not (dummy-p (term-key term)))
                       (< (term-multiplier term) 0f0))
              (setf enter (term-key term))
              (return)))
          (unless enter (return))

          (do-expressions (expression solver)
            (let ((term (find-term enter expression)))
              (unless (or (null term)
                          (not (pivotable-p (expression-key expression)))
                          (< 0f0 (term-multiplier term)))
                (let ((r (/ (- (expression-constant expression))
                            (term-multiplier term))))
                  (when (or (< r min-ratio)
                            (and (~= r min-ratio)
                                 (< (get (expression-key expression) 'id)
                                    (get exit 'id))))
                    (setf min-ratio r)
                    (setf exit (expression-key expression)))))))
          (assert (not (null exit))
                  () 'assertion-violated)

          (get-expression solver exit tmp)
          (solve-for tmp enter exit)
          (substitute-expressions solver enter tmp)
          (unless (eq objective (solver-objective solver))
            (substitute objective enter tmp))
          (put-expression solver enter tmp))))

(defun make-expression (solver constraint)
  (let ((expression (%make-expression)))
    (setf (expression-constant expression) (expression-constant (constraint-expression constraint)))
    (do-terms (term (constraint-expression constraint))
      (mark-dirty solver (find-variable (term-key term) solver))
      (merge-expression solver expression (term-key term) (term-multiplier term)))
    (macrolet ((initsymbol (place type)
                 `(unless ,place
                    (setf ,place (mksym ,type)))))
      (cond ((not (eq '= (constraint-relation constraint)))
             (initsymbol (constraint-marker constraint) 'slack)
             (add-variable expression (constraint-marker constraint) -1f0)
             (when (< (constraint-strength constraint) +REQUIRED+)
               (initsymbol (constraint-other constraint) 'error)
               (add-variable expression (constraint-other constraint) 1f0)
               (add-variable (solver-objective solver) (constraint-other constraint) (constraint-strength constraint))))
            ((<= +REQUIRED+ (constraint-strength constraint))
             (initsymbol (constraint-marker constraint) 'dummy)
             (add-variable expression (constraint-marker constraint) 1f0))
            (T
             (initsymbol (constraint-marker constraint) 'error)
             (initsymbol (constraint-other constraint) 'error)
             (add-variable expression (constraint-marker constraint) -1f0)
             (add-variable expression (constraint-other constraint) 1f0)
             (add-variable (solver-objective solver) (constraint-marker constraint) (constraint-strength constraint))
             (add-variable (solver-objective solver) (constraint-other constraint) (constraint-strength constraint)))))
    (when (< (expression-constant expression) 0f0)
      (multiply expression -1))
    expression))

(defun remove-errors (solver constraint)
  (when (error-p (constraint-marker constraint))
    (merge-expression solver (solver-objective solver) (constraint-marker constraint) (- (constraint-strength constraint))))
  (when (error-p (constraint-other constraint))
    (merge-expression solver (solver-objective solver) (constraint-other constraint) (- (constraint-strength constraint))))
  (when (constant-p (solver-objective solver))
    (setf (expression-constant (solver-objective solver)) 0f0))
  (setf (constraint-marker constraint) NIL)
  (setf (constraint-other constraint) NIL)
  constraint)

(defun add-with-artificial (solver expression constraint)
  (let ((a (mksym 'slack))
        (tmp (%make-expression)))
    (decf *symbol-ids*)
    (with-protection (setf (find-expression a solver) NIL)
      (add-expression tmp expression 1f0)
      (put-expression solver a expression)
      ;; KLUDGE: not sure if this is necessary, but it seems to me like it should be.
      (setf (solver-infeasible-expressions solver)
            (delete expression (solver-infeasible-expressions solver) :test #'eq))
      (optimize-for tmp solver)
      (let ((state (unless (~zerop (expression-constant tmp))
                     (make-condition 'expression-unbound :expression tmp :solver solver))))
        (macrolet ((ret ()
                     `(if state
                          (signal state)
                          (return-from add-with-artificial T))))
          (when-ok (get-expression solver a tmp)
            (when (constant-p tmp)
              (ret))
            (let ((entry (do-terms (term tmp)
                           (when (pivotable-p (term-key term))
                             (return (term-key term))))))
              (unless entry
                (error 'expression-unbound :expression tmp :solver solver))
              (solve-for tmp entry a)
              (substitute-expressions solver entry tmp)
              (put-expression solver entry tmp)))
          (do-expressions (expression solver)
            (let ((term (find-term a expression)))
              (when term
                (setf (find-term (term-key term) expression) NIL))))
          (let ((term (find-term a (solver-objective solver))))
            (when term (setf (find-term (term-key term) (solver-objective solver)) NIL))
            (when state (remove-constraint constraint))
            (ret)))))))

(defun try-add-expression (solver expression constraint)
  (let ((subject (do-terms (term expression)
                   (when (external-p (term-key term))
                     (return (term-key term))))))
    (when (and (null subject) (pivotable-p (constraint-marker constraint))
               (< (term-multiplier (find-term (constraint-marker constraint) expression)) 0f0))
      (setf subject (constraint-marker constraint)))
    (when (and (null subject) (pivotable-p (constraint-other constraint))
               (< (term-multiplier (find-term (constraint-other constraint) expression)) 0f0))
      (setf subject (constraint-other constraint)))
    (when (null subject)
      (unless (do-terms (term expression)
                (unless (dummy-p (term-key term))
                  (return term)))
        (cond ((~zerop (expression-constant expression))
               (setf subject (constraint-marker constraint)))
              (T
               (clrhash (expression-terms expression))
               (error 'expression-unsatisfied :expression expression :solver solver)))))
    (cond ((null subject)
           (add-with-artificial solver expression constraint))
          (T
           (solve-for expression subject NIL)
           (substitute-expressions solver subject expression)
           (put-expression solver subject expression)
           expression))))

(defun get-leaving-expression (solver marker)
  (let ((r1 MOST-POSITIVE-SINGLE-FLOAT)
        (r2 MOST-POSITIVE-SINGLE-FLOAT)
        first second third)
    (do-expressions (expression solver (or first second third))
      (let ((term (find-term marker expression)))
        (when term
          (cond ((external-p (expression-key expression))
                 (setf third (expression-key expression)))
                ((< (term-multiplier term) 0f0)
                 (let ((r (/ (- (expression-constant expression))
                             (term-multiplier term))))
                   (when (< r r1)
                     (setf r1 r first (expression-key expression)))))
                (T
                 (let ((r (/ (- (expression-constant expression))
                             (term-multiplier term))))
                   (when (< r r2)
                     (setf r2 r second (expression-key expression)))))))))))

(defun delta-edit-constant (solver delta constraint)
  (let (expression)
    (cond ((setf expression (find-expression (constraint-marker constraint) solver))
           (when (< (decf (expression-constant expression) delta) 0f0)
             (mark-infeasible solver expression)))
          ((setf expression (find-expression (constraint-other constraint) solver))
           (when (< (incf (expression-constant expression) 0f0))
             (mark-infeasible solver expression)))
          (T
           (do-expressions (expression solver)
             (let ((term (find-term (constraint-marker constraint) expression)))
               (when term
                 (incf (expression-constant expression) (* (term-multiplier term) delta))
                 (cond ((external-p (expression-key expression))
                        (mark-dirty solver (find-variable (expression-key expression) solver)))
                       ((< (expression-constant expression) 0f0)
                        (mark-infeasible solver expression))))))))))

(defun dual-optimize (solver)
  (loop with tmp = (%make-expression)
        for expression = (pop (solver-infeasible-expressions solver))
        while expression
        do (let ((min-ratio MOST-POSITIVE-SINGLE-FLOAT)
                 (enter NIL)
                 (exit (expression-key expression)))
             (setf (expression-infeasible-p expression) NIL)
             (when (< (expression-constant expression) 0f0)
               (do-terms (term expression)
                 (unless (or (dummy-p (term-key term))
                             (<= (term-multiplier term) 0f0))
                   (let* ((objective-term (find-term (term-key term) (solver-objective solver)))
                          (r (if objective-term
                                 (/ (term-multiplier objective-term)
                                    (term-multiplier term))
                                 0f0)))
                     (when (<= r min-ratio)
                       (setf min-ratio r
                             enter (term-key term))))))
               (assert (not (null enter)))
               (get-expression solver exit tmp)
               (solve-for tmp enter exit)
               (substitute-expressions solver enter tmp)
               (put-expression solver enter tmp)))))

(defun make-solver (&key auto-update)
  (%make-solver :auto-update auto-update))

(defun reset-solver (solver &optional clear-constraints)
  (unless (solver-auto-update solver)
    (update-variables solver))
  (do-variables (variable solver)
    (when (variable-constraint variable)
      (remove-constraint (variable-constraint variable)))
    (setf (variable-constraint variable) NIL))
  (assert (~zerop (expression-constant (solver-objective solver))))
  (assert (null (solver-infeasible-expressions solver)))
  (assert (null (solver-dirty-variables solver)))
  (when clear-constraints
    (reset-expression (solver-objective solver))
    (do-constraints (constraint solver)
      (when (constraint-marker constraint)
        (setf (constraint-marker constraint) NIL)
        (setf (constraint-other constraint) NIL)))
    (do-expressions (expression solver)
      (remhash (expression-key expression) (solver-expressions solver))))
  solver)

(defun update-variables (solver)
  (loop for variable = (pop (solver-dirty-variables solver))
        while variable
        do (let ((expression (find-expression (variable-symbol variable) solver)))
             (setf (variable-dirty-p variable) NIL)
             (setf (variable-value variable) (if expression (expression-constant expression) 0f0))))
  solver)

(defun add-constraint (constraint)
  (assert (null (constraint-marker constraint))
          () 'assertion-violated)
  (let* ((solver (constraint-solver constraint))
         (expression (make-expression solver constraint)))
    (handler-bind ((classowary-condition (lambda (e)
                                           (declare (ignore e))
                                           (remove-errors solver constraint))))
      (try-add-expression solver expression constraint))
    (optimize-for (solver-objective solver) solver)
    (when (solver-auto-update solver)
      (update-variables solver))
    constraint))

(defun remove-constraint (constraint)
  (when (constraint-marker constraint)
    (let ((solver (constraint-solver constraint))
          (marker (constraint-marker constraint))
          (tmp (%make-expression)))
      (remove-errors solver constraint)
      (handler-case (get-expression solver marker tmp)
        (classowary-condition ()
          (let ((exit (get-leaving-expression solver marker)))
            (assert (not (null exit)))
            (get-expression solver exit tmp)
            (solve-for tmp marker exit)
            (substitute-expressions solver marker tmp))))
      (clrhash (expression-terms tmp))
      (optimize-for (solver-objective solver) solver)
      (when (solver-auto-update solver)
        (update-variables solver))
      constraint)))

(defun strength (constraint)
  (constraint-strength constraint))

(defun (setf strength) (strength constraint)
  (let ((strength (->strength strength)))
    (unless (= strength (constraint-strength constraint))
      (cond ((or (<= +REQUIRED+ (constraint-strength constraint))
                 (<= +REQUIRED+ strength))
             (remove-constraint constraint)
             (setf (constraint-strength constraint) strength)
             (add-constraint constraint))
            (T
             (when (constraint-marker constraint)
               (let ((solver (constraint-solver constraint))
                     (diff (- strength (constraint-strength constraint))))
                 (merge-expression solver (solver-objective solver) (constraint-marker constraint) diff)
                 (merge-expression solver (solver-objective solver) (constraint-other constraint) diff)
                 (optimize-for (solver-objective solver) solver)
                 (when (solver-auto-update solver)
                   (update-variables solver))))
             (setf (constraint-strength constraint) strength)))))
  strength)

(defun make-suggestable (variable &optional (strength :strong))
  (assert (not (null (variable-symbol variable))))
  (let ((strength (min +STRONG+ (->strength strength))))
    (cond ((suggestable-p variable)
           (setf (strength (variable-constraint variable)) strength))
          (T
           (let ((constraint (make-constraint (variable-solver variable) :strength strength)))
             (setf (relation constraint) '=)
             (add-variable-term constraint variable 1f0)
             (add-constant constraint (- (variable-value variable)))
             (add-constraint constraint)
             (setf (variable-constraint variable) constraint)
             (setf (variable-edit-value variable) (variable-value variable))
             variable)))))

(defun make-unsuggestable (variable)
  (when (variable-constraint variable)
    (delete-constraint (variable-constraint variable))
    (setf (variable-constraint variable) NIL)
    (setf (variable-edit-value variable) 0f0)
    variable))

(defun suggest (variable value &optional (if-not-suggestable :error))
  (unless (suggestable-p variable)
    (ecase if-not-suggestable
      (:error
       (restart-case (error 'variable-not-suggestable :variable variable :solver (variable-solver variable))
         (make-suggestable (&optional strength)
           :report (lambda (s) (format s "Make ~a suggestable." variable))
           (make-suggestable variable (or strength :medium)))
         (abort ()
           :report (lambda (s) (format s "Do not suggest ~a." variable))
           (return-from suggest NIL))))
      (:make-suggestable
       (make-suggestable variable))
      ((NIL)
       (return-from suggest NIL))))
  (let* ((value (float value 0f0))
         (delta (- value (variable-edit-value variable)))
         (solver (variable-solver variable)))
    (setf (variable-edit-value variable) value)
    (delta-edit-constant solver delta (variable-constraint variable))
    (dual-optimize solver)
    (when (solver-auto-update solver)
      (update-variables solver))
    variable))
