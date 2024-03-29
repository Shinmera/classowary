(in-package #:org.shirakumo.classowary)

(defstruct (term (:constructor make-term (key &optional multiplier)))
  (key NIL :type symbol)
  (multiplier 0f0 :type single-float))

(defmethod print-object ((term term) stream)
  (print-unreadable-object (term stream :type T)
    (format stream "~s ~f" (term-key term) (term-multiplier term))))

(defmethod describe-object ((term term) stream)
  (format stream " ~:[+~;-~] " (< (term-multiplier term) 0))
  (unless (~= 1f0 (abs (term-multiplier term)))
    (format stream "~f*" (abs (term-multiplier term))))
  (write-sym (term-key term) stream))

(defstruct (expression (:constructor %make-expression (&optional key)))
  (key NIL :type symbol)
  (infeasible-p NIL :type boolean)
  (terms (make-hash-table :test 'eq) :type hash-table)
  (constant 0f0 :type single-float))

(defmacro do-terms ((term expression &optional result) &body body)
  `(loop for ,term being the hash-values of (expression-terms ,expression)
         do (progn ,@body)
         finally (return ,result)))

(defmethod print-object ((expression expression) stream)
  (print-unreadable-object (expression stream :type T)
    (format stream "~s ~f"
            (expression-key expression)
            (expression-constant expression))
    (do-terms (term expression)
      (describe-object term stream))))

(defun constant-p (expression)
  (= 0 (hash-table-count (expression-terms expression))))

(defun clear-expression (expression)
  (setf (expression-key expression) NIL)
  (setf (expression-infeasible-p expression) NIL)
  (setf (expression-constant expression) 0f0)
  (clrhash (expression-terms expression))
  expression)

(defun reset-expression (expression)
  (setf (expression-constant expression) 0f0)
  (clrhash (expression-terms expression))
  expression)

(defmethod describe-object ((expression expression) stream)
  (write-sym (expression-key expression) stream)
  (format stream " = ~f" (expression-constant expression))
  (do-terms (term expression)
    (describe-object term stream))
  (when (expression-infeasible-p expression)
    (format stream " INFEASIBLE")))

(defun multiply (expression multiplier)
  (setf (expression-constant expression) (* multiplier (expression-constant expression)))
  (do-terms (term expression expression)
    (setf (term-multiplier term) (* multiplier (term-multiplier term)))))

(defun find-term (symbol expression)
  (gethash symbol (expression-terms expression)))

(defun (setf find-term) (term symbol expression)
  (if term
      (setf (gethash symbol (expression-terms expression)) term)
      (remhash symbol (expression-terms expression)))
  term)

(defun ensure-term (symbol expression)
  (or (gethash symbol (expression-terms expression))
      (setf (gethash symbol (expression-terms expression))
            (make-term symbol))))

(defun add-variable (expression symbol multiplier)
  (let ((term (ensure-term symbol expression)))
    (if (~zerop (incf (term-multiplier term) multiplier))
        (setf (find-term symbol expression) NIL)
        term)))

(defun add-expression (expression to-add multiplier)
  (incf (expression-constant expression) (* (expression-constant to-add) multiplier))
  (do-terms (term to-add expression)
    (add-variable expression (term-key term) (* (term-multiplier term) multiplier))))

(defun solve-for (expression entry exit)
  (let* ((term (find-term entry expression))
         (reciprocal (/ (term-multiplier term))))
    (assert (and (not (eq entry exit)) (not (~zerop (term-multiplier term)))))
    (setf (find-term entry expression) NIL)
    (multiply expression (- reciprocal))
    (if exit
        (add-variable expression exit reciprocal)
        expression)))

(defun substitute (expression entry to-substitute)
  (let ((term (find-term entry expression)))
    (when term
      (setf (find-term entry expression) NIL)
      (add-expression expression to-substitute (term-multiplier term)))))
