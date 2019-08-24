#|
 This file is a part of Classowary
 (c) 2019 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.classowary)

(defvar *symbol-ids* 0)

(defun mksym (type &optional (id 'symbol))
  (assert (or (eq type 'external)
              (eq type 'error)
              (eq type 'slack)
              (eq type 'dummy)))
  (let* ((id (incf *symbol-ids*))
         (sym (make-symbol (format NIL "~a~a"
                                   (ecase type
                                     (external "V")
                                     (slack "S")
                                     (error "E")
                                     (dummy "D"))
                                   id))))
    (setf (get sym 'type) type)
    (setf (get sym 'id) id)
    sym))

(defun external-p (symbol)
  (eq 'external (get symbol 'type)))

(defun error-p (symbol)
  (eq 'error (get symbol 'type)))

(defun slack-p (symbol)
  (eq 'slack (get symbol 'type)))

(defun dummy-p (symbol)
  (eq 'dummy (get symbol 'type)))

(defun pivotable-p (symbol)
  (or (slack-p symbol)
      (error-p symbol)))

(defun write-sym (symbol stream)
  (if symbol
      (format stream "~a~a"
              (ecase (get symbol 'type)
                (external "v")
                (slack "s")
                (error "e")
                (dummy "d"))
              (get symbol 'id))
      (format stream "~a" NIL)))

(defconstant +REQUIRED+ 1000000000f0)
(defconstant +STRONG+ 1000000f0)
(defconstant +MEDIUM+ 1000f0)
(defconstant +WEAK+ 1f0)

(defun ->strength (strength)
  (etypecase strength
    (real (float strength 0f0))
    ((eql :required) +REQUIRED+)
    ((eql :strong) +STRONG+)
    ((eql :medium) +MEDIUM+)
    ((eql :weak) +WEAK+)
    ((eql :none) 0f0)))
