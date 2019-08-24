#|
 This file is a part of Classowary
 (c) 2019 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.classowary)

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

(defun ~= (a b)
  (< (abs (- a b)) 1e-4))

(defun ~zerop (float)
  (~= float 0f0))
