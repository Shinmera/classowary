#|
 This file is a part of Classowary
 (c) 2019 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(defpackage #:org.shirakumo.classowary
  (:use #:cl)
  (:shadow #:substitute #:variable)
  (:export
   #:cassowary-condition
   #:assertion-violated
   #:expression-unsatisfied
   #:solver
   #:expression
   #:expression-unbound)
  (:export
   #:+REQUIRED+
   #:+STRONG+
   #:+MEDIUM+
   #:+WEAK+
   #:solver
   #:variable
   #:constraint
   #:expression
   #:term
   #:make-solver
   #:reset-solver
   #:update-variables
   #:auto-update
   #:edited-p
   #:constrained-p
   #:add-constraint
   #:remove-constraint
   #:add-edit
   #:suggest
   #:delete-edit
   #:make-variable
   #:use-variable
   #:delete-variable
   #:with-variables
   #:value
   #:make-constraint
   #:clone-constraint
   #:reset-constraint
   #:delete-constraint
   #:constrain
   #:add-term
   #:relation
   #:add-constant
   #:strength
   #:merge-constraint))
