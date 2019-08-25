#|
 This file is a part of Classowary
 (c) 2019 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(defpackage #:org.shirakumo.classowary
  (:use #:cl)
  (:shadow #:substitute #:variable)
  (:export
   #:classowary-condition
   #:assertion-violated
   #:expression-unsatisfied
   #:solver
   #:expression
   #:expression-unbound
   #:variable-not-suggestable
   #:variable)
  (:export
   #:+REQUIRED+
   #:+STRONG+
   #:+MEDIUM+
   #:+WEAK+
   #:+NONE+
   #:solver
   #:variable
   #:constraint
   #:expression
   #:term
   #:make-solver
   #:reset-solver
   #:update-variables
   #:auto-update
   #:suggestable-p
   #:constrained-p
   #:add-constraint
   #:remove-constraint
   #:make-suggestable
   #:suggest
   #:make-unsuggestable
   #:make-variable
   #:delete-variable
   #:with-variables
   #:find-variable
   #:find-constraint
   #:value
   #:auto-update
   #:make-constraint
   #:clone-constraint
   #:reset-constraint
   #:delete-constraint
   #:constrain
   #:add-term
   #:remove-term
   #:relation
   #:strength
   #:merge-constraint))
