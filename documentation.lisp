#|
 This file is a part of Classowary
 (c) 2019 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.classowary)

;; cassowary.lisp
(docs:define-docs
  (function edited-p
    "")
  
  (function constrained-p
    "")
  
  (function make-solver
    "")
  
  (function update-variables
    "")
  
  (function add-constraint
    "")
  
  (function remove-constraint
    "")
  
  (function strength
    "")
  
  (function add-edit
    "")
  
  (function delete-edit
    "")
  
  (function suggest
    ""))

;; conditions.lisp
(docs:define-docs
  (type cassowary-condition
    "")
  
  (type assertion-violated
    "")
  
  (type expression-unsatisfied
    "")
  
  (function solver
    "")
  
  (function expression
    "")
  
  (type expression-unbound
    ""))

;; constraint.lisp
(docs:define-docs
  (type solver
    "")
  
  (type variable
    "")
  
  (type constraint
    "")
  
  (function value
    "")
  
  (function make-variable
    "")
  
  (function delete-variable
    "")
  
  (function make-constraint
    "")
  
  (function delete-constraint
    "")
  
  (function clone-constraint
    "")
  
  (function reset-constraint
    "")
  
  (function add-term
    "")
  
  (function add-constant
    "")
  
  (function relation
    "")
  
  (function with-variables
    "")
  
  (function constrain
    ""))

;; expression.lisp
(docs:define-docs
  (type term
    "")
  
  (type expression
    ""))

;; symbol.lisp
(docs:define-docs)

;; toolkit.lisp
(docs:define-docs
  (variable +REQUIRED+
    "Constant for constraints with a very high weight.

Corresponds to 1e9. You can also specify the keyword :required instead
of this constant wherever strengths are accepted.")

  (variable +STRONG+
    "Constant for constraints with a high weight.

Corresponds to 1e6. You can also specify the keyword :strong instead
of this constant wherever strengths are accepted.")

  (variable +MEDIUM+
    "Constant for constraints with a medium weight.

Corresponds to 1e3. You can also specify the keyword :medium instead
of this constant wherever strengths are accepted.")

  (variable +WEAK+
    "Constant for constraints with a weak weight.

Corresponds to 1e0. You can also specify the keyword :weak instead
of this constant wherever strengths are accepted."))
