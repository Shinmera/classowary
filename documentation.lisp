#|
 This file is a part of Classowary
 (c) 2019 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.classowary)

;; cassowary.lisp
(docs:define-docs
  (function suggestable-p
    "Returns true if the variable is suggestable.

See VARIABLE (type)
See MAKE-SUGGESTABLE
See MAKE-UNSUGGESTABLE
See SUGGEST")
  
  (function constrained-p
    "Returns true if the constraint is additionally constrained through a variable.

See CONSTRAINT (TYPE)")
  
  (function make-solver
    "Returns a new solver instance.

If AUTO-UPDATE is T, the solver will automatically call
UPDATE-VARIABLES when constraints or variables change.

See SOLVER (type)
See AUTO-UPDATE
See RESET-SOLVER
See MAKE-CONSTRAINT
See CONSTRAIN
See MAKE-VARIABLE
See WITH-VARIABLES")

  (function reset-solver
    "Resets the solver.

This resets all variables and makes them unsuggestable.

If the solver is AUTO-UPDATE, the variables will be updated before this
operation.

If CLEAR-CONSTRAINTS is T, removes all constraints from the solver.

See SOLVER (type)")
  
  (function update-variables
    "Recomputes the variable values according to the current solver constraints.

You should call this function whenever you are done changing
constraints or suggesting variables and would like to read out the
new, computed values for the variables.

Returns the solver.

See AUTO-UPDATE")
  
  (function add-constraint
    "Adds the constraint to its solver.

Without this, the constraint will not apply.

If the solver is AUTO-UPDATE, the variables will be updated after this
operation.

See SOLVER
See CONSTRAINT (type)
See REMOVE-CONSTRAINT")
  
  (function remove-constraint
    "Removes the constraint from its solver.

After this, the constraint will not apply.

If the solver is AUTO-UPDATE, the variables will be updated after this
operation.

See SOLVER
See CONSTRAINT (type)
See REMOVE-CONSTRAINT")
  
  (function strength
    "Accessor to the strength of the constraint.

The strength is a positive float, denoting how important this
constraint is to the overall system. Valid values for the strength are
positive real numbers and the following keywords:

  :REQUIRED
  :STRONG
  :MEDIUM
  :WEAK
  :NONE

Each of these keywords corresponds to the constant of the same
name. If a value that is neither a positive real, nor one of the above
keywords is set, an error is signalled.

If the solver is AUTO-UPDATE, the variables will be updated after the
strength is updated.

See CONSTRAINT (type)
See +REQUIRED+
See +STRONG+
See +MEDIUM+
See +WEAK+
See +NONE+")
  
  (function make-suggestable
    "Make the variable suggestable.

This means that you can edit the variable and suggest a value that the
solver should try to keep steady when solving its system. Typically
this is used for variables that are constrained by the user somehow.

In order to use SUGGEST, a variable must first be made suggestable.

If the variable is already suggestable, its strength is updated via
(SETF STRENGTH).

If the variable was not already suggestable, it is suggested to the
current value of the variable.

The strength of the suggestion is capped to +STRONG+. Any value above
will automatically be reduced to +STRONG+.

Returns the variable.

See STRENGTH
See VARIABLE (type)
See MAKE-UNSUGGESTABLE
See SUGGESTABLE-P
See SUGGEST
See STRENGTH")
  
  (function make-unsuggestable
    "Makes the variable unsuggestable again.

After this, SUGGEST will fail unless the variable is explicitly made
suggestable again.

Returns the variable.

See MAKE-SUGGESTABLE
See SUGGESTABLE-P
See VARIABLE (type)
See SUGGEST")
  
  (function suggest
    "Suggest a particular value for the variable.

The solver will try to solve the system while keeping the value of the
variable to the suggested value. Note however that this does not
circumvent required constraints.

If the variable is not already suggestable, the value of
if-not-suggestable becomes important. It may be one of the following:
  :ERROR             --- An error of type VARIABLE-NOT-SUGGESTABLE is
                         signalled.
  :MAKE-SUGGESTABLE  --- The variable is made suggestable with medium
                         strength.
  NIL                --- The function simply returns NIL.

If the solver is AUTO-UPDATE, the variables will be updated after this
operation.

On success, returns the variable.

See VARIABLE (type)
See SUGGESTABLE-P
See MAKE-SUGGESTABLE
See MAKE-UNSUGGESTABLE
See VARIABLE-NOT-SUGGESTABLE"))

;; conditions.lisp
(docs:define-docs
  (type classowary-condition
    "Supertype for all conditions related to Classowary.

See CL:CONDITION (type)")
  
  (type assertion-violated
    "Error signalled when an internal assertion was violated.

This may arise due to invalid values being passed, or otherwise
invalid sequences of operations being performed.

See CLASSOWARY-CONDITION
See CL:ERROR (type)")
  
  (type expression-unsatisfied
    "Error signalled when it is impossible for the solver to satisfy an expression.

See CLASSOWARY-CONDITION
See CL:ERROR (type)
See SOLVER (function)
See EXPRESSION (function)")
  
  (function solver
    "Returns the solver associated with the object.

See SOLVER (type)
See VARIABLE (type)
See CONSTRAINT (type)
See EXPRESSION-UNSATISFIED
See EXPRESSION-UNBOUND")
  
  (function expression
    "Returns the expression associated with the object.

See EXPRESSION (type)
See EXPRESSION-UNSATISFIED
see EXPRESSION-UNBOUND")
  
  (type expression-unbound
    "Error signalled when an expression is unbound.

See CLASSOWARY-CONDITION
See CL:ERROR (type)
See SOLVER (function)
See EXPRESSION (function)")

  (type variable-not-suggestable
    "Error signalled when attempting to suggest an unsuggestable variable.

When this error is signalled, two restarts must be active with the
following behaviour:
  MAKE-SUGGESTABLE  --- Makes the variable suggestable before setting
                        the suggested value. The restart accepts an
                        optional argument that sets the strength of
                        the variable.
  ABORT             --- Aborts the suggestion and returns NIL.

See CLASSOWARY-CONDITION
See CL:ERROR (type)
See SOLVER (function)
See VARIABLE (function)")

  (function variable
    "Returns the variable associated with the object.

See VARIABLE (type)
See VARIABLE-NOT-SUGGESTABLE"))

;; constraint.lisp
(docs:define-docs
  (type solver
    "Represents a linear constraint system.

A linear constraint system is composed out of several constraints that
describe an equality or inequality composed out of linear terms.
The solver can then attempt to find a solution that fits the variables
in the terms to the constraints as well as possible.

Note that the solver will bend constraints if there is no solution
that does not violate any of them.

See CONSTRAINT (type)
See MAKE-SOLVER
See RESET-SOLVER
See AUTO-UPDATE
See MAKE-CONSTRAINT
See MAKE-VARIABLE
See CONSTRAIN
see WITH-VARIABLES
See FIND-VARIABLE
See FIND-CONSTRAINT")
  
  (type variable
    "Represents a variable in a linear system.

A variable can appear in an arbitrary number of constraints to express
linear terms. A variable has a value and two states: suggestable, or
unsuggestable. In the default, unsuggestable state, the variable is
freely controlled by the linear system and its value is directly
deduced from the constraints. In the suggestable state, the variable
has a suggested value that the user supplies, which the solver should
attempt to keep constant as the system is solved.

See MAKE-VARIABLE
See DELETE-VARIABLE
See FIND-VARIABLE
See ADD-TERM
See WITH-VARIABLES
See SOLVER (function)
See MAKE-SUGGESTABLE
See MAKE-UNSUGGESTABLE
See SUGGESTABLE-P
See SUGGEST
See VALUE")
  
  (type constraint
    "Represents a constraint in a linear system.

Each of the constraints in a system must appear in the following
normalised, mathematical form:

  CONSTRAINT ::= TERM RELATION TERM (\\+ TERM)*
  RELATION   ::= <= | = | >=
  TERM       ::= number (\\* variable)?
  number     --- a SINGLE-FLOAT
  variable   --- a VARIABLE

For instance, the following are valid constraints:

  0 = 0
  1 = 2
  1*x <= 1
  2*x >= 3 + 4*y
  
And the following are not valid constraints:

  1*x + 1*y = 3*y + 5*z
  x*x = 2

See MAKE-CONSTRAINT
See CLONE-CONSTRAINT
See DELETE-CONSTRAINT
See FIND-CONSTRAINT
See ADD-CONSTRAINT
See REMOVE-CONSTRAINT
See ADD-TERM
See ADD-CONSTANT
See RELATION
See SOLVER (function)
See CONSTRAIN")

  (function find-variable
    "Returns the variable associated with the given name, if any.

Note that you are /not/ permitted to SETF this place.

See VARIABLE (type)
See SOLVER (type)")

  (function find-constraint
    "Returns the constraint associated with the given name, if any.

Note that you are /not/ permitted to SETF this place.

See CONSTRAINT (type)
See SOLVER (type)")

  (function auto-update
    "Accessor to whether the solver automatically updates variables on changes.

See SOLVER (type)")
  
  (function value
    "Returns the current value of the variable.

You might need to call UPDATE-VARIABLES on the solver before this
returns an accurate value for the variable.

Note that for suggestable variables this might return a value
different from the one previously suggested.

See VARIABLE (type)")
  
  (function make-variable
    "Returns a new variable for the solver.

The NAME is an optional identifier for the symbol used for the
variable by which it can later be found again.

If STRENGTH is given, the variable is made suggestable and
its strength set accordingly. STRENGTH may also be T in this case, in
which case the strength used is +STRONG+.

See VARIABLE (type)
See DELETE-VARIABLE
See MAKE-SUGGESTABLE
See FIND-VARIABLE")
  
  (function delete-variable
    "Deletes the variable completely from the solver.

This will remove the variable and all terms in all expressions that
include it.

See MAKE-VARIABLE
See VARIABLE (type)")
  
  (function make-constraint
    "Returns a new constraint for the solver.

The STRENGTH designates how strongly this constraint is adhered. By
default the strength is +REQUIRED+.

The NAME designates the symbol by which this constraint can later be
found again.

See STRENGTH (type)
See DELETE-CONSTRAINT
See FIND-CONSTRAINT")
  
  (function delete-constraint
    "Deletes the constraint completely from the solver.

This will render the constraint useless. Operating on it beyond this
point results in undefined behaviour.

See CONSTRAINT (type)
See MAKE-CONSTRAINT")
  
  (function clone-constraint
    "Return a copy of the given constraint.

This will copy the constraint including its terms into a fresh
instance.

See CONSTRAINT (type)
See MAKE-CONSTRAINT
See DELETE-CONSTRAINT")
  
  (function reset-constraint
    "Resets the constraint.

This will remove the constraint, unset its relation, and clear its
expression of all terms.

Returns the updated constraint.

See CONSTRAINT (type)
See REMOVE-CONSTRAINT")
  
  (function add-term
    "Adds the given term description to the constraint.

This corresponds to a term of either
  number
or
  number*variable
being added to the constraint depending on whether the variable is
given.

Note that adding or removing terms can automatically cancel out as
soon as either the constant term of the expression reaches zero, or
the multiplier of a variable term reaches zero. In the latter case,
the variable term is automatically removed from the expression.

Returns the updated constraint.

See CONSTRAINT (type)
See VARIABLE (type)
See REMOVE-TERM")

  (function remove-term
    "Removes the given term description from the constraint.

This corresponds to a term of either
  number
or
  number*variable
being removed from the constraint depending on whether the variable is
given.

Note that adding or removing terms can automatically cancel out as
soon as either the constant term of the expression reaches zero, or
the multiplier of a variable term reaches zero. In the latter case,
the variable term is automatically removed from the expression.

This operation is equivalent to ADD-TERM with the multiplier inverted.

Returns the updated constraint.

See CONSTRAINT (type)
See VARIABLE (type)
See ADD-TERM")
  
  (function relation
    "Accessor to the relation of the constraint.

May be one of the following CL symbols:

  <=
  =
  >=

See CONSTRAINT (type)")
  
  (function with-variables
    "Convenience macro to create and bind multiple variables at once.

Each binding must follow this structure:

  BINDING  ::= variable | (variable name? strength?)
  variable --- The CL variable the solver variable is bound to.
  name     --- The name of the solver variable as a symbol.
               This can later be used to retrieve the variable. If no
               explicit name is given, the CL variable name is used.
  strength --- The strength of the variable if it is newly created.

If a variable with the given name already exists in the solver, it is
returned. Otherwise, a new variable of the given name is created. If
NAME is NIL, a new variable is always created.

Note that the variables are /not/ deleted on exit from the body.

See VARIABLE (type)
See SOLVER (type)
See FIND-VARIABLE
See MAKE-VARIABLE")
  
  (function constrain
    "Constrain the linear system in the solver.

The constraint should be an expression of the following form:

  CONSTRAINT ::= (RELATION EXPRESSION EXPRESSION)
  RELATION   ::= <= | = | >=
  EXPRESSION ::= TERM | COMPOUND
  COMPOUND   ::= (+ EXPRESSION*)
               | (- EXPRESSION+)
               | (* EXPRESSION*) 
               | (/ EXPRESSION+)
  TERM       ::= variable | real | (quote quoted)
  variable   --- A CL variable binding name.
  quoted     --- A symbol naming a solver variable.
  real       --- A real number.

If a quoted variable is used, the variable is retrieved from the
solver, or created if it does not exist yet. It will have the name of
the quoted symbol. Otherwise the variable denotes a CL variable whose
value must be of type VARIABLE.

If the expression contains non-linear terms, such as the
multiplication of two variables, or the division by a variable, an
error is signalled at macro-expansion-time.

This macro is a short-hand to create constraint, add terms, set the
relation, and add the constraint to the solver.

The new constraint instance is returned.

See CONSTRAINT (type)
See SOLVER (type)
See MAKE-CONSTRAINT
See ADD-TERM
See RELATION
See ADD-CONSTRAINT"))

;; expression.lisp
(docs:define-docs
  (type term
    "The representation of a variable term.

A term is a variable and a linear multiplication factor.")
  
  (type expression
    "The representation of a linear expression.

A linear expression represents a constant factor added together with a
series of linear variable terms.

See TERM"))

;; symbol.lisp
(docs:define-docs)

;; toolkit.lisp
(docs:define-docs
  (cl:variable +REQUIRED+
    "Constant for constraints with a very high weight.

Corresponds to 1e9. You can also specify the keyword :required instead
of this constant wherever strengths are accepted.")

  (cl:variable +STRONG+
    "Constant for constraints with a high weight.

Corresponds to 1e6. You can also specify the keyword :strong instead
of this constant wherever strengths are accepted.")

  (cl:variable +MEDIUM+
    "Constant for constraints with a medium weight.

Corresponds to 1e3. You can also specify the keyword :medium instead
of this constant wherever strengths are accepted.")

  (cl:variable +WEAK+
    "Constant for constraints with a weak weight.

Corresponds to 1e0. You can also specify the keyword :weak instead
of this constant wherever strengths are accepted.")

  (cl:variable +NONE+
    "Constant for constraints with no weight.

Corresponds to 0e0. You can also specify the keyword :none instead
of this constant wherever strengths are accepted."))
