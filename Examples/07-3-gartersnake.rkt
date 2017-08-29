;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname 07-3-defs) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require rackunit)
(require "extras.rkt")
(require "sets.rkt")

;; This problem concerns a tiny language, which we call GarterSnake.

;; The information:

;; A Program is a sequence of function definitions. The function
;; defined in each definition is available for use in all of the
;; following definitions.

;; A Definition looks like def f(x1,..,xn):exp .

;; This defines a function named f with arguments x1, x2, etc., and
;; body exp. The arguments of the function are available in the body
;; of the function.  The function f itself is also available in the
;; body of the function.  It is legal for a function to take no arguments.

;; An Exp is either a variable v or a function call
;; f(e1,..,en). v is a reference to the variable or function named v.
;; f(e1,e2,...) is an application of f to the arguments e1, e2, etc.
;; It is legal for a function to be applied to no arguments.

;; There is no distinction between function names and variable names:
;; a named function may be passed as an argument to another function,
;; and an argument may be called as a function.

;; Examples:
#|
def f1(x):f1(x)           ; f1 is defined in the body of f1
def f2 (x, y):f1(y)       ; f1 is defined in the body of f2
def f3 (x,z): f1(f2(z,f1)) ; f1 and f2 are defined in the body of f3
                           ; spaces are ignored
                          ; you can pass a function as an argument
def f4 (x, z):x(z,z)       ; you can call an argument as a function
|#

;; If we use a variable in a place where it is not available, we say
;; it is _undefined_ there.


;; Bad Examples
#|
def f1(x):f2(y)            ; f2 is undefined, and y is undefined
def f2(x,y): f3(y,x)       ; f3 is undefined in the body of f2
def f3 (x,z):f1(f2(z,y),z) ; y is undefined
|#




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; The problem:

;;; Given a GarterSnake program p, determine whether there are any
;;; undefined variables in p.

;; program-all-defined? : Program -> Bool
;; GIVEN: A GarterSnake program p
;; RETURNS: true iff there every variable occurring in p is available at
;; the place it occurs.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; INFORMATION ANALYSIS

;; The information does not specify the external representation
;; (information) in complete detail, so we have some freedom in designing
;; the internal representation (data).

;; IMPORTANT: We are NOT writing functions to convert from an external
;; representation to this internal representation (or vice versa).
;; That will come next week.

;; We will try a simple representation

;;;;;;;;;;;;;;;;

;; A Program is a ListOfDefinition

;;;;;;;;;;;;;;;;

(define-struct def (name args body))
;; A Definition is a (make-def Variable ListOfVariable Exp)
;; INTERPRETATION:
;; name is the name of the function being defined
;; args is the list of arguments of the function
;; body is the body of the function.

;;;;;;;;;;;;;;;;

(define-struct varexp (name))
(define-struct appexp (fn args))

;; An Exp is one of
;; -- (make-varexp Variable)
;; -- (make-appexp Variable ListOfExp)
;; INTERPRETATION;
;; (make-varexp v)                   represents a use of the variable v
;; (make-appexp f (list e1 ... en))  represents a call to the function
;;                                   named f, with arguments e1,..,en

;;;;;;;;;;;;;;;;

;; A Variable is a Symbol

;; We could have represented variables using strings instead of
;; symbols, but using symbols makes it a little easier to build
;; examples. 

;;;;;;;;;;;;;;;;

;; OBSERVER TEMPLATES:

;; We group these because def-fn and exp-fn are mutually recursive.

;; pgm-fn : Program -> ??
#;
(define (pgm-fn p)
  (lodef-fn p))

;; def-fn : Definition -> ??
#;
(define (def-fn d)
  (... (def-name d) (def-args d) (def-body d)))

;; exp-fn : Exp -> ??
#;
(define (exp-fn e)
  (cond
    [(varexp? e) (... (varexp-name e))]
    [(appexp? e) (... (appexp-fn e) (loexp-fn (appexp-args e)))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SYSTEM DESIGN

;; We'll need to recur on the list structure of programs.  When we
;; analyze a definition, what information do we need to carry forward?
;; Let's look at an example.  We'll annotate each definition with a
;; list of the variables available in its body. 

#|
def f1(x):f1(x)          ; f1 and x are available in the body.
def f2(u,y):f1(y)        ; f1, f2, u, and y, are available in the body.
def f3(x,z):f1(f2(z,f1)) ; f1, f2, f3, x, and z are available in the body.
def f4(x,z):x(z,z)       ; f1, f2, f3, f4, x, and z are available in the body.
|#

;; In each case, the variables available in the body are the names of
;; the functions defined _before_ the current function, plus the names
;; of the current function and its arguments.

;; Let's look at the "middle" of the calculation.
;; When we analyze the definition of f3, we need to know that f1 and
;; f2 are defined.  When we analyze the body of f3, we need to know
;; that f1, f2, x, and z are defined.

;; So we generalize our functions to take a second argument which is
;; the set of defined variables.

;; We'll have a family of functions that follow the data definitions;

;; program-all-defined : Program                        -> Boolean
;; lod-all-defined?    : ListOfDefinition SetOfVariable -> Boolean
;; def-all-defined?    : Definition       SetOfVariable -> Boolean         
;; exp-all-defined?    : Exp              SetOfVariable -> Boolean         

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; lod-all-defined? : ListOfDefinition SetOfVariable -> Boolean
;; GIVEN: a list of definitions 'defs' from some program p and a set of
;; variables 'vars'
;; WHERE: vars is the set of variables available at the start of defs in
;; p.
;; RETURNS: true iff there are no undefined variables in defs.
;; EXAMPLES: See example above
;; STRATEGY: Use template for ListOfDefinition on defs.  The names
;; available in (rest defs) are those in vars, plus the variable
;; defined in (first defs).

(define (lod-all-defined? defs vars)
  (cond
    [(null? defs) true]
    [else
     (and
      (def-all-defined? (first defs) vars)
      (lod-all-defined? (rest  defs)
                        (set-cons (def-name (first defs))
                                  vars)))]))


;; def-all-defined? : Definition SetOfVariable -> Boolean 
;; GIVEN: A definition 'def' from some program p and a set of
;; variables 'vars'
;; WHERE: vars is the set of variables available at the start of def in
;; p.
;; RETURNS: true if there are no undefined variables in the body of
;; def.  The available variables in the body are the ones in def, plus
;; the name and arguments of the definition.
;; EXAMPLES: See example above
;; STRATEGY: Use template for Definition on def

(define (def-all-defined? def vars)
  (exp-all-defined? (def-body def)
                    (set-cons
                     (def-name def)
                     (set-union (def-args def) vars))))


;; exp-all-defined? : Exp SetOfVariable -> Boolean
;; GIVEN: A GarterSnake expression e from some program p and a set of
;; variables vars  
;; WHERE: vars is the set of variables that are available at the
;; occurrence of e in p
;; RETURNS: true iff all the variable in exp are defined
;; STRATEGY: Use template for Exp on e

(define (exp-all-defined? e vars)
  (cond
    [(varexp? e) (my-member? (varexp-name e) vars)]
    [(appexp? e)
     (and (my-member? (appexp-fn e) vars)
          (andmap
           (lambda (e1) (exp-all-defined? e1 vars))
           (appexp-args e)))]))


;; program-all-defined? : Program -> Bool
;; GIVEN: A GarterSnake program p
;; RETURNS: true iff there every variable occurring in p is defined at
;; the place it occurs.
;; STRATEGY: Initialize the invariant of lod-all-defined?

(define (program-all-defined? p)
  (lod-all-defined? p empty))

;;; Let's turn our examples into tests

;; Examples:
#|
def f1 (x) (f1 x)           ; f1 is defined in the body of f1
def f2 (x y) (f1 y)         ; f1 is defined in the body of f2
def f3 (x z) (f1 (f2 z f1)) ; f1 and f2 are defined in the body of f3
                            ; you can pass a function as an argument
def f4 (x z) (x z z)        ; you can call an argument as a function
|#

;; Bad Examples
#|
def f1 (x) (f2 y)            ; f2 is undefined, and y is undefined
def f2 (x y) (f3 y x)        ; f3 is undefined in the body of f2
def f3 (x z) (f1 (f2 z y) z) ; y is undefined
|#

(begin-for-test

  (check-true
   (program-all-defined?
    (list
     (make-def 'f1 (list 'x) (make-appexp 'f1 (list (make-varexp 'x))))
     (make-def 'f2 (list 'x 'y) (make-appexp 'f1 (list (make-varexp 'y))))
     (make-def 'f3 (list 'x 'y 'z)
               (make-appexp 'f1 (list (make-appexp 'f2
                                             (list (make-varexp 'z)
                                                   (make-varexp 'y)))
                                   (make-varexp 'z)))))))

  (check-false
   (program-all-defined?
    (list
     (make-def 'f1 (list 'x) (make-appexp 'f2 (list (make-varexp 'x))))
     (make-def 'f2 (list 'x 'y) (make-appexp 'f1 (list (make-varexp 'y))))
     (make-def 'f3 (list 'x 'y 'z)
               (make-appexp 'f1 (list (make-appexp 'f2
                                             (list (make-varexp 'z)
                                                   (make-varexp 'y)))
                                   (make-varexp 'z))))))
   "should find f2 undefined in body of f1")

  (check-false
   (program-all-defined?
    (list
     (make-def 'f1 (list 'x) (make-appexp 'f1 (list (make-varexp 'x))))
     (make-def 'f2 (list 'x 'y) (make-appexp 'f3 (list (make-varexp 'y))))
     (make-def 'f3 (list 'x 'y 'z)
               (make-appexp 'f1 (list (make-appexp 'f2
                                             (list (make-varexp 'z)
                                                   (make-varexp 'y)))
                                   (make-varexp 'z))))))
   "should find f3 undefined in body of f2")

  (check-false
   (program-all-defined?
    (list
     (make-def 'f1 (list 'x) (make-appexp 'f1 (list (make-varexp 'x))))
     (make-def 'f2 (list 'x 'y) (make-appexp 'f1 (list (make-varexp 'y))))
     (make-def 'f3 (list 'x 'z)
               (make-appexp 'f1 (list (make-appexp 'f2
                                             (list (make-varexp 'z)
                                                   (make-varexp 'y)))
                                   (make-varexp 'z))))))
   "should find y undefined in body of f3")

  )


  

  
     
               



