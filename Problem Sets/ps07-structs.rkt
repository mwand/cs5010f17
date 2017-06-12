;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ps07) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require rackunit)
(require "extras.rkt")
(provide (struct-out def)
         (struct-out varexp)
         (struct-out appexp)
         (struct-out ir-definition)
         (struct-out local-varexp)
         (struct-out global-varexp)
         (struct-out undefined-varexp)
         (struct-out ir-app-exp))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Lexical Addressing for GarterSnake

;; We want to replace each occurrence of a variable with a unique
;; descriptor that will tell the compiler WHERE to find the value of
;; the variable.

;;; The problem:

;;; Translate a GarterSnake program p to a corresponding intermediate
;;; representation. 

;; translation-of-program : Program -> IRProgram
;; GIVEN: A GarterSnake program p

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; GarterSnake Definition
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

;; A Variable is a Symbol

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; GarterSnake IR
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; An IRProgram is a ListOfIRDefinition

;; An IRDefinition is a (make-ir-definition NonNegInt IRExpression)
(define-struct ir-definition (nargs body))
;; Interpretation:
;; nargs is the number of arguments this function takes
;; body is an IRExpression that represents the body

;; An IRExpression is one of
;; -- (make-localvar-exp PosInt)
;; -- (make-globalvar-exp PosInt)
;; -- (make-undefinedvar-exp Variable)
;; -- (make-ir-app-exp IRExpression PosInt ListOfIRExpression)
;;    WHERE the first IRExpresion is the IR for a variable

(define-struct local-varexp (index))  
(define-struct global-varexp (index))
(define-struct undefined-varexp (name))
(define-struct ir-app-exp (fn args))

;; Interpretation:

;; (globalvar-exp i)
;; refers to the i-th function definition (starting at 1)

;; (localvar-exp i)
;; refers to the i-th argument of the current function (starting at 1); 

;; (undefined-varexp name)
;; is a reference to an undefined variable called 'name'.

;; (make-appexp i (list ir1 ... irn))
;; represents a call to the i-th function, with arguments represented
;; by ir1,..,irn.

;; SHADOWING RULES: In case a name is defined multiple times, names are
;; looked up in the following order:
;; 1. the name of the current function (a global variable)
;; 2. an argument of the current function (a local variable)
;; 3. the most recent function with that name.

;; These rules are chosen to be consistent with 07-3-gartersnake.rkt

;; EXAMPLE:

;; Consider the following program:

#|
def f1(x):f1(x)          
def f2(u,y):f1(y)        
def f3(x,u):f1(f2(u,f3)) 
def f4(x,f2):x(f2,z)     ; f2 refers to the local, not the function
|#

;; When we translate the definition of f3, we need to know that any
;; variables occurring in the body of f3 should be translated
;; according to the following rules:

#|
f1 -> (globalvar-exp 1)
f2 -> (globalvar-exp 2)
f3 -> (globalvar-exp 3)
x  -> (localvar-exp 1)
z  -> (localvar-exp 2)
name -> (undefinedvar-exp name) if name is anything else
|#

;; Thus the definition of f3 should be translated to

;; (make-ir-definition 2
;;                     (make-ir-app-exp (make-global-varexp 1)
;;                                      (list
;;                                       (make-ir-app-exp
;;                                        (make-global-varexp 2) 
;;                                        (list (make-local-varexp 2)
;;                                              (make-global-varexp 3)))
;;                                       (make-undefined-varexp 'z))))

;; Here is a more complete example:

#|
def f1(x):f1(x)          
def f2(u,y):f1(y)        
def f3(x,u):f1(f2(u,f3)) 
def f4(x,f2):x(f2,u)       ; f2 refers to the local, not the function
|#

(define pgm1
  (list
     (make-def 'f1 (list 'x) (make-appexp 'f1 (list (make-varexp 'x))))
     (make-def 'f2 (list 'u 'y) (make-appexp 'f1 (list (make-varexp 'y))))
     (make-def 'f3 (list 'x 'u)
               (make-appexp 'f1 (list (make-appexp 'f2
                                             (list (make-varexp 'u)
                                                   (make-varexp 'f3)))
                                      (make-varexp 'z))))
     (make-def 'f4 (list 'x 'f2)
               (make-appexp 'x
                            (list (make-varexp 'f2)
                                  (make-varexp 'u))))))

#|
(begin-for-test
  (check-equal?
   (translation-of-program pgm1)
   (list
    (make-ir-definition 1
                        (make-ir-app-exp (make-global-varexp 1)
                                         (list (make-local-varexp 1))))
    (make-ir-definition 2
                        (make-ir-app-exp (make-global-varexp 1)
                                         (list (make-local-varexp 2))))
    (make-ir-definition 2
                        (make-ir-app-exp (make-global-varexp 1)
                                         (list
                                          (make-ir-app-exp
                                           (make-global-varexp 2) 
                                           (list (make-local-varexp 2)
                                                 (make-global-varexp 3)))
                                          (make-undefined-varexp 'z))))
    (make-ir-definition 2
                        (make-ir-app-exp (make-local-varexp 1)
                                         (list (make-local-varexp 2)
                                               (make-undefined-varexp 'u)))))))

|#


