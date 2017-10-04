#lang racket

(require rackunit)
(require "extras.rkt")

;; Data Definitions for Programs and Values.

(provide (struct-out program)
         (struct-out fcn-decl)
         (struct-out const-decl)
         (struct-out const) 
         (struct-out variable)
         (struct-out ifzero)
         (struct-out diff)
         (struct-out call)
         decl-name
         decl-value)

;; language:

#|
Rough concrete syntax:

program ::= decl* expr
decl ::= name arg* expr
decl ::= name number
expr ::= number | variable | if expr expr expr
      |  (expr - expr)
      |  (name expr expr ...)   ; function call

|#

;; A Program is a (make-program ListOfDecl Expr)

;; A Decl is one of
;; -- (make-fcn-decl Name ListOfName Expr)
;; -- (make-const-decl Name Number)

;; An Expr is one of
;; -- (make-const Number)
;; -- (make-variable Symbol)
;; -- (make-ifzero Expr Expr Expr)
;; -- (make-diff Expr Expr)
;; -- (make-call Name ListOfExpr)

;; A Value is one of
;; -- Number
;; -- (make-fcn-decl Name ListOfName Expr)

;; A Name is a Symbol

(define-struct program (decls main))
(define-struct fcn-decl (name args body))
(define-struct const-decl (name value))
(define-struct const (value))
(define-struct variable (name))
(define-struct diff (expr1 expr2))
(define-struct ifzero (test then else))
(define-struct call (name exprs))

;; We'll put a couple of help functions for declarations here:

;; Decl -> Name
(define (decl-name d)
  (cond
    [(fcn-decl? d) (fcn-decl-name d)]
    [(const-decl? d) (const-decl-name d)]))

;; Decl -> Value
(define (decl-value d)
  (cond
    [(fcn-decl? d) d]
    [(const-decl? d) (const-decl-value d)]))

(begin-for-test

  (local
    ((define decl1 (make-fcn-decl 'f '(x y)
                     (diff
                       (make-variable 'x)
                       (make-variable 'y))))
     (define decl2 (make-const-decl 'u 3)))
    (check-equal? (decl-name decl1) 'f)
    (check-equal? (decl-name decl2) 'u)
    (check-equal? (decl-value decl1) decl1)
    (check-equal? (decl-value decl2) 3))

)

                                              


