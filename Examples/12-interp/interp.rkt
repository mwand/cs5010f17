#lang racket

(require "datatypes.rkt")
(require "environments.rkt")

(provide pgm-value)           ;; Program -> Value

;; Program -> Value
;; RETURNS: the value of the program
(define (pgm-value pgm)
  (expr-value
    (program-main pgm)
    (decls2env (program-decls pgm))))

;; Expr Env -> Value
;; RETURNS: the value of the given expression in the given environment
(define (expr-value exp env)
  (cond
    [(const? exp) (const-value exp)]
    [(variable? exp) (env-lookup env (variable-name exp))]
    [(diff? exp) (- (expr-value (diff-expr1 exp) env)
                   (expr-value (diff-expr2 exp) env))]
    [(ifzero? exp)
     (if (zero? (expr-value (ifzero-test exp) env))
       (expr-value (ifzero-then exp) env)
       (expr-value (ifzero-else exp) env))]
    [(call? exp)
     (local
       ((define the-fcn (env-lookup env (call-name exp)))
        (define the-args 
          (map
            (lambda (arg-expr) (expr-value arg-expr env))
            (call-exprs exp))))
       (expr-value
         (fcn-decl-body the-fcn)
         (make-env
           (fcn-decl-args the-fcn)
           the-args
           env)))]))    ; this is the _dynamic_ environment-- which is
                      ; wrong in general.  Most languages, including
                      ; Racket, use a _static_ environment.  Dealing
                      ; with the static environment isn't hard, but
                      ; requires more than 1 hr to develop.
    



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Utility Functions.  
;;; These are here because they know about declarations.

;; decls2env : Decls -> Env
(define (decls2env decls)
  (make-env
    (map decl-name decls)
    (map decl-value decls)
    empty-env))





