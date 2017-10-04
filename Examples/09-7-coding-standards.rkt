#lang racket

;; illustrations for coding examples

(define Foo<%>
  (interface ()

    ; -> Integer
    ; purpose statement omitted...
    m1

    ; Bar -> Foo
    ; purpose statement omitted...
    add-bar))

(define Class1%
  (class* object% (Foo<%>)

    (init-field ...)
    (field [LOCAL-CONSTANT ...])
    (super-new)

    ; m1 : -> Integer
    ; purpose statement omitted...
    (define/public (m1) ...)

    ; add-bar : Bar -> Foo
    (define/public (add-bar b) ...)

    ;; (define/public (method-not-in-interface ...) ...)


    (define (private-function1 ...) ...)
    (define (private-function2 ...) ...)

    ;; for-test:... methods don't need to be in the interface

    (define/public (for-test:test-fcn1 ...) ...)

    ))