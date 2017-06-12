#lang racket

(require "extras.rkt")
(require rackunit)

;; examples from Lesson 9.1

(define Interface1<%>
  (interface ()
    foo  ; -> Int
    bar  ; Int -> Int
    baz  ; Int -> Int
    ))


 
(define Class1%
  (class* object% (Interface1<%>)
    
    (init-field x y r)  ;;  x,y,r : Int

    (super-new)   ;; required magic

    ;; foo : -> Int
    (define/public (foo) (+ x y))

    ;; bar : Int -> Int
    (define/public (bar n) (+ r n))
    
    ;; baz : Int -> Int
    (define/public (baz n) 
      (+ (send this foo) n))

    ))

(define Class2%
  (class* object% (Interface1<%>)
    
    (init-field a b c)  ; a, b, c : Int

    (super-new)
    
    ;; foo : -> Int
    (define/public (foo) (+ a b))
    
    ;; bar : Int -> Int
    (define/public (bar n) (* c n))
    
    ;; baz : Int -> Int
    (define/public (baz n) 
      (+ (send this foo) n))

    ))

(define Class2a%
  (class* object% (Interface1<%>)
    
    (init-field a b c)  ; a, b, c : Int

    (field [a1 (- a)])   ; add a new field, initialized to -a

    (super-new)
    
    ;; foo : -> Int
    ; (define/public (foo) (+ a b))
    (define/public (foo) (- b a1))
    
    ;; bar : Int -> Int
    (define/public (bar n) (* c n))
    
    ;; baz : Int -> Int
    (define/public (baz n) 
      (+ (send this foo) n))

    ))



(define obj1  (new Class1% [x 10][y 20][r 10]))
(define obj2  (new Class1% [x 15][y 35][r 5]))
(define obj3  (new Class2% [a 15][b 35][c 5]))
(define obj3a (new Class2a% [a 15][b 35][c 5]))

(begin-for-test
  
  (check-equal? (send obj1 foo) 30)
  (check-equal? (send obj2 foo) 50)
  
  (check-equal? (send obj1 bar 8) 18)
  (check-equal? (send obj2 bar 8) 13)

  (check-equal? (send obj1 baz 20) 50)
  (check-equal? (send obj2 baz 20) 70)

  (check-equal? (send obj2 bar 8) 13)
  (check-equal? (send obj3 bar 8) 40)

  ;; foo is the only method that is different in Class2% and Class2a%.
  (check-equal? (send obj3 foo) (send obj3a foo))


  )
  
  

