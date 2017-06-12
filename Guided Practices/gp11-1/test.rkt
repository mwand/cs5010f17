#lang racket

(require rackunit)

(define Class1%
  (class* object% ()
    (init-field a)
    (field [c (+ a 5)])

    (super-new)
    
    ;; Number -> Number
    (define/public (m1 x)
      (if (>= x 106) x
        (send this m2 (- x a))))
    
    ;; Number -> Number
    (define/public (m2 x) (+ x c))

    ))

(define Class2%
  (class* Class1% ()
    (inherit-field a c)
    (init-field b)

    (super-new [a (+ b 1)])

    ;; Number -> Number
    (define/override (m1 x) (- x c))
    
    ))

(define Class3%
  (class* Class1% ()
    (inherit-field a c)
    
    (super-new)

    ;; Number -> Number
    (define/override (m2 x) (send this m1 (+ x c)))

    ))

(define Class4%
  (class* Class2% ()
    (inherit-field a c)
    (super-new)
    (define/override (m2 x) (+ x c)) ))

(define obj1 (new Class1% [a 10]))
(define obj2 (new Class2% [b 20]))
(define obj3 (new Class3% [a 30]))
(define obj4 (new Class4% [b 40]))


(check-equal? (send obj1 m1 100) 105)
(check-equal? (send obj1 m2 100) 115)


(check-equal? (send obj2 m1 100) 74)
(check-equal? (send obj2 m2 100) 126)

(check-equal? (send obj3 m1 100) 110)
(check-equal? (send obj3 m2 100) 135)

(check-equal? (send obj4 m1 100) 54)
(check-equal? (send obj4 m2 100) 146)
