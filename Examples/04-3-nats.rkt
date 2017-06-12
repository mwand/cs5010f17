;; Recursion on the Natural Numbers

(require rackunit)
(require "extras.rkt")

;; A Nat is a natural number, represented as a Racket integer.

;; CONSTRUCTOR TEMPLATES:
;; -- 0                           
;; -- (add1 n)  WHERE n is a Nat
;; INTERP: self-evident

;; OBSERVER TEMPLATE:

;; nat-fn : Nat -> ??
(define (nat-fn n)
 (cond
  [(zero? n) ...]
  [else (...
         n
         (nat-fn (sub1 n)))]))

;;;;;;;;;;;;;;;; EXAMPLES ;;;;;;;;;;;;;;;;

;; double : Nat -> Nat
;; RETURNS: 2*n
;; STRATEGY: use observer template for Nat on n

(define (double n)
  (cond
    [(zero? n) 0]
    [else (+ 2 (double (sub1 n)))]))



;; sum : Nat Nat -> Nat
;; RETURNS: the sum of its arguments
;; STRATEGY: use observer template for Nat on x

(define (sum x y)
 (cond
   [(zero? x) y]
   [else (add1 (sum (sub1 x) y))]))

 (begin-for-test
   (check-equal? (sum 0 4) 4)
   (check-equal? (sum 3 2) (+ 3 2))
   (check-equal? (sum 27 18) (+ 27 18)))


;; prod : Nat Nat -> Nat
;; RETURNS: the product of its arguments
;; STRATEGY: use observer template for Nat on y

(define (prod x y)
  (cond
    [(zero? y) 0]
    [else 
      (sum x (prod x (sub1 y)))]))

(begin-for-test
   (check-equal? (prod 0 4) 0)
   (check-equal? (prod 3 2) (* 3 2))
   (check-equal? (prod 27 18) (* 27 18)))

;; fact : Nat -> Nat
;; GIVEN: a natural number n
;; RETURNS: its factorial

(define (fact n)
  (cond
    [(zero? n) 1]
    [else (prod n (fact (sub1 n)))]))


 

 
