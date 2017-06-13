;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname 04-3-nats) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; Recursion on the Natural Numbers

(require rackunit)
(require "extras.rkt")

;; A Natural Number (Nat) is one of 
;; -- 0
;; -- (add1 Nat)

;; nat-fn : Nat -> ??
;; HALTING MEASURE: the value of n
#|
(define (nat-fn n)
 (cond
  [(zero? n) ...]
  [else (... n (nat-fn (sub1 n)))]))
|#


;; double : Nat -> Nat
;; RETURNS: 2*n
;; STRATEGY: use template for Nat on n
;; HALTING MEASURE: the value of n
(define (double n)
  (cond
    [(zero? n) 0]
    [else (+ 2 (double (sub1 n)))]))



;; sum : Nat Nat -> Nat
;; RETURNS: the sum of its arguments
;; STRATEGY: use template for Nat on x
;; HALTING MEASURE: the value of x
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
 ;; STRATEGY: use template for Nat on y
 ;;
(define (prod x y)
  (cond
    [(zero? y) 0]
    [else 
      (sum x (prod x (sub1 y)))]))

(begin-for-test
   (check-equal? (prod 0 4) 0)
   (check-equal? (prod 3 2) (* 3 2))
   (check-equal? (prod 27 18) (* 27 18)))


 

 
