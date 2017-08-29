;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname 04-4-other-nats) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; Recursion on the Natural Numbers

(require rackunit)
(require "extras.rkt")

;; An LNat is a natural number, represented as a list of "true"s.

;; CONSTRUCTOR TEMPLATES:
;; -- empty                represents 0
;; -- (cons true ln)       
;;       WHERE ln is a LNat

;; INTERP:
;; empty represents 0
;; if ln represents n, then (cons true ln)
;;                         represents n+1

;; OBSERVER TEMPLATE:

;; nat-fn : Nat -> ??
(define (nat-fn n)
 (cond
  [(empty? n) ...]
  [else (...
         n
         (nat-fn (rest n)))]))

;; useful help functions

;; succ : LNat -> LNat
;; GIVEN: an LNat representing n
;; RETURNS: an LNat representing n+1

(define (succ ln) (cons true ln))

;; pred : LNat -> LNat
;; GIVEN: an LNat representing a natural number n > 0
;; RETURNS: an LNat representing n-1

(define (pred ln) (rest ln))

;; nat-to-lnat : Nat -> LNat
;; GIVEN: A Nat n
;; RETURNS: An LNat representing n
;; EXAMPLES:
;; (nat-to-lnat 0) = empty
;; (nat-to-lnat 3) = (list true true true)
;; STRATEGY: Use template for Nat on n

;; COMMENT: We will use this only to construct examples

(define (lnat n)
  (cond
    [(zero? n) empty]
    [else (cons true (lnat (sub1 n)))]))

;;;;;;;;;;;;;;;; EXAMPLES ;;;;;;;;;;;;;;;;

;; ldouble : LNat -> LNat
;; GIVEN: An LNat representing n
;; RETURNS: An LNat representing 2*n
;; STRATEGY: use observer template for LNat on n

(define (double n)
  (cond
    [(empty? n) 0]
    [else (succ (succ (double (pred n))))]))



;; sum : LNat LNat -> LNat
;; RETURNS: an LNat representing the sum of its arguments
;; STRATEGY: use observer template for LNat on x

(define (sum x y)
 (cond
   [(empty? x) y]
   [else (succ (sum (pred x) y))]))

 (begin-for-test
   (check-equal? (sum (lnat 0) (lnat 4)) (lnat 4))
   (check-equal? (sum (lnat 3) (lnat 2)) (lnat (+ 3 2)))
   (check-equal? (sum (lnat 27) (lnat 18)) (lnat (+ 27 18))))


;; prod : LNat LNat -> LNat
;; RETURNS: An LNat representing the product of its arguments
;; STRATEGY: use observer template for LNat on y

(define (prod x y)
  (cond
    [(empty? y) empty]
    [else 
      (sum x (prod x (pred y)))]))

(begin-for-test
   (check-equal? (prod (lnat 0) (lnat 4)) (lnat 0))
   (check-equal? (prod (lnat 3) (lnat 2)) (lnat (* 3 2)))
   (check-equal? (prod (lnat 27) (lnat 18)) (lnat (* 27 18))))


 

 
