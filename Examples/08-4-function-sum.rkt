;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname 08-5-function-sum) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; function-sum.rkt

(require rackunit)
(require "extras.rkt")

;; function-sum : 
;;   Nat Nat (Nat Nat -> Number) -> Number
;; GIVEN: natural numbers lo <= hi and a function f,
;; RETURNS: SUM{f(j) | lo <= j <= hi}

;; NOTE: Nat ("natural number") is just another name for NonNegInt.
;; You can use either name in your files; just be consistent.

;; EXAMPLES/TESTS:
(begin-for-test
  (check-equal?
    (function-sum 1 3 (lambda (j) j))
    (+ 1 2 3))
  (check-equal?
    (function-sum 1 3 (lambda (j) (+ j 10)))
    (+ 11 12 13))
  (check-equal?
    (function-sum 1 3 sqr)
    (+ 1 4 9)))

;; before defining function-sum, we'll write the generalized version:

;; generalized-function-sum : 
;;   Nat Nat Number (Nat Nat -> Number) -> Number
;; GIVEN: natural numbers i <= hi , a number sofar, and a function f,
;; RETURNS: sofar + SUM{f(j) | i <= j <= hi} 

;; EXAMPLES/TESTS:
(begin-for-test
  (check-equal?
    (generalized-function-sum 1 3 17 (lambda (j) j))
    (+ 17 (+ 1 2 3)))
  (check-equal?
    (generalized-function-sum 1 3 42 (lambda (j) (+ j 10)))
    (+ 42 (+ 11 12 13))))

;; STRATEGY: If i < hi, recur on i+1.
;; COMMENT: adapt-solution is the identity function
;; HALTING MEASURE: (- hi i)

(define (generalized-function-sum i hi sofar f)
  (cond
    [(= i hi) (+ sofar (f i))]
    [else (generalized-function-sum
            (+ i 1)
            hi
            (+ sofar (f i))
            f)]))

;; now we can define function-sum:
;; STRATEGY: Call more general function
(define (function-sum lo hi f)
  (generalized-function-sum lo hi 0 f))
