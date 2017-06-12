;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname 08-4-fib) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require rackunit)
(require "extras.rkt")

;; General-Recursion Template for fib:

#|
;; General Recursion Template for decode:
(define (solution the-problem)
  (cond
    [(trivial1? the-problem) (trivial-solution1 the-problem)]
    [(trivial2? the-problem) (trivial-solution2 the-problem)]
    [(difficult? the-problem)
     (local
       ((define new-problem1 (simpler-instance1 the-problem))
        (define new-problem2 (simpler-instance2 the-problem)))
       (combine-solutions
        (solve new-problem1)
        (solve new-problem2)))]))
|#


;; fib : NonNegInt -> NonNegInt
;; GIVEN: a non-negative integer n
;; RETURNS: the n-th fibonacci number
;; EXAMPLES: 
;; fib(0) = 1
;; fib(1) = 1
;; fib(2) = 2
;; fib(3) = 3
;; fib(4) = 5
;; fib(5) = 8
;; STRATEGY: Recur on n-1 and n-2
;; HALTING MEASURE: n
(define (fib n)
  (cond
   [(= n 0) 1]
   [(= n 1) 1]
   [else (+ (fib (- n 1))
            (fib (- n 2)))]))

;; NonNegInt NonNegInt -> Boolean
;; RETURNS: true iff (fib in) = out
(define (fib-test in out)
  (check-equal? (fib in) out))

(begin-for-test
  (fib-test 0 1)
  (fib-test 1 1)
  (fib-test 2 2)
  (fib-test 3 3)
  (fib-test 4 5)
  (fib-test 5 8)
  (fib-test 6 13))


