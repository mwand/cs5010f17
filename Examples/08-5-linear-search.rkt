;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname 08-2-linear-search) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(require rackunit)
(require "extras.rkt")

;; linear-search : Nat Nat [Nat -> Bool] -> Maybe<Nat>
;; GIVEN: 2 natural numbers lo and hi, and a predicate pred
;; WHERE: lo <= hi
;; RETURNS: the smallest number in [lo,hi) that satisfies pred, or false if 
;; there is none. 
;; EXAMPLES/TESTS
(begin-for-test
  (check-equal?
    (linear-search 7 11 even?) 8)
  (check-false
    (linear-search 2 4 (lambda (n) (> n 6)))))

;; strategy: recur on (+ lo 1)
;; halting measure: (- hi lo)

(define (linear-search lo hi pred)
  (cond
    [(= lo hi) false]
    [(pred lo) lo]
    [else (linear-search (+ lo 1) hi pred)]))
