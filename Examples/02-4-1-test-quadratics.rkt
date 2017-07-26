;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname 02-4-1-test-quadratics) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require rackunit)
(require "extras.rkt")

;; No data definitions here because we're just doing math.

;; zero-of-quadratic? : Real^4 -> Boolean
;; RETURNS: whether ax^2 + bx + c < tolerance
(define (zero-of-quadratic? a b c x tol)
  (< (abs (+ (* a x x)
             (* b x)
             c))
     tol))

(define (quadratic-solution1 a b c)
  (/ (+ (- b) (sqrt (- (* b b) (* 4 a c))))
    (* 2 a)))

(define (quadratic-solution2 a b c)
  (/ (- (- b) (sqrt (- (* b b) (* 4 a c))))
    (* 2 a)))

(begin-for-test
  (check-true (zero-of-quadratic? 1 0 4 (quadratic-solution1 1 0 4) .01))
  (check-true (zero-of-quadratic? 1 0 4 (quadratic-solution2 1 0 4) .01))
  (check-true (zero-of-quadratic? 13 58 6 (quadratic-solution1 13 58 6) .01))
  )



