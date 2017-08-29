;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname 05.2-convert-currencies) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;; convert-currencies.rkt

(require rackunit)

;; convert-currencies : Rational ListOfRational -> ListOfRational
;; GIVEN: a conversion factor and a list of values in the first
;; currency,  
;; RETURNS: a list of the same values in the second currency. 

;; EXAMPLE:
;; (convert-currencies 1.18 (list 1 10 100)) = (list 1.18 11.18 111.80)

;; STRATEGY: Use HOF map on lst

(define (convert-currencies factor lst)
  (map
    (lambda (val) (* factor val))
    lst))

;; TEST
(check-equal?
 (convert-currencies 1.18 (list 1 10 100))
 (list 1.18 11.8 118.0)
 "convert-currencies failed on given example")

;; this should work, because Racket always computes exact results on
;; rational numbers.  To see this in action, change the 118.0 to
;; 118.000001 and look at the failure message.

;; If you were doing operations on irrational numbers, or functions
;; that return irrational numbers (like sqrt, sin, or cos), then you'd
;; have to use check-=.

;; Don't stress about the use of Rational numbers here.  I don't
;; believe they ever come up in the problem sets.
