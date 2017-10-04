;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname sets) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;; sets
(require rackunit)
(require "extras.rkt")
(provide my-member? subset? set-equal? 
  set-cons set-union set-minus set-diff)

;; A SetOf<X> is a ListOf<X> WITH NO DUPLICATES

;; note: empty is a SetOf<X>

;; my-member? : X SetOf<X> -> Boolean
;; strategy: HO Function Combination
(define (my-member? x set1)
  (ormap
    (lambda (z) (equal? x z))
    set1))

(check-true (my-member? 3 (list 1 3 5)))
(check-false (my-member? 4 (list 1 3 5)))


;; subset? : SetOf<X> SetOf<X> -> Boolean
;; strategy: HO Function Combination
(define (subset? set1 set2)
  (andmap
    (lambda (x) (my-member? x set2))
    set1))


(check-true (subset? (list 1 3 5) (list 1 3 2 4 5 8)))
(check-false (subset? (list 1 3 5) (list 1 3 8)))


;; set-equal? : SetOf<X> SetOf<X> -> Boolean
(define (set-equal? set1 set2)
  (and
   (subset? set1 set2)
   (subset? set2 set1)))

(check-true (set-equal? (list 1 3 5) (list 3 5 1)))
(check-false (set-equal? (list 1 3 5) (list 1 3 4 5)))
(check-false (set-equal? (list 1 3 5) (list 1 3 5 7)))
(check-false (set-equal? (list 1 3 5 7) (list 1 3 5)))



;; these should have tests:


;; set-cons : X SetOf<X> -> SetOf<X>
(define (set-cons x set1)
  (if (my-member? x set1)
    set1
    (cons x set1)))

;; set-union : SetOf<X> SetOf<X> -> SetOf<X>
(define (set-union set1 set2)
  (foldr
    set-cons
    set2
    set1))

;; this starts with set2 and adds the elements of set1 one at a time.
;; This makes set-union work like append.

(check-equal?
  (set-union '(a b c) '(c b d))
  '(a c b d))

;; set-minus : SetOf<X> X -> SetOf<X>
(define (set-minus set1 x)
  (filter
    (lambda (elt) (not (equal? x elt)))
    set1))

;; set-diff : SetOf<X> SetOf<X> -> SetOf<X>
;; return all elements of set1 that are NOT elements of set2
(define (set-diff set1 set2)
  (filter
    (lambda (elt) (not (my-member? elt set2)))
    set1))






