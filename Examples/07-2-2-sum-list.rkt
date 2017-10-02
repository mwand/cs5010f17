;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname 07-2-2-sum-list) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

;; list-sum (left-to-right, with context argument)

(require rackunit)
(require "extras.rkt")

;; sublist-sum : Number NumberList -> Number
;; GIVEN: a number 'so-far' and a list of numbers 'unsummed'
;; WHERE: 'unsummed' is a sublist of some list 'whole-list'
;; AND:   so-far is the sum of all the elements to the left of
;;        unsummed in whole-list
;; EXAMPLE:
;; (sublist-sum 5 (list 2 3 4)) = 14  [whole-list was (3 2 2 3 4)]
;; (sublist-sum 5 (list 2 3 4)) = 14  [whole-list was (3 1 1 2 3 4)]
;; note that a given set of arguments might correspond to different
;; values of 'whole-list'.  All we care about whole-list is that the
;; sum of its elements before the (list 2 3 4) is exactly 5.
;; STRATEGY:
;; observer pattern for NumberList on 'unsummed'
(define (sublist-sum so-far unsummed)
  (cond
    [(empty? unsummed) so-far]
    [else (sublist-sum (+ so-far (first unsummed))
                       (rest unsummed))]))

;; list-sum : NumberList -> Number
;; GIVEN: a list l
;; RETURNS: the sum of the numbers in l
;; EXAMPLE:
;; (list-sum (3 2 2 3 4)) = 14
;; (list-sum (3 1 1 2 3 4)) = 14
;; STRATEGY:
;; Call a more general function
(define (list-sum l)
  (sublist-sum 0 l))

(begin-for-test
  (check-equal? (sublist-sum 5 (list 2 3 4)) 14)
  (check-equal? (sublist-sum 5 (list 2 3 4)) 14)
  (check-equal? (list-sum (list 3 2 2 3 4)) 14)
  (check-equal? (list-sum (list 3 1 1 2 3 4)) 14))
