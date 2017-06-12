;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname 08-2-merge-sort) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; general-recursion for merge, merge-sort

(require rackunit)
(require "extras.rkt")

;; merge : SortedList SortedList -> SortedList
;; merges its two arguments
;; STRATEGY: recur on either (rest lst1) or (rest lst2)
;; HALTING MEASURE: length of lst1 + length of lst2

(define (merge lst1 lst2)
  (cond
    [(empty? lst1) lst2]
    [(empty? lst2) lst1]
    [(< (first lst1) (first lst2))
     (cons (first lst1) (merge (rest lst1) lst2))]
    [else
     (cons (first lst2) (merge lst1 (rest lst2)))]))


;; merge-sort : ListOfNumber -> SortedList
;; GIVEN: a list of numbers
;; RETURNS: a sorted version of the same list
;; EXAMPLE:
;; empty => empty
;; (list 4 2 6 7 6 8) => (list 2 4 6 6 7 8)
;; STRATEGY: recur on even elements and odd elements, then merge
;; results.

(define (merge-sort lon)
  (cond
    [(empty? lon) lon]
    [(empty? (rest lon)) lon]
    [else
      (local
       ((define evens (even-elements lon))
        (define odds  (odd-elements lon)))
       (merge 
        (merge-sort evens)
        (merge-sort odds)))]))


;; INSERT EVEN-ELEMENTS AND ODD-ELEMENTS HERE

(begin-for-test
  (check-equal? (merge-sort  '()) empty)
  (check-equal? (merge-sort (list 4 2 6 7 6 8)) (list 2 4 6 6 7 8)))

