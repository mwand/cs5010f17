;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname 04-1-lists) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; 04-1-lists.rkt

(require rackunit)
(require "extras.rkt")

;; A NumberList is represented as a list of Number.

;; CONSTRUCTOR TEMPLATE AND INTERPRETATION
;; empty                  -- the empty sequence
;; (cons n ns)
;;   WHERE:
;;    n  is a Number      -- the first number
;;                           in the sequence
;;    ns is a NumberList  -- the rest of the 
;;                           numbers in the sequence

;; OBSERVER TEMPLATE:
;; nl-fn : NumberList -> ??
(define (nl-fn lst)
  (cond
    [(empty? lst) ...]
    [else (... (first lst)
               (nl-fn (rest lst)))]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; examples of list calculations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(begin-for-test
  (check-equal? (empty? empty) true)
  (check-equal? (empty? (cons 11 empty)) false)
  (check-equal? (empty? (cons 22 (cons 11 empty))) false)

  (check-equal? (first (cons 11 empty)) 11)
  (check-equal? (rest  (cons 11 empty)) empty)

  (check-equal? (first (cons 22 (cons 11 empty))) 22)
  (check-equal? (rest  (cons 22 (cons 11 empty))) (cons 11 empty))

  (check-error (first empty)
    "first: expected argument of type <non-empty list>; given empty")
  (check-error (rest  empty)
    "rest: expected argument of type <non-empty list>; given empty")
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; nl-length : NumberList -> Integer
;; GIVEN: a NumberList
;; RETURNS: its length

;; If the tests are simple, you can use them as examples.
;; But they'd better be very very simple.

(begin-for-test
  (check-equal? (nl-length empty) 0)
  (check-equal? (nl-length (cons 11 empty)) 1)
  (check-equal? (nl-length (cons 33 (cons 11 empty))) 2))

; STRATEGY: Use observer template for NumberList on lst

(define (nl-length lst)
  (cond
    [(empty? lst) 0]
    [else (+ 1 
            (nl-length (rest lst)))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; nl-sum : NumberList -> Number
;; GIVEN: a NumberList
;; RETURNS: its sum

;; EXAMPLES/TESTS:
(begin-for-test
  (check-equal? (nl-sum empty) 0)
  (check-equal? (nl-sum (cons 11 empty)) 11)
  (check-equal? (nl-sum (cons 33 (cons 11 empty))) 44)
  (check-equal? (nl-sum (cons 10 (cons 20 (cons 3 empty)))) 33))

;; STRATEGY: Use template for NumberList on lst

(define (nl-sum lst)
  (cond
    [(empty? lst) 0]
    [else (+ (first lst)
             (nl-sum (rest lst)))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; nl-avg : NumberList -> Number
;; GIVEN: a non-empty NumberList
;; RETURNS: its average

(begin-for-test
  (check-equal? (nl-avg (cons 11 empty)) 11)
  (check-equal? (nl-avg (cons 33 (cons 11 empty))) 22)
  (check-equal? (nl-avg (cons 10 (cons 20 (cons 3 empty)))) 11))

; STRATEGY: combine simpler functions (!)

(define (nl-avg lst)
  (/ (nl-sum lst) (nl-length lst)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; double-all : NumberList -> NumberList
;; GIVEN: a NumberList
;; RETURNS: a list just like the original, but with each
;; number doubled 

(begin-for-test
  (check-equal? (double-all empty) empty)
  (check-equal? (double-all (cons 11 empty)) (cons 22 empty))
  (check-equal? 
   (double-all (cons 33 (cons 11 empty)))
   (cons 66 (cons 22 empty))))

;; STRATEGY: Use template for NumberList on lst

(define (double-all lst)
  (cond
    [(empty? lst) empty]
    [else (cons (* 2 (first lst))
                (double-all (rest lst)))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; For the next few examples, we will restrict ourselves to sequences
;; of integers.

;; DATA DEFINITION:

;; A sequence of integers (IntList) is represented as a list of integers.
;; CONSTRUCTOR TEMPLATES:
;; empty                -- the empty sequence
;; (cons n iseq)
;;   WHERE:
;;    n : Integer     is the first integer in the sequence
;;    iseq : IntList     is the the rest of the sequence

;; OBSERVER TEMPLATE:
;; il-fn : IntList -> ??
(define (il-fn s)
  (cond
    [(empty? s) ...]
    [else (... (first s)
               (il-fn (rest s)))]))

;; remove-evens : IntList -> IntList
;; GIVEN: a IntList
;; RETURNS: a sequence just like the original, but with all
;; the even numbers removed 

(define list-22-11-13-46-7 
    (cons 22 
          (cons 11 (cons 13 (cons 46 (cons 7 empty))))))
;; a list whose first even is not in the first position (not on the slides!)
(define list-17-22-11-13-46-7 (cons 17 list-22-11-13-46-7))

(begin-for-test
  (check-equal? (remove-evens empty) empty)
  (check-equal? (remove-evens (cons 11 empty)) (cons 11 empty))
  (check-equal?
   (remove-evens list-22-11-13-46-7)
   (cons 11 (cons 13 (cons 7 empty))))
  (check-equal?
    (remove-evens list-17-22-11-13-46-7)
    (cons 17 (cons 11 (cons 13 (cons 7 empty))))))

;; STRATEGY: Use template for IntList on lst

(define (remove-evens lst)
  (cond
    [(empty? lst) empty]
    [else (if (even? (first lst))
              (remove-evens (rest lst))
              (cons (first lst)
                    (remove-evens (rest lst))))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; remove-first-even : IntList -> IntList
;; GIVEN: a IntList
;; RETURNS: a list just like the original, but with the first even
;; number, if any, removed.

(begin-for-test
  (check-equal? (remove-first-even empty) empty)
  (check-equal? (remove-first-even (cons 11 empty)) (cons 11 empty))
  (check-equal? (remove-first-even list-22-11-13-46-7)
                (cons 11 (cons 13 (cons 46 (cons 7 empty)))))
  (check-equal?
    (remove-first-even list-17-22-11-13-46-7)
    (cons 17 (cons 11 (cons 13 (cons 46 (cons 7 empty)))))))

;; STRATEGY: Use template for IntList on lst
;; HALTING MEASURE: (length lst)

(define (remove-first-even lst)
  (cond
    [(empty? lst) empty]
    [else (if (even? (first lst))
              (rest lst)
              (cons (first lst)
                    (remove-first-even
                     (rest lst))))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; A SortedIntList is an IntList in which the numbers are sorted in ascending
;; order.

;; A sorted sequence of integers (SortedIntList) is represented as a list
;; of integers. 

;; CONSTRUCTOR TEMPLATES:
;; empty                -- the empty sequence
;; (cons n seq)
;;   WHERE:
;;    n : Integer         is the first integer in the sequence
;;    seq : SortedIntList     is the the rest of the sequence
;;   AND:
;;    n is less than any number in seq.

;; OBSERVER TEMPLATE:
;; Same as for IntList.


;; Example 6: insert

;; insert : Integer SortedIntList -> SortedIntList
;; GIVEN: An integer and a sorted sequence
;; RETURNS: A new SortedIntList just like the original, but with the
;; new integer inserted.
;; EXAMPLES:
;; (insert 3 empty) = (list 3)
;; (insert 3 (list 5 6)) = (list 3 5 6)
;; (insert 3 (list -1 1 5 6)) = (list -1 1 3 5 6)
;; STRATEGY: Use observer template for SortedIntList

(define (insert n seq)
  (cond
    [(empty? seq) (cons n empty)]
    [(< n (first seq)) (cons n seq)]
    [else (cons (first seq)
                (insert n (rest seq)))]))

(begin-for-test
  (check-equal? (insert 3 empty)  (list 3))
  (check-equal? (insert 3 (list 5 6))  (list 3 5 6))
  (check-equal? (insert 3 (list -1 1 5 6))  (list -1 1 3 5 6)))

;; Note: this can take time proportional to the length of 'seq'.

;; Example 7: insertion sort

;; sort is predefined in ISL+lambda, so we need a different name

;; mysort : IntList -> SortedIntList
;; GIVEN: An integer sequence
;; RETURNS: The same sequence, only sorted by <= .
;; EXAMPLES:
;; (mysort empty) = empty
;; (mysort (list 3)) = (list 3)
;; (mysort (list 2 1 4)) = (list 1 2 4)
;; (mysort (list 2 1 4 2)) = (list 1 2 2 4)
;; STRATEGY: Use observer template for IntList

(define (mysort ints)
  (cond
    [(empty? ints) empty]
    [else (insert (first ints)
                  (mysort (rest ints)))]))

(begin-for-test
  (check-equal? (mysort empty) empty)
  (check-equal? (mysort (list 3)) (list 3))
  (check-equal? (mysort (list 2 1 4)) (list 1 2 4))
  (check-equal? (mysort (list 2 1 4 2)) (list 1 2 2 4)))
