;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname 04-1-lists) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; 04-1-lists.rkt

(require rackunit)
(require "extras.rkt")

;; A NumberSeq is represented as a list of Number.

;; CONSTRUCTOR TEMPLATE:
;; empty                  -- the empty sequence
;; (cons n ns)
;;   WHERE:
;;    n  is a Number      -- the first number
;;                           in the sequence
;;    ns is a NumberSeq   -- the rest of the 
;;                           numbers in the sequence

;; OBSERVER TEMPLATE:
;; ns-fn : NSeq -> ??
(define (ns-fn lst)
  (cond
    [(empty? lst) ...]
    [else (... (first lst)
               (ns-fn (rest lst)))]))


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

;; ns-length : NSeq -> Integer
;; GIVEN: a NSeq
;; RETURNS: its length

;; If the tests are simple, you can use them as examples.
;; But they'd better be very very simple.

(begin-for-test
  (check-equal? (ns-length empty) 0)
  (check-equal? (ns-length (cons 11 empty)) 1)
  (check-equal? (ns-length (cons 33 (cons 11 empty))) 2))

; STRATEGY: Use observer template for NSeq on lst

(define (ns-length lst)
  (cond
    [(empty? lst) 0]
    [else (+ 1 
            (ns-length (rest lst)))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ns-sum : NSeq -> Number
;; GIVEN: a NSeq
;; RETURNS: its sum

;; EXAMPLES/TESTS:
(begin-for-test
  (check-equal? (ns-sum empty) 0)
  (check-equal? (ns-sum (cons 11 empty)) 11)
  (check-equal? (ns-sum (cons 33 (cons 11 empty))) 44)
  (check-equal? (ns-sum (cons 10 (cons 20 (cons 3 empty)))) 33))

;; STRATEGY: Use template for NSeq on lst

(define (ns-sum lst)
  (cond
    [(empty? lst) 0]
    [else (+ (first lst)
             (ns-sum (rest lst)))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ns-avg : NSeq -> Number
;; GIVEN: a non-empty NSeq
;; RETURNS: its average

(begin-for-test
  "ns-avg tests"
  (check-equal? (ns-avg (cons 11 empty)) 11)
  (check-equal? (ns-avg (cons 33 (cons 11 empty))) 22)
  (check-equal? (ns-avg (cons 10 (cons 20 (cons 3 empty)))) 11))

; STRATEGY: combine simpler functions (!)

(define (ns-avg lst)
  (/ (ns-sum lst) (ns-length lst)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; double-all : NSeq -> NSeq
;; GIVEN: a NSeq
;; RETURNS: a list just like the original, but with each
;; number doubled 

(begin-for-test
  (check-equal? (double-all empty) empty)
  (check-equal? (double-all (cons 11 empty)) (cons 22 empty))
  (check-equal? 
   (double-all (cons 33 (cons 11 empty)))
   (cons 66 (cons 22 empty))))

;; STRATEGY: Use template for NSeq on lst

(define (double-all lst)
  (cond
    [(empty? lst) empty]
    [else (cons (* 2 (first lst))
                (double-all (rest lst)))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; For the next few examples, we will restrict ourselves to sequences
;; of integers.

;; DATA DEFINITION:

;; A sequence of integers (ISeq) is represented as a list of integers.
;; CONSTRUCTOR TEMPLATES:
;; empty                -- the empty sequence
;; (cons n iseq)
;;   WHERE:
;;    n : Integer     is the first integer in the sequence
;;    iseq : ISeq     is the the rest of the sequence

;; OBSERVER TEMPLATE:
;; seq-fn : ISeq -> ??
(define (seq-fn s)
  (cond
    [(empty? s) ...]
    [else (... (first s)
               (list-fn (rest s)))]))

;; remove-evens : ISeq -> ISeq
;; GIVEN: a ISeq
;; RETURNS: a sequence just like the original, but with all
;; the even numbers removed 

(define seq-22-11-13-46-7 
    (cons 22 
          (cons 11 (cons 13 (cons 46 (cons 7 empty))))))
;; a list whose first even is not in the first position (not on the slides!)
(define seq-17-22-11-13-46-7
  (cons 17 seq-22-11-13-46-7))

(begin-for-test
  (check-equal? (remove-evens empty) empty)
  (check-equal? (remove-evens (cons 11 empty)) (cons 11 empty))
  (check-equal?
   (remove-evens seq-22-11-13-46-7)
   (cons 11 (cons 13 (cons 7 empty))))
  (check-equal?
    (remove-evens seq-17-22-11-13-46-7)
    (cons 17 (cons 11 (cons 13 (cons 7 empty))))))

;; STRATEGY: Use template for ISeq on lst
;; HALTING MEASURE: (length lst)

(define (remove-evens lst)
  (cond
    [(empty? lst) empty]
    [else (if (even? (first lst))
              (remove-evens (rest lst))
              (cons (first lst)
                    (remove-evens (rest lst))))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; remove-first-even : ISeq -> ISeq
;; GIVEN: a ISeq
;; RETURNS: a list just like the original, but with the first even
;; number, if any, removed.

(begin-for-test
  (check-equal? (remove-first-even empty) empty)
  (check-equal? (remove-first-even (cons 11 empty)) (cons 11 empty))
  (check-equal? (remove-first-even seq-22-11-13-46-7)
                (cons 11 (cons 13 (cons 46 (cons 7 empty)))))
  (check-equal?
    (remove-first-even seq-17-22-11-13-46-7)
    (cons 17 (cons 11 (cons 13 (cons 46 (cons 7 empty)))))))

;; STRATEGY: Use template for ISeq on lst
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

;; A SortedSeq is an ISeq in which the numbers are sorted in ascending
;; order.

;; A sorted sequence of integers (SortedSeq) is represented as a list
;; of integers. 

;; CONSTRUCTOR TEMPLATES:
;; empty                -- the empty sequence
;; (cons n seq)
;;   WHERE:
;;    n : Integer         is the first integer in the sequence
;;    seq : SortedSeq     is the the rest of the sequence
;;   AND:
;;    n is less than any number in seq.

;; OBSERVER TEMPLATE:
;; Same as for ISeq.


;; Example 6: insert

;; insert : Integer SortedSeq -> SortedSeq
;; GIVEN: An integer and a sorted sequence
;; RETURNS: A new SortedSeq just like the original
;; EXAMPLES:
;; (insert 3 empty) = (list 3)
;; (insert 3 (list 5 6)) = (list 3 5 6)
;; (insert 3 (list -1 1 5 6)) = (list -1 1 3 5 6)
;; STRATEGY: Use observer template for SortedSeq

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

;; mysort : IntSeq -> SortedSeq
;; GIVEN: An integer sequence
;; RETURNS: The same sequence, only sorted by <= .
;; EXAMPLES:
;; (mysort empty) = empty
;; (mysort (list 3)) = (list 3)
;; (mysort (list 2 1 4)) = (list 1 2 4)
;; (mysort (list 2 1 4 2)) = (list 1 2 2 4)
;; STRATEGY: Use observer template for IntSeq

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
