;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname 07-1-number-list-with-stress-tests) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))

(require rackunit)
(require "extras.rkt")

;; comment out all versions of number-list except the one you want,
;; then say (run-stress-tests 0)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; DATA DEFINITIONS

;; A NumberedX       is a (list Int X)
;; A NumberedListOfX is a ListOfNumberedX

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The contract and purpose statement:

;; number-list : ListOfX -> NumberedListOfX
;; RETURNS: a list like the original, but with the
;;    elements numbered consecutively, starting
;;    from 1

;; EXAMPLE:
;; (number-list (list 66 88 77))
;;   = (list (list 1 66) (list 2 88) (list 3 77))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Solution 1: write a more general function that can number a list
;; starting from any number.

;; number-list-from : ListOfX Number -> NumberedListOfX
;; RETURNS: a list with same elements as lst, but numbered starting at n.
;; EXAMPLE: (number-list-from (list 88 77) 2) 
;;          = (list (list 2 88) (list 3 77))
;; STRATEGY: Use the template for ListOfX on lst

(define (number-list-from lst n)
  (cond
    [(empty? lst) empty]
    [else
      (cons
        (list n (first lst))
        (number-list-from
          (rest lst)
          (+ n 1)))]))

(check-equal? 
  (number-list-from (list 88 77) 2) 
  (list (list 2 88) (list 3 77)))

;; now we can specialize number-list-from to get number-list:

;; STRATEGY: Call a more general function

(define (number-list1 lst)
  (number-list-from lst 1))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Solution 2: Version with invariant

;; same code, different purpose statement

;; number-list-from : ListOfX Number -> NumberedListOfX
;; GIVEN a sublist slst
;; WHERE slst is the n-th sublist of some list lst0
;; RETURNS: a copy of slst numbered according to its position in lst0.
;; STRATEGY: Use the template for ListOfX on lst

(define (number-sublist slst n)
  (cond
    [(empty? slst) empty]
    [else
      (cons
        (list n (first slst))
        (number-sublist (rest slst) (+ n 1)))]))

(define (number-list2 lst) (number-sublist lst 1))
       
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Solution 3: pure structural decomposition

;; STRATEGY: Use template for ListOfX on lst
(define (number-list3 lst)
  (cond
    [(empty? lst) empty]
    [else (number-list-combiner
            (first lst)
            (number-list3 (rest lst)))]))


;; number-list-combiner : X NumberedListOfX -> NumberedListOfX
;; GIVEN: an X x1 and a NumberedListOfX ((1 x2) (2 x3) ...)
;; RETURNS: the list ((1 x1) (2 x2) (3 x3) ...)
;; STRATEGY: HOFC
(define (number-list-combiner first-val numbered-list)
  (cons
    (list 1 first-val)
    (map
      ;; NumberedX -> NumberedX
      ;; GIVEN: a list containing a number and a value
      ;; RETURNS: a list like the given one, but with the number
      ;; increased by one.
      (lambda (elt)
        (list
          (+ 1 (first elt))
          (second elt)))
      numbered-list)))

(begin-for-test
  (check-equal?
    (number-list-combiner 11 (list (list  1 33) (list  2 13) (list 3 42)))
    (list (list 1 11) (list 2 33) (list 3 13) (list  4 42))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; these next tests illustrate functions that build Checks, and also
;; tests that test properties of the answer rather than the whole answer

;; A Check is a (check-equal? ComputedAnswer CorrectAnswer ErrorMessage)

;; ListOfX -> Check
;; Given a list lst, check to see that the first elements of
;; (number-list lst) are (1 ... (length lst))
(define (make-test1 lst)
  (check-equal?
   (map first (number-list lst))
   (build-list (length lst) add1)
   (format "first elements of (number-list ~s) should be 1 through ~s"
     lst
     (add1 (length lst)))))

(define (number-list lst) (number-list1 lst))

;; ListOfX -> Check
;; Given a list lst, check to see that the second elements of
;; (number-list lst) are the same as lst
(define (make-test2 lst)
  (check-equal?
   (map second (number-list lst))
   lst))

(begin-for-test
  (check-equal? (number-list empty)
                empty
                "(number-list empty) should be empty")
  (check-equal? (number-list '(a b c)) 
                '((1 a) (2 b) (3 c))
                "(number-list '(a b c)) should be ((1 a) (2 b) (3 c))")
  (make-test1 '(11 22 33 44))
  (make-test2 '(11 22 33 44))
  (make-test1 empty)
  (make-test2 empty))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; stress tests

(require racket)   
;; included for getting time out of stress tests.  Don't let me catch
;; you requiring racket. 

;; (X -> Any) (X -> Any) X -> (list Nat Nat)
(define (compare-times fn1 fn2 val)
  (let-values (((v1 t1 y1 z) (time-apply fn1 (list val)))
               ((v2 t2 y2 zz) (time-apply fn2 (list val))))
    (list y1 y2)))

;; builds a list of size n
(define (list-of-size n) (build-list n (lambda (i) 666)))

(define (stress-test sizes)
  (map
   (lambda (n)
     (cons n (compare-times number-list2 number-list (list-of-size n))))
   sizes))
                    
(define (run-stress-tests)
  (stress-test '(1000 2000 4000 8000)))

;; do this twice-- once with the invariant and once with pure template:

;; with the invariant:
;; > (run-stress-tests)
;; (list (list 1000 0 1) (list 2000 0 0) (list 4000 2 1) (list 8000 42 2))

;; solution #3: pure template, no invariant:
;> (run-stress-tests)
;(list (list 1000 0 125) (list 2000 15 484) (list 4000 0 1950) (list 8000 16 7924))






