;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname 04-1-lists) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;; lists.rkt

(require rackunit)
(require "extras.rkt")

;; A List of Numbers (LON) is one of:
;; -- empty
;; -- (cons Number LON)

;; Template:
;; ;; lon-fn : LON -> ??
;; ;; HALTING MEASURE: (length lst)
;; (define (lon-fn lst)
;;   (cond
;;     [(empty? lst) ...]
;;     [else (... (first lst)
;;                (lon-fn (rest lst)))]))


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

;; lon-length : LON -> Integer
;; GIVEN: a LON
;; RETURNS: its length

;; If the tests are simple, you can use them as examples.
;; But they'd better be very very simple.

(begin-for-test
  (check-equal? (lon-length empty) 0)
  (check-equal? (lon-length (cons 11 empty)) 1)
  (check-equal? (lon-length (cons 33 (cons 11 empty))) 2))

; STRATEGY: Use template for LON on lst
(define (lon-length lst)
  (cond
    [(empty? lst) 0]
    [else (+ 1 
            (lon-length (rest lst)))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; lon-sum : LON -> Number
;; GIVEN: a LON
;; RETURNS: its sum

;; EXAMPLES/TESTS:
(begin-for-test
  (check-equal? (lon-sum empty) 0)
  (check-equal? (lon-sum (cons 11 empty)) 11)
  (check-equal? (lon-sum (cons 33 (cons 11 empty))) 44)
  (check-equal? (lon-sum (cons 10 (cons 20 (cons 3 empty)))) 33))

;; STRATEGY: Use template for LON on lst
;; HALTING MEASURE: (length lst)

(define (lon-sum lst)
  (cond
    [(empty? lst) 0]
    [else (+ (first lst)
             (lon-sum (rest lst)))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; lon-avg : LON -> Number
;; GIVEN: a non-empty LON
;; RETURNS: its average

(begin-for-test
  "lon-avg tests"
  (check-equal? (lon-avg (cons 11 empty)) 11)
  (check-equal? (lon-avg (cons 33 (cons 11 empty))) 22)
  (check-equal? (lon-avg (cons 10 (cons 20 (cons 3 empty)))) 11))

; STRATEGY: combine simpler functions (!)

(define (lon-avg lst)
  (/ (lon-sum lst) (lon-length lst)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; double-all : LON -> LON
;; GIVEN: a LON
;; RETURNS: a list just like the original, but with each
;; number doubled 

(begin-for-test
  (check-equal? (double-all empty) empty)
  (check-equal? (double-all (cons 11 empty)) (cons 22 empty))
  (check-equal? 
   (double-all (cons 33 (cons 11 empty)))
   (cons 66 (cons 22 empty))))

; strategy: Use template for LON on lst
;; HALTING MEASURE: (length lst)

(define (double-all lst)
  (cond
    [(empty? lst) empty]
    [else (cons (* 2 (first lst))
                (double-all (rest lst)))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; remove-evens : LON -> LON
;; GIVEN: a LON
;; RETURNS: a list just like the original, but with all
;; the even numbers removed 

(define list-22-11-13-46-7 
    (cons 22 
          (cons 11 (cons 13 (cons 46 (cons 7 empty))))))
;; a list whose first even is not in the first position (not on the slides!)
(define list-17-22-11-13-46-7
  (cons 17 list-22-11-13-46-7))

(begin-for-test
  (check-equal? (remove-evens empty) empty)
  (check-equal? (remove-evens (cons 11 empty)) (cons 11 empty))
  (check-equal?
   (remove-evens list-22-11-13-46-7)
   (cons 11 (cons 13 (cons 7 empty))))
  (check-equal?
    (remove-evens list-17-22-11-13-46-7)
    (cons 17 (cons 11 (cons 13 (cons 7 empty))))))

;; STRATEGY: Use template for LON on lst
;; HALTING MEASURE: (length lst)

(define (remove-evens lst)
  (cond
    [(empty? lst) empty]
    [else (if (even? (first lst))
              (remove-evens (rest lst))
              (cons (first lst)
                    (remove-evens (rest lst))))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; remove-first-even : LON -> LON
;; GIVEN: a LON
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

;; STRATEGY: Use template for LON on lst
;; HALTING MEASURE: (length lst)

(define (remove-first-even lst)
  (cond
    [(empty? lst) empty]
    [else (if (even? (first lst))
              (rest lst)
              (cons (first lst)
                    (remove-first-even
                             (rest lst))))]))
