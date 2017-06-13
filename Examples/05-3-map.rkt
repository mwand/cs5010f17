;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname abstractions) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;;; Examples of generalization

;;; HtDP talks about "abstraction", but we prefer the term "generalization".

(require rackunit)
(require "extras.rkt")

;; A ListOfX is either
;; -- empty
;; -- (cons X ListOfX)

;; TEMPLATE:
;; lox-fn : ListOfX -> ??
;; (define (lox-fn lst)
;;   (cond
;;     [(empty? lst) ...]
;;     [else (... 
;;             (first lst)
;;             (lox-fn (rest lst)))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; example 1:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ListOfNumber -> ListOfNumber
;; GIVEN: a list of numbers
;; RETURNS: the list obtained by adding 1 to each number
;; EXAMPLE: (10 20 30) => (11 21 31)
;; STRATEGY: Use template for ListOfNumber on lon
(define (add-1-to-each lon) 
  (cond
    [(empty? lon) empty]
    [else (cons (add1 (first lon))
                (add-1-to-each (rest lon)))]))

(begin-for-test
  (check-equal?
    (add-1-to-each (list 22 33 44))
    (list 23 34 45)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Employee
(define-struct employee (name salary))
;; interpretation omitted...

;; A ListOfEmployee is either
;; -- empty
;; -- (cons Employee ListOfEmployee)

;; TEMPLATE:
;; lox-fn : ListOfEmployee -> ??
;; (define (loe-fn lst)
;;   (cond
;;     [(empty? lst) ...]
;;     [else (... 
;;             (employee-fn (first lst))
;;             (loe-fn (rest lst)))]))




;; employee-names : ListOfEmployee -> ListOfString
;; GIVEN: a list of employees
;; RETURNS: the list of their names
;; STRATEGY: Use template for ListOfEmployee on loe
(define (employee-names loe)
  (cond
    [(empty? loe) empty]
    [else (cons (employee-name (first loe))
                (employee-names (rest loe)))]))

(define loe1
  (list (make-employee "Joe" 100)
        (make-employee "Steve" 300)))

(begin-for-test
  (check-equal? (employee-names loe1) (list "Joe" "Steve")))

;; Generalize these to:

;; apply-to-each : (X->Y) ListOfX  -> ListOfY
;; GIVEN: a list lox and a function f
;; RETURNS: the list obtained by applying f to each element of lox
;; EXAMPLES:
;; (apply-to-each (list 10 20 30) add1) = (list 11 21 31)
;; (apply-to-each (list 1 2 3) sqr) = (list 1 4 9)
;; STRATEGY: Use template for ListOfX to lox
(define (apply-to-each fn lox)
  (cond
    [(empty? lox) empty]
    [else (cons (fn (first lox))
                (apply-to-each fn (rest lox)))]))

;; This is built into ISL under the name "map".  That is the version
;; you should use in your own code.

;; map : (X -> Y) ListOfX -> ListOfY

;; versions using the generalization:

;; comment out the old definitions and uncomment these to test.

;; ;; STRATEGY: Use HOF apply-to-each to lon
;; (define (add-1-to-each lon)
;;   (apply-to-each add1 lon))

;; ;; STRATEGY: Use HOF apply-to-each on loe
;; (define (employee-names loe)
;;   (apply-to-each employee-name loe))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; More uses of apply-to-each
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; STRATEGY: Use HOF apply-to-each on lon
(define (add-5-to-each lon)
  (local
    ((define (add5 n) (+ n 5)))  
    (apply-to-each add5 lon)))

;; Number ListOfNumber -> ListOfNumber
;; GIVEN: a number n and list of numbers lon
;; RETURNS: the list obtained by adding n to each element of the list
;; EXAMPLE:
;; (add-to-each 4 (list 20 30 40)) = (list 24 34 44)
;; STRATEGY: Apply HOF apply-to-each to lon.

(define (add-to-each x lon)
  (local 
    ((define (addx n) (+ n x)))
    (apply-to-each addx lon)))

(begin-for-test
  (check-equal?
    (add-to-each 4 (list 20 30 40))
    (list 24 34 44))
  (check-equal?
    (add-5-to-each (list 20 30 40))
    (list 25 35 45)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Example 2:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; sum : ListOfNumber -> Number
;; GIVEN: a list of numbers
;; RETURNS: their sum
;; STRATEGY: Use template for ListOfNumber on lon

(define (sum lon)
  (cond
    [(empty? lon) 0]
    [else (+
           (first lon)
           (sum (rest lon)))]))

;; product : ListOfNumber -> Number
;; GIVEN: a list of numbers
;; RETURNS: their product
;; STRATEGY: Use template for ListOfNumber on lon

(define (product lon)
  (cond
    [(empty? lon) 1]
    [else (*
           (first lon)
           (product (rest lon)))]))

(begin-for-test
  (check-equal? (sum (list 2 3 4)) 9)
  (check-equal? (product (list 2 3 4)) 24))

;;; Generalized function:

;; my-foldr: (Number Number -> Number) Number ListOfNumber -> Number
;; STRATEGY: Use template for ListOfNumber on lon
(define (my-foldr combiner base lon)
  (cond
    [(empty? lon) base]
    [else (combiner
           (first lon)
           (my-foldr combiner base (rest lon)))]))

;; This is built in to ISL under the name "foldr".  That is the version
;; you should use in your own code.
;; The most general contract for foldr is
;; foldr : (X Y -> Y) Y ListOfX -> Y

;; now we can define sum and product in terms of my-foldr

;; ;; STRATEGY: Use HOF my-foldr on lon
;; (define (sum lon)
;;   (my-foldr + 0 lon))

;; ;; STRATEGY: Use HOF my-foldr on lon
;; (define (product lon)
;;   (my-foldr * 1 lon))

;; Be sure to test these.

;;; Can define apply-to-each in terms of my-foldr:

;; ;; STRATEGY: Use HOF my-foldr on lox
;; (define (apply-to-each lox fn)
;;   (local
;;     ((define (combiner first-guy result-on-the-rest)
;;        (cons (fn first-guy)
;;              result-on-the-rest)))  
;;   (my-foldr combiner empty lox)))

;; Try using this definition of apply-to-each in place of the
;; definition above.
