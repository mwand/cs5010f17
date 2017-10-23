;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname abstractions) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;;; Examples of generalization

;;; HtDP talks about "abstraction", but we prefer the term "generalization".

(require rackunit)
(require "extras.rkt")

;; Data 
;; -- empty
;; -- (cons X XList)

;; TEMPLATE:
;; xlst-fn : XList -> ??
;; (define (xlst-fn lst)
;;   (cond
;;     [(empty? lst) ...]
;;     [else (... 
;;             (first lst)
;;             (xlst-fn (rest lst)))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; example 1:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; NumberList -> NumberList
;; GIVEN: a list of numbers
;; RETURNS: the list obtained by adding 1 to each number
;; EXAMPLE: (10 20 30) => (11 21 31)
;; STRATEGY: Use template for NumberList on lst
(define (add-1-to-each lst) 
  (cond
    [(empty? lst) empty]
    [else (cons (add1 (first lst))
                (add-1-to-each (rest lst)))]))

(begin-for-test
  (check-equal?
    (add-1-to-each (list 22 33 44))
    (list 23 34 45)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Employee
(define-struct employee (name salary))
;; interpretation omitted...

;; A EmployeeList is either
;; -- empty
;; -- (cons Employee EmployeeList)

;; OBSERVER TEMPLATE:
;; employee-list-fn : EmployeeList -> ??
;; (define (loe-fn lst)
;;   (cond
;;     [(empty? lst) ...]
;;     [else (... 
;;             (employee-fn (first lst))
;;             (employee-list-fn (rest lst)))]))




;; employee-names : EmployeeList -> StringList
;; GIVEN: a list of employees
;; RETURNS: the list of their names
;; STRATEGY: Use template for EmployeeList on lst
(define (employee-names lst)
  (cond
    [(empty? lst) empty]
    [else (cons (employee-name (first lst))
                (employee-names (rest lst)))]))

(define lst1
  (list (make-employee "Joe" 100)
        (make-employee "Steve" 300)))

(begin-for-test
  (check-equal? (employee-names lst1) (list "Joe" "Steve")))

;; Generalize these to:

;; apply-to-each : (X->Y) XList  -> YList
;; GIVEN: a list xlst and a function f
;; RETURNS: the list obtained by applying f to each element of xlst
;; EXAMPLES:
;; (apply-to-each (list 10 20 30) add1) = (list 11 21 31)
;; (apply-to-each (list 1 2 3) sqr) = (list 1 4 9)
;; STRATEGY: Use template for XList to xlst
(define (apply-to-each fn xlst)
  (cond
    [(empty? xlst) empty]
    [else (cons (fn (first xlst))
                (apply-to-each fn (rest xlst)))]))

;; This is built into ISL under the name "map".  That is the version
;; you should use in your own code.

;; map : (X -> Y) XList -> YList

;; versions using the generalization:

;; comment out the old definitions and uncomment these to test.

;; ;; STRATEGY: Use HOF apply-to-each to lst
;; (define (add-1-to-each lst)
;;   (apply-to-each add1 lst))

;; ;; STRATEGY: Use HOF apply-to-each on lst
;; (define (employee-names lst)
;;   (apply-to-each employee-name lst))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; More uses of apply-to-each
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; STRATEGY: Use HOF apply-to-each on lst
(define (add-5-to-each lst)
  (local
    ((define (add5 n) (+ n 5)))  
    (apply-to-each add5 lst)))

;; Number NumberList -> NumberList
;; GIVEN: a number n and list of numbers lst
;; RETURNS: the list obtained by adding n to each element of the list
;; EXAMPLE:
;; (add-to-each 4 (list 20 30 40)) = (list 24 34 44)
;; STRATEGY: Apply HOF apply-to-each to lst.

(define (add-to-each x lst)
  (local 
    ((define (addx n) (+ n x)))
    (apply-to-each addx lst)))

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

;; sum : NumberList -> Number
;; GIVEN: a list of numbers
;; RETURNS: their sum
;; STRATEGY: Use template for NumberList on lst

(define (sum lst)
  (cond
    [(empty? lst) 0]
    [else (+
           (first lst)
           (sum (rest lst)))]))

;; product : NumberList -> Number
;; GIVEN: a list of numbers
;; RETURNS: their product
;; STRATEGY: Use template for NumberList on lst

(define (product lst)
  (cond
    [(empty? lst) 1]
    [else (*
           (first lst)
           (product (rest lst)))]))

(begin-for-test
  (check-equal? (sum (list 2 3 4)) 9)
  (check-equal? (product (list 2 3 4)) 24))

;;; Generalized function:

;; my-foldr: (Number Number -> Number) Number NumberList -> Number
;; STRATEGY: Use template for NumberList on lst
(define (my-foldr combiner base lst)
  (cond
    [(empty? lst) base]
    [else (combiner
           (first lst)
           (my-foldr combiner base (rest lst)))]))

;; This is built in to ISL under the name "foldr".  That is the version
;; you should use in your own code.
;; The most general contract for foldr is
;; foldr : (X Y -> Y) Y XList -> Y

;; now we can define sum and product in terms of my-foldr

;; ;; STRATEGY: Use HOF my-foldr on lst
;; (define (sum lst)
;;   (my-foldr + 0 lst))

;; ;; STRATEGY: Use HOF my-foldr on lst
;; (define (product lst)
;;   (my-foldr * 1 lst))

;; Be sure to test these.

;;; Can define apply-to-each in terms of my-foldr:

;; ;; STRATEGY: Use HOF my-foldr on xlst
;; (define (apply-to-each fn xlst)
;;   (local
;;     ((define (combiner first-guy result-on-the-rest)
;;        (cons (fn first-guy)
;;              result-on-the-rest)))  
;;   (my-foldr combiner empty xlst)))

;; Try using this definition of apply-to-each in place of the
;; definition above.
