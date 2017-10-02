;; to follow Lesson 7.2

;; design a function

;; diff : NonEmptyNumberList -> Number
;; GIVEN: a nonempty list of numbers
;; RETURNS: the result of subtracting the numbers, from left to right.
;; EXAMPLE:
;; (diff (list 10 5 3)) = 2

;; Hints:  

;; Use the data definition

;; NonEmptyNumberList = (cons Number NumberList)

;; You will need one function for the whole list, and one for the
;; NumberList.

;; Your function for the NumberList must follow the template for
;; NumberList, but you will need a context argument.  Think carefully
;; about what the context argument means.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Solution:

;; DATA DEFINITIONS:

;; NonEmptyNumberList = (cons Number NumberList)
;; template:
(define (nelst-fn nelst)
  (... (first nelst) (rest nelst)))

;; NumberList template:
(define (lon-fn lon)
  (cond
    [(empty? lon) ...]
    [else (... 
            (first lon)
            (lon-fn (rest lon)))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; diff : NonEmptyNumberList -> Number
;; GIVEN: a nonempty list of numbers
;; RETURNS: the result of subtracting the numbers, from left to right.
;; EXAMPLE:
;; (diff (list 10 5 3)) = 2
;; STRATEGY: Follow the template for NonEmptyNumberList

(define (diff nelst)
  (diff-inner (first nelst) (rest nelst)))


;; diff-inner : Number NumberList
;; GIVEN: a number num and a sublist lon of some list lon0
;; WHERE: num is the result of subtracting all the numbers in lon0
;; that are above lon from some number num0
;; RETURNS: the result of subtracting all the numbers in lon0 from num0
;; STRATEGY: Follow the template for NumberList

(define (diff-inner num lon)
  (cond
    [(empty? lon) num]
    [else (diff-inner
            (- num (first lon))
            (rest lon))]))


;; Now, we could do this one without an invariant, just by changing
;; the purpose statement:

;; diff-inner : Number NumberList -> Number
;; GIVEN: a number num and a list of numbers lon
;; RETURNS: the result of subtracting each of the numbers in lon from
;; num
;; EXAMPLE: (diff-inner 10 (list 5 3)) = 2
;; STRATEGY: Follow the template for NumberList

(define (diff-inner num lon)
  (cond
    [(empty? lon) num]
    [else (diff-inner
            (- num (first lon))
            (rest lon))]))

;; Note that the code is exactly the same thing-- only the purpose
;; statement has changed.
