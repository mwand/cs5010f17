(require rackunit)
(require "extras.rkt")

;; Lists of books

(define-struct book (author title on-hand price))

;; A Book is a 
;;  (make-book String String Number Number)
;; Interpretation:
;; author is the author’s name
;; title is the title
;; on-hand is the number of copies on hand
;; price is the price in USD

;; book-fn : Book -> ??
;; (define (book-fn b)
;;   (... (book-author b) (book-title b) (book-on-hand b) (book-price b)))

;; A ListOfBooks (LOB) is either
;; -- empty
;; -- (cons Book LOB)

;; lob-fn : LOB -> ??
;; (define (lob-fn lob)
;;   (cond
;;     [(empty? lob) ...]
;;     [else (...
;;             (book-fn (first lob))
;;             (lob-fn (rest lob)))]))

;; An Inventory is a ListOfBooks.
;; Interp: the list of books that the bookstore carries, in any order.

(define lob1
  (list
    (make-book "Felleisen" "HtDP/1" 20 7)
    (make-book "Wand" "EOPL" 5 50)
    (make-book "Shakespeare" "Hamlet" 0 2)
    (make-book "Shakespeare" "Macbeth" 0 10)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; FUNCTION DEFINITIONS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; book-inventory-value : Book -> Number
;; GIVEN: the inventory record for a book
;; RETURNS: the value of the copies on hand of the given book
;; (book-inventory-value (make-book "Felleisen" "HtDP/1" 20 7)) = 140
;; STRATEGY: struct decomp on b : Book
(define (book-inventory-value b)
  (* (book-on-hand b) (book-price b)))

(begin-for-test
  (check-equal?
    (book-inventory-value (make-book "Felleisen" "HtDP/1" 20 7))
    140
    "value of 20 Felleisens at $7 should have been $140"))

;; inventory-total-value : LOB -> Number
;; GIVEN: a LOB
;; RETURNS: the value of all the copies on hand of all the books in the
;; given LOB
;; (inventory-total-value lob1) = 390

(begin-for-test
  (check-equal? 
    (inventory-total-value empty)
    0
    "value of the empty inventory should have been 0")
  (check-equal?
    (inventory-total-value lob1)
    390
    "simple test"))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; SOLUTIONS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Problem 1

;; inventory-total-value : LOB -> Number
;; GIVEN: a LOB
;; RETURNS: the value of all the copies on hand of all the books in the
;; given LOB
;; (inventory-total-value lob1) = 390
;; EXAMPLES/TESTS:

(begin-for-test
  (check-equal? 
    (inventory-total-value empty)
    0
    "value of the empty inventory should have been 0")
  (check-equal?
    (inventory-total-value lob1)
    390
    "simple test"))

;; STRATEGY: Use template for LOB on lst
(define (inventory-total-value lst)
  (cond
    [(empty? lst) 0]
    [else (+
            (book-inventory-value (first lst))
            (inventory-total-value (rest lst)))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Problem 2


;; books-out-of-stock : LOB -> LOB
;; returns a list of the books that are out of stock in the given LOB
;; Example:
;; (books-out-of-stock lob1) =
;;  (list
;;    (make-book "Shakespeare" "Hamlet" 0 2)
;;    (make-book "Shakespeare" "Macbeth" 0 10))
;; Strategy: Use template for LOB on lst

(define (books-out-of-stock lst)
  (cond
    [(empty? lst) empty]
    [else (if (book-out-of-stock? (first lst))
            (cons (first lst) (books-out-of-stock (rest lst)))
            (books-out-of-stock (rest lst)))]))

(check-equal?
  (books-out-of-stock lob1)
  (list
    (make-book "Shakespeare" "Hamlet" 0 2)
    (make-book "Shakespeare" "Macbeth" 0 10)))

;; book-out-of-stock? : Book -> Boolean
;; returns true iff the given book is out of stock
;; EXAMPLE: 
;; (book-out-of-stock? (make-book "Felleisen" "HtDP/1" 20 7)) = false
;; (book-out-of-stock? (make-book "Felleisen" "HtDP/1"  0 7)) = true
;; STRATEGY: Use template for Book on b
(define (book-out-of-stock? b)
  (<= (book-on-hand b) 0))

(begin-for-test
  (check-false
    (book-out-of-stock? (make-book "Felleisen" "HtDP/1" 20 7)))
  (check-true
    (book-out-of-stock? (make-book "Felleisen" "HtDP/1"  0 7))))

;; (check-equal? ... true) would have been fine, too.  Look at the
;; Help Desk page for check-equal? to see some other useful checks.


;; Note that the design of the help function book-out-of-stock?, with
;; its deliverables, is required for a perfect solution.  Writing 
;; (if (<= (book-on-hand (first lob)) 0) ... ...)
;; matches the template, but it is undesirable because it violates the
;; principle of one-function-one-task.  It violates this principle
;; because this version of the function requires books-out-of-stock to
;; know both about the representation of an inventory AND the
;; how being out of stock is represented for a book.

