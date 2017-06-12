(require rackunit)
(require "extras.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; DATA DEFINITIONS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Book

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

;; ListofBooks

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

;; Inventory

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

;; TEST:
(begin-for-test
  (check-equal?
    (book-inventory-value (make-book "Felleisen" "HtDP/1" 20 7))
    140
    "value of 20 Felleisens at $7 should have been $140"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Problem 1
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


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
;;; Problem 2

;; books-out-of-stock : LOB -> LOB
;; GIVEN: a list of books
;; RETURNS: a list of the books that are out of stock in the given LOB
;; Example:
;; (books-out-of-stock lob1) =
;;  (list
;;    (make-book "Shakespeare" "Hamlet" 0 2)
;;    (make-book "Shakespeare" "Macbeth" 0 10))

