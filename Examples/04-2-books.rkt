(require rackunit)
(require "extras.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; DATA DEFINITIONS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Book

(define-struct book (author title on-hand price))

;; A Book is a 
;;  (make-book String String NonNegInt NonNegInt)
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
;; Interp: the list of books that the bookstore carries, IN ANY ORDER.

(DEFINE lob1
  (list
    (make-book "Felleisen" "HtDP/1" 20 7)
    (make-book "Wand" "EOPL" 5 50)
    (make-book "Shakespeare" "Hamlet" 0 2)
    (make-book "Shakespeare" "Macbeth" 0 10)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; FUNCTION DEFINITIONS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; book-inventory-value : Book -> NonNegInt
;; GIVEN: the inventory record for a book
;; RETURNS: the value of the copies on hand of the given book
;; EXAMPLE: (book-inventory-value (make-book "Felleisen" "HtDP/1" 20 7)) = 140
;; STRATEGY: Use template for Book on b

(define (book-inventory-value b)
  (* (book-on-hand b) (book-price b)))

;; TEST
(begin-for-test
  (check-equal?
    (book-inventory-value (make-book "Felleisen" "HtDP/1" 20 7))
    140
    "value of 20 Felleisens at $7 should have been $140"))

;;; SEE GUIDED PRACTICE 4.4 FOR MORE
