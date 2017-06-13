;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname 02-1-book-receipts) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;; book-receipts.rkt

;;; DATA DEFINITION

(define-struct book (author title on-hand price))

;; A Book is a 
;;  (make-book String String NonNegInt NonNegInt)
;; Interpretation:
;;   author is the author’s name
;;   title is the title
;;   on-hand is the number of copies on hand
;;   price is the price in USD*100  (e.g. $7.95 => 795)

;; book-fn : Book -> ??
#|                   
(define (book-fn b)
  (...
    (book-author b)
    (book-title b)
    (book-on-hand b)
    (book-price b)))
|#

;;; here we've used block comments, delimited by #| ... |# to avoid
;;; having to deal with all those semicolons.

;; book-receipts : Book NonNegInt -> NonNegInt
;; GIVEN: a Book and the number of copies sold
;; RETURNS: the total receipts from the sales of the given book, in
;; USD*100. Ignores the number of copies on hand.
;; EXAMPLE:
;; (book-receipts (make-book "Felleisen" "HtdP2" 13 2795) 100) = 279500 

;; first, make a copy of the template.  Here we will leave it
;; commented:
#|
(define (book-fn b sales)
  (...
    sales
    (book-author b)
    (book-title b)
    (book-on-hand b)
    (book-price b)))
|#

;; next, fill in the function name and add more arguments if needed:
#|
(define (book-receipts b sales)
  (...
    (book-author b)
    (book-title b)
    (book-on-hand b)
    (book-price b)))
|#

;; you don't have to show any of the steps above.

;; the final version, with strategy:

;; STRATEGY: Use template for Book on b.
(define (book-receipts b sales)
  (* (book-price b) sales))



