;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname 02-1-book-receipts) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;; book-receipts.rkt

;;; DATA DEFINITIONS

;; A  Title  is represented as a String
;; An Author is represented as a String

;; A MoneyAmount is represented as a NonNegInt
;; INTERP: a price in USD*100
;; EXAMPLE: 795 represents the price $7.95


;; REPRESENTATION:
;; A Book is represented as a (make-book title on-hand price) with
;; the following fields:
;; INTERP:
;; title   : Title      is the title of the book
;; author  : Author     is the author of the book
;; on-hand : NonNegInt  is the number of copies on hand
;; price   : MoneyAmount is the price of the book

;; IMPLEMENTATION:
(define-struct book (author title on-hand price))

;; CONSTRUCTOR TEMPLATE:
;; (make-book Title Author NonNegInt MoneyAmount)

;; OBSERVER TEMPLATE:
;; book-fn : Book -> ??
(define (book-fn b)
  (... (book-title b)
       (book-author b)
       (book-on-hand b)
       (book-price b)))



;; book-receipts : Book NonNegInt -> MoneyAmount
;; GIVEN: a Book and the number of copies sold
;; RETURNS: the total receipts from the sales of the given book, in
;; USD*100. Ignores the number of copies on hand.
;; EXAMPLE:
;; (book-receipts (make-book "Felleisen" "HtdP2" 13 2795) 100) = 279500 
;; STRATEGY: Transcribe formula
(define (book-receipts b sales)
  (* (book-price b) sales))



