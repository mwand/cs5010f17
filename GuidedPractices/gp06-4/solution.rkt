;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname solution) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
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

;; STRATEGY: Use HOF foldr on lob

(define (inventory-total-copies-on-hand lob)
  (foldr
    (lambda (b total-for-rest)
      (+ (book-on-hand b) total-for-rest))
    0
    lob))

(begin-for-test
  (check-equal?
    (inventory-total-copies-on-hand lob1)
    25))

  