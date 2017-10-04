;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname 01-4-bar-order-template) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(require rackunit)
(require "extras.rkt")


;;; Example Template for Mixed Data

(define-struct coffee (size type milk?))
(define-struct wine (vineyard year))
(define-struct tea (size type))

;; A BarOrder is one of
;; -- (make-coffee Size String Boolean)
;;  interp: 
;;   size is the size of cup desired
;;   type is the origin of the coffee (as a string)
;;   milk? tells whether milk is desired.
;; -- (make-wine Vineyard Year)
;;  interp:
;;   vineyard is the origin of the grapes
;;   year is the year of harvest
;; -- (make-tea Size String)
;;  interp: 
;;   size is the size of cup desired
;;   type is the type of tea (as a string)

;; bo-fn : BarOrder -> ??
;(define (bo-fn order)
;  (cond
;    [(coffee? order) (...
;                      (coffee-size order)
;                      (coffee-type order)
;                      (coffee-milk? order))]
;    [(wine? order) (...
;                    (wine-vineyard order)
;                    (wine-year order))]
;    [(tea? order) (...
;                   (tea-size order)
;                   (tea-type order))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Example template for compound data

(define-struct book (author title on-hand price))

;; A Book is a 
;;  (make-book String String NonNegInt NonNegInt)
;; Interpretation:
;;   author is the author’s name
;;   title is the title
;;   on-hand is the number of copies on hand
;;   price is the price in USD*100  (e.g. $7.95 => 795)

;; book-fn : Book -> ??
(define (book-fn b)
  (...
    (book-author b)
    (book-title b)
    (book-on-hand b)
    (book-price b)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Example template for itemization data


;; A Size is one of
;; -- "small"
;; -- "medium"
;; -- "large"

;; size-fn : Size -> ??
(define (size-fn s)
  (cond
    [(string=? s "small") ...]
    [(string=? s "medium") ...]
    [(string=? s "large") ...]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


