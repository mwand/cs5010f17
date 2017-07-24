;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname 01-4-bar-order-template) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(require rackunit)
(require "extras.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; A Book in a bookstore

;; REPRESENTATION:
;; a Book is represented as a struct (make-book author title on-hand price)
;; with the following fields:
;; author : String     is the author's name
;; title  : String     is the title of the book
;; on-hand : NonNegInt is the number of copies on hand
;; price   : NonNegInt is the price of the book in USD*100
;;                            (e.g. $7.95 => 795)

;; IMPLEMENTATION
(define-struct book (author title on-hand price))

;; CONSTRUCTOR TEMPLATE
;; (make-book String String NonNegInt NonNegInt)

;; OBSERVER TEMPLATE
;; book-fn : Book -> ??
(define (book-fn b)
  (...
    (book-author b)
    (book-title b)
    (book-on-hand b)
    (book-price b)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; INFORMATION ANALYSIS:

;; An Altitude is represented as a Real, measured in meters

;; A Velocity is represented as a Real, measured in meters/sec upward

;; We have a single rocket, which is at some altitude and is
;; travelling vertically at some velocity.

;; REPRESENTATION:
;; A Rocket is represented as a struct (make-rocket altitude velocity)
;; with the following fields:
;; altitude : Altitude  is the rocket's altitude
;; velocity : Velocity  is the rocket's velocity

;; IMPLEMENTATION:
(define-struct rocket (altitude velocity))

;; CONSTRUCTOR TEMPLATE:
;; (make-rocket Real Real)

;; OBSERVER TEMPLATE:
;; rocket-fn : Rocket -> ??
(define (rocket-fn r)
  (... (rocket-altitude r) (rocket-velocity r)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; A Ring, with inner and outer radii

;; REPRESENTATION
;; A Ring is represented as a struct
;;  (make-ring inner outer)
;; with the following fields:
;; inner : PosReal  is the ring's inner radius
;; outer : PosReal  is the ring's outer radius
;; WHERE (< inner outer) is true

;; IMPLEMENTATION
(define-struct ring (inner outer))

;; CONSTRUCTOR TEMPLATE:
;; (make-ring PosReal PosReal)

;; OBSERVER TEMPLATE
;; ring-fn : Ring -> ??
(define (ring-fn r)
  (... (ring-inner r) (ring-outer r)))


