;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname 02-2-traffic-light) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require rackunit)
(require "extras.rkt")

;; DATA DEFINITION:
;; a TrafficLightColor (TLColor) is represented by one of :
;; -- "red"
;; -- "yellow" 
;; -- "green"
;; INTERPRETATION: self-evident

;; TEMPLATE
;; tlc-fn : TLColor -> ??
#|
(define (tlc-fn color)
 (cond
   [(string=? color "red")    ...]
   [(string=? color "yellow") ...]
   [(string=? color "green")  ...]))  
|#  

;; next-color : TLColor -> TLColor
;; GIVEN: a TLColor
;; RETURNS: the TLColor that follows the given TLColor
;; EXAMPLES:
;; (next-color "red") = "green"
;; (next-color "yellow") = "red"
;; (next-color "green") = "yellow"
;; STRATEGY: Cases on color

(define (next-color color)
 (cond
   [(string=? color "red")    "green"]
   [(string=? color "yellow") "red"]
   [(string=? color "green")  "yellow"]))

(begin-for-test
  (check-equal? (next-color "green") "yellow")
  (check-equal? (next-color "yellow") "red")
  (check-equal? (next-color "red") "green"))

;; two-colors-later : TLColor -> TLColor
;; given a TLColor, produces the color that should occur two steps later
;; (two-colors-later "green") = "red", etc.
;; design strategy: combine simpler functions
(define (two-colors-later tlc1)
  (next-color (next-color tlc1)))

;; previous-color : TLColor -> TLColor
;; GIVEN: a TLColor
;; RETURNS: the color that preceded it
;; (previous-color "red") = "yellow"
;; (previous-color "yellow") = "green"
;; (previous-color "green") = "red"
;; STRATEGY: Use template for TLColor on color

(define (previous-color color) ...)

;; (begin-for-test
;;   (check-equal? (previous-color "red") "yellow"
;;     "red should be preceded by yellow")

;;   (check-equal? (previous-color "yellow") "green"
;;     "yellow should be preceded by green")

;;   (check-equal? (previous-color "green") "red"
;;     "green should be preceded by red"))

