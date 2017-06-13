;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname 02-3-traffic-light) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(require rackunit)
(require "extras.rkt")

;; DATA DEFINITION:
;; a TrafficLightState (TLState) is one of
;; -- "red"
;; -- "yellow" 
;; -- "green"
;; INTERPRETATION: self-evident

;; TEMPLATE
;; tls-fn : TLState -> ??
#|
(define (tls-fn state)
 (cond
   [(string=? state "red")    ...]
   [(string=? state "yellow") ...]
   [(string=? state "green")  ...]))  
|#  

;; next-state : TLState -> TLState
;; GIVEN: a TLState
;; RETURNS: the TLState that follows the given TLState
;; EXAMPLES:
;; (next-state "red") = "green"
;; (next-state "yellow") = "red"
;; (next-state "green") = "yellow"
;; STRATEGY: Use template for TLState on state

(define (next-state state)
 (cond
   [(string=? state "red")    "green"]
   [(string=? state "yellow") "red"]
   [(string=? state "green")  "yellow"]))

(begin-for-test
  (check-equal? (next-state "green") "yellow")
  (check-equal? (next-state "yellow") "red")
  (check-equal? (next-state "red") "green"))

;; two-states-later : TLState -> TLState
;; given a TLState, produces the state that should occur two steps later
;; (two-states-later "green") = "red", etc.
;; design strategy: combine simpler functions
(define (two-states-later tls1)
  (next-state (next-state tls1)))

;; previous-state : TLState -> TLState
;; GIVEN: a TLState
;; RETURNS: the state that preceded it
;; (previous-state "red") = "yellow"
;; (previous-state "yellow") = "green"
;; (previous-state "green") = "red"
;; STRATEGY: Use template for TLState on state

(define (previous-state state) ...)

;; (begin-for-test
;;   (check-equal? (previous-state "red") "yellow"
;;     "red not preceded by yellow")

;;   (check-equal? (previous-state "yellow") "green"
;;     "yellow not preceded by green")

;;   (check-equal? (previous-state "green") "red"
;;     "green not preceded by red"))

