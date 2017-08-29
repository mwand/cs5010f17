;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname 02-2-1-velocity) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

;; velocity.rkt

(require "extras.rkt")
(require rackunit)


;; Data Definitions

;; A Velocity      is represented as a Real number
;; An Acceleration is represented as a Real number
;; A Time          is represented as a Real number
;; These representations use any compatible set of units (e.g. MKS,
;; CGS, English) 

;; NOTE: although all these quantities are represented as Real number,
;; they are very different!  In particular, they have different
;; units.  For example, you shouldn't add an acceleration and a time.


;; velocity-at-time : Velocity Acceleration Time -> Velocity
;; PURPOSE:
;; GIVEN: initial velocity v0, acceleration a, and time t,
;; RETURNS: the velocity at time t.
;; EXAMPLES:
; (velocity-at-time 10 20 0) = 10
; (velocity-at-time 10 20 2) = 50
;; STRATEGY: transcribe formula
(define (velocity-at-time v0 a t)
 (+ v0 (* a t)))

(begin-for-test
  (check-equal? 
   (velocity-at-time 10 20 0) 
   10               
   "at time 0, velocity should be v0")
  
  (check-equal? 
   (velocity-at-time 10 20 2) 
   50
   "at time 2, velocity should 50"))
