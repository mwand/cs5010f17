;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname 01-4-velocity) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; velocity.rkt

(require "extras.rkt")
(require rackunit)


;; velocity-at-time : Real Real Real -> Real
;; PURPOSE:
;; GIVEN: initial velocity v0, acceleration a, and time t,
;; in any compatible set of units. 
;; RETURNS: the velocity at time t.
;; EXAMPLES:
;; (velocity-at-time 10 20 0) = 10
;; (velocity-at-time 10 20 2) = 50
;; STRATEGY: combine simpler functions
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

;; be sure to show what happens when a test fails, 
;; or when an error is raised





