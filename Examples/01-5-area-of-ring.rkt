;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname 01-7-area-of-ring) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;; area-of-ring.rkt

(require rackunit)
(require "extras.rkt")

;; area-of-ring : Real Real -> Real
;; GIVEN: the inner and outer radii of a ring
;; RETURNS: the area of the ring
;; EXAMPLE:
;; (area-of-ring 2 3) = 9*pi - 4*pi = 5*pi    

;; STRATEGY: combine simpler functions
(define (area-of-ring inner outer)
  (- (area-of-circle outer)
     (area-of-circle inner)))

(begin-for-test
  (check-=
    (area-of-ring 3 3)
    0
    0.01
    "(area-of-ring 3 3) should be 0")
  (check-=
    (area-of-ring 2 3)
    (* 5 pi)
    0.01
    "(area-of-ring 2 3) should be 5*pi"))

;; area-of-circle : Real -> Real
;; GIVEN: the radius of a circle
;; RETURNS: its area
;; EXAMPLE:
;; (area-of-circle 2) = 4*pi
;; (area-of-circle 3) = 9*pi
;; (area-of-circle 4) = 16*pi
;; STRATEGY: function composition
(define (area-of-circle radius)
  (* pi radius radius))

;;; tests for area-of-circle
;; pi happens to be predefined in BSL (you could look it up!)

(begin-for-test

  (check-= (area-of-circle 2) (* 4 pi) 0.01
    "radius = 2 should give area = 4*pi")

  (check-= (area-of-circle 3) (* 9 pi) 0.01
    "radius = 3 should give area = 9*pi")

  (check-= (area-of-circle 4) (* 16 pi) 0.01
    "radius = 4 should give area = 16*pi"))

;; COMMENTARY: could we have written the following?

;; STRATEGY: combine simpler functions
(define (area-of-ring2 inner outer)
  (- (* pi outer outer) (* pi inner inner)))

;; Answer: Yes, probably, but this is about as complicated as "combine
;; simpler functions" should get.

;; Could we have written this one?

(define (area-of-ring3 inner outer)
  (* pi (- (* outer outer) (* inner inner))))

;; Answer: you'd have to supply some explanation to go with this,
;; because it's certainly not obvious!



