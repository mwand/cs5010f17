;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname 01-7-area-of-ring) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;; area-of-ring.rkt

(require rackunit)
(require "extras.rkt")

;; Data Definitions:

;; A Radius is represented as a PosReal
;; An Area is represented as a PosReal
;; These must use some compatible set of units (MKS, CGS, English, etc.)

;; area-of-ring : Radius Radius -> Area
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

;; area-of-circle : Radius -> Area
;; GIVEN: the radius of a circle
;; RETURNS: its area
;; EXAMPLE:
;; (area-of-circle 2) = 4*pi
;; (area-of-circle 3) = 9*pi
;; (area-of-circle 4) = 16*pi
;; STRATEGY: transcribe formula
(define (area-of-circle radius)
  (* pi radius radius))

;; tests for area-of-circle
;; pi happens to be predefined in BSL (you could look it up!)

(begin-for-test

  (check-= (area-of-circle 2) (* 4 pi) 0.01
    "radius = 2 should give area = 4*pi")

  (check-= (area-of-circle 3) (* 9 pi) 0.01
    "radius = 3 should give area = 9*pi")

  (check-= (area-of-circle 4) (* 16 pi) 0.01
    "radius = 4 should give area = 16*pi"))

;; COMMENTARY: remember that there are lots of functions with contract
;; Radius Radius -> Area.  Let's look at a few:

;; STRATEGY: combine simpler functions
(define (area-of-ring1 inner outer)
  (+ (area-of-circle inner) (area-of-circle outer)))

;; area-of-ring1 satisfies the contract for area-of-ring, but not its
;; purpose statement.  The purpose of the purpose statement is to
;; enable us to distinguish correct definitions from incorrect ones.

;; Some function definitions are correct, but are bad because they are
;; unclear or too complicated.  Let's look at some examples:

;; STRATEGY: combine simpler functions
(define (area-of-ring2 inner outer)
  (- (* pi outer outer) (* pi inner inner)))

;; area-of-ring2 is probably OK, but some TA is likely to say that you
;; have duplicated code ((* pi outer outer) and (* pi inner inner))
;; that you should move into a common help function.  Our original
;; area-of-ring is probably better.

(define (area-of-ring3 inner outer)
  (* pi (- (* outer outer) (* inner inner))))

;; area-of-ring3 needs some explanation; it doesn't seem obvious, at
;; least to me.  Remember: your job is to make the program as easy as
;; possible for the reader to understand.
