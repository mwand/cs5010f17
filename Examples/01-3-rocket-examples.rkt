;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname 01-3-rocket-examples) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; making examples human-readable

;; a rocket simulation.  

;; INFORMATION ANALYSIS:

;; An Altitude is represented as a Real, measured in meters

;; A Velocity is represented as Real, measured in meters/sec upward

;; We have a single rocket, which is at some altitude and is
;; travelling vertically at some velocity.

;; REPRESENTATION:
;; A Rocket is represented as (rocket altitude velocity)
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

;; rocket-after-dt : Rocket Real -> Rocket
;; GIVEN: a rocket and and a time interval (in seconds)
;; RETURNS: the state of the rocket after the interval has passed

;; OK EXAMPLE:
;; (rocket-after-dt (make-rocket 100 30) 0) = (make-rocket 100 30)
;; (rocket-after-dt (make-rocket 100 30) 2) = (make-rocket 160 30)

;; BETTER WAY TO WRITE THE EXAMPLES:

(define rocket-at-100 (make-rocket 100 30))
(define rocket-at-160 (make-rocket 160 30))

;; (rocket-after-dt rocket-at-100 0) = rocket-at-100
;; (rocket-after-dt rocket-at-100 2) = rocket-at-160


