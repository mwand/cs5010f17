;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname 02-5-balls-collide) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;; Example: Structural Decomposition on two compound values

;; decompose the first compound, then pass the relevant fields to a
;; help function that decomposes the second compound.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; DATA DEFINITIONS

;; REPRESENTATION:
;; A Ball is represented as (ball x y radius selected?) with the
;; following fields:
;; x, y : Integer        the coordinates of the center of the ball, in pixels,
;;                       relative to the origin of the scene.
;; radius : NonNegReal   the radius of the ball, in pixels
;; selected? : Boolean   true iff the ball has been selected for dragging.

;; IMPLEMENTATION:
(define-struct ball (x y radius selected?))

;; CONSTRUCTOR TEMPLATE
;; (make-ball Integer Integer NonNegReal Boolean)

;; OBSERVER TEMPLATE
;; ball-fn : Ball -> ??
(define (ball-fn b)
  (... (ball-x b)
       (ball-y b)
       (ball-radius b)
       (ball-selected? b)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; balls-intersect? : Ball Ball -> Boolean
;; GIVEN: two balls
;; ANSWERS: do the balls intersect?
;; STRATEGY: Extract relevant information and call a simpler function
(define (would-balls-collide? b1 b2)
  (circles-intersect?
    (ball-x b1) (ball-y b1) (ball-radius b1)
    (ball-x b2) (ball-y b2) (ball-radius b2)))

;; circles-intersect? : Int Int NNReal Int Int NNReal -> Boolean
;; GIVEN: two positions and radii
;; ANSWERS: Would two circles with the given positions and radii intersect?
;; STRATEGY: Transcribe mathematical formula
(define (circles-intersect? x1 y1 r1 x2 y2 r2)
  (<=
    (+
      (sqr (- x1 x2))
      (sqr (- y1 y2)))
    (sqr (+ r1 r2))))

;; circles-intersect? knows about geometry.  It doesn't know about
;; balls: eg it doesn't know the field names of Ball or about
;; ball-selected?.

;; If we changed the representation of balls, to add color, text, or
;; to change the names of the fields, would-balls-collide? would need
;; to change, but would-circles-collide? wouldn't need to change.

