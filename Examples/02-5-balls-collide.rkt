;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname 02-5-balls-collide) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; Example: Structural Decomposition on two compound values

;; decompose the first compound, then pass the relevant fields to a
;; help function that decomposes the second compound.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; DATA DEFINITIONS

(define-struct ball (x y radius selected?))

;; A Ball is a (make-ball Number Number Number Boolean)
;; x and y are the coordinates of the center of the ball, in pixels,
;; relative to the origin of the scene.
;; radius is the radius of the ball, in pixels
;; selected? is true iff the ball has been selected for dragging.

;; TEMPLATE:
;; (define (ball-fn b)
;;   (...
;;     (ball-x b)
;;     (ball-y b)
;;     (ball-radius b)
;;     (ball-selected? b)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; balls-intersect? : Ball Ball -> Boolean
;; GIVEN: two balls
;; ANSWERS: do the balls intersect?
;; STRATEGY: use templates for b1 and b2 : Ball
(define (would-balls-collide? b1 b2)
  (circles-intersect?
   (ball-x b1) (ball-y b1) (ball-radius b1)
   (ball-x b2) (ball-y b2) (ball-radius b2)))

;; would-circles-collide? : Real^3 Real^3 -> Boolean
;; GIVEN: two positions and radii
;; ANSWERS: Would two circles with the given positions and radii intersect?
;; STRATEGY: Function Composition
(define (would-circles-collide? x1 y1 r1 x2 y2 r2)
  (<= (+ (sqr (- x1 x2))
         (sqr (- y1 y2)))
      (sqr (+ r1 r2))))

;; would-circles-collide? knows about geometry.  It doesn't know about
;; balls: eg it doesn't know the field names of Ball or about
;; ball-selected?.

;; If we changed the representation of balls, to add color, text, or
;; to change the names of the fields, would-circles-collide? wouldn't
;; need to change.

