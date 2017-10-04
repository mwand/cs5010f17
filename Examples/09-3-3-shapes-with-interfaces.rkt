#lang racket

;;  09-3-shapes.rkt

(require rackunit)
(require "extras.rkt")
(require 2htdp/image)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; A Tiny System for creating and drawing shapes on a canvas.

;;; demo with (demo)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; We don't know what shapes there will be, but we know what
;; operations we will want on shapes.  This is the shape interface:

;;; INTERFACE:

;; a Shape is an object of a class that implements Shape<%>.

(define Shape<%> 
  (interface ()

    ;; weight : -> Number
    ;; RETURNS: the weight of this shape
    weight 

    ;; add-to-scene : Scene -> Scene
    ;; RETURNS: a scene like the given one, but with this shape
    ;; painted on it.
    add-to-scene

    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Classes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; We will have a class for each kind of shape

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Circles

;; Constructor Template for Circle%:
;; (new Circle% [x Integer][y Integer][r Integer][c ColorString])
;; Interpretation: a circle on the canvas.

(define Circle%
  (class* object% (Shape<%>)
    (init-field 
      x                         ; Integer, x pixels of center from left
      y                         ; Integer, y pixels of center from top
      r                         ; Integer, radius
      c)                        ; ColorString 

    (field [IMG (circle r "solid" c)])

    (super-new)

    ;; for each method, we copy down the contract and purpose
    ;; statement from the interface, with perhaps additional details
    ;; relating to this class.

    ;; weight : -> Integer
    ;; RETURNS: the weight of this shape
    ;; DETAILS: this shape is a circle
    ;; STRATEGY: combine simpler functions
    (define/public (weight) (* pi r r))

    ;; add-to-scene : Scene -> Scene
    ;; RETURNS: a scene like the given one, but with this shape
    ;; painted on it.
    ;; DETAILS: this shape is a circle
    ;; STRATEGY: call a more general function
    (define/public (add-to-scene s) (place-image IMG x y s))

    ))

;; Constructor Template for Square%:
;; (new Square% [x Integer][y Integer][l Integer][c ColorString])
;; Interpretation: a square parallel to sides of canvas 


(define Square%
  (class* object% (Shape<%>)
    (init-field x  ; Integer, x pixels of center from left
                y  ; Integer, y pixels of center from top
		l  ; Integer, length of one side 
		c) ; ColorString 

    (field [IMG (rectangle l l "solid" c)])

    (super-new)

    ;; weight : -> Real
    ;; RETURNS: the weight of this shape
    ;; DETAILS: this shape is a square
    ;; STRATEGY: combine simpler functions
    (define/public (weight) (* l l))

    ;; add-to-scene : Scene -> Scene
    ;; RETURNS: a scene like the given one, but with this shape
    ;; painted on it.
    ;; DETAILS: this shape is a square
    ;; STRATEGY: call a more general function
    (define/public (add-to-scene s) (place-image IMG x y s))

    ))

;; Constructor Template for Composite%:
;; a (new Composite% [front Shape][back Shape])
;; Interpretation: a composite of front and back shapes

(define Composite%
  (class* object% (Shape<%>)
    (init-field 
      front   ; Shape, the shape in front
      back    ; Shape, the shape in back
      )

    (super-new)

    ;; all we know here is that front and back implement Shape<%>.
    ;; we don't know if they are circles, squares, or other composites!

    ;; weight : -> Number
    ;; RETURNS: the weight of this shape
    ;; DETAILS: this shape is a composite
    ;; STRATEGY: recur on the components
    (define/public (weight) (+ (send front weight)
                              (send back weight)))

    ;; add-to-scene : Scene -> Scene
    ;; RETURNS: a scene like the given one, but with this shape
    ;; painted on it.
    ;; DETAILS: this shape is a composite
    ;; strategy: recur on the components
    (define/public (add-to-scene scene)
      (send front add-to-scene
        (send back add-to-scene scene)))
     
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define EMPTY-CANVAS (empty-scene 200 100))

(define s1 (new Circle% [x 50][y 20][r 40][c "red"]))
(define s2 (new Square% [x 80][y 70][l 40][c "blue"]))
(define s3 (new Composite% [front s1][back s2]))


(begin-for-test
  (check-= (send s1 weight) (* pi 1600) .1)
  (check-equal? (send s2 weight) 1600)
  (check-= (send s3 weight) (+ (* pi 1600) 1600) .1))

(define (demo)
  (send s3 add-to-scene EMPTY-CANVAS))
