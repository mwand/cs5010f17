#lang racket

(require "interfaces.rkt")

(require "DraggableWidget.rkt")
(require 2htdp/image)                   ; for 'circle'

(provide make-ball)
;; FlashingBall% inherits from SBall%, so we need to provide the class
;; as well.
(provide SBall%)

;; Wall -> SWidgetListener
(define (make-ball w) (new SBall% [w w]))

;; The SBall% class

;; Constructor template for SBall%:
;; (new SBall% [x Int][y Int][speed Int]
;;            [saved-mx Integer][saved-my Integer][selected? Boolean]
;;            [w Wall])

(define SBall%
  (class*

    ;; inherit method implementations from DraggableWidget%
    DraggableWidget%
    
    ;; must implement the interface(s) of DraggableWidget% + the open
    ;; hooks from the superclass 
    (SWidgetListener<%> DraggableWidgetHooks<%>)

    ;; inherit all these fields from the superclass:

    ;; the Wall that the ball should bounce off of
    (inherit-field w)  

    ;; initial values of x, y (center of ball) and speed:
    (inherit-field x y speed)

    ; is this selected? Default is false.
    (inherit-field selected?) 

    ;; position of the wall, updated by update-wall-pos
    (inherit-field wall-pos)
    
    ;; this field is local to Ball%
    (field [radius 20])

    (super-new)

    ;; make this a method instead of a function:

    ;; -> Integer
    ;; position of the ball at the next tick
    ;; STRATEGY: use the ball's cached copy of the wall position to
    ;; set the upper limit of motion 
    (define/override (next-x-pos)
      (limit-value
        radius
        (+ x speed)
        (-  wall-pos radius)))

    ;; Number^3 -> Number
    ;; WHERE: lo <= hi
    ;; RETURNS: val, but limited to the range [lo,hi]
    (define (limit-value lo val hi)
      (max lo (min val hi)))

    ;; -> Integer
    ;; RETURNS: the velocity of the ball at the next tick
    ;; STRATEGY: if the ball will not be at its limit, return it
    ;; unchanged. Otherwise, negate the velocity.
    (define/override (next-speed)
      (if
        (< radius (next-x-pos) (- wall-pos radius))
        speed
        (- speed)))

    ;; the image of the ball.  This could be dynamic.
    (define/override (get-image)
      (circle radius 
        "outline"
        "red"))

    ;; in-this? : Integer Integer -> Boolean
    ;; GIVEN: a location on the canvas
    ;; RETURNS: true iff the location is inside this.
    (define/override (in-this? other-x other-y)
      (<= (+ (sqr (- x other-x)) (sqr (- y other-y)))
          (sqr radius)))
    
    ))

