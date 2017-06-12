#lang racket

(require "interfaces.rkt")

(require "DraggableWidget.rkt")
(require 2htdp/image)                   ; for 'circle'

(provide make-square)

; Wall -> SWidgetListener
(define (make-square w)
  (new Square% [w w]))

;; A square is like a ball, but it has different geometry

(define Square%
  (class* DraggableWidget%

    ;; must implement SWidgetListener + the open hooks from the superclass
    (SWidgetListener<%> DraggableWidgetHooks<%>)

    (inherit-field w)  ;; the Wall that the square should bounce off of

    ;; initial values of x, y (center of square)
    (inherit-field x y speed)

    ; is this selected? Default is false.
    (inherit-field selected?) 

    (inherit-field wall-pos)
   
    (field [size 40])
    (field [half-size (/ size 2)])

    (super-new)

    ;; Square-specific: turn into method

    ;; -> Integer
    ;; position of the square at the next tick
    ;; STRATEGY: use the square's cached copy of the wall position to
    ;; set the upper limit of motion
    (define/override (next-x-pos)
      (limit-value
        half-size
        (+ x speed)
        (-  wall-pos half-size)))

    ;; Number^3 -> Number
    ;; WHERE: lo <= hi
    ;; RETURNS: val, but limited to the range [lo,hi]
    (define (limit-value lo val hi)
      (max lo (min val hi)))

    ;; Square-specific: turn into method

    ;; -> Integer
    ;; RETURNS: the velocity of the square at the next tick
    ;; STRATEGY: if the square will not be at its limit, return it
    ;; unchanged. Otherwise, negate the velocity.
    (define/override (next-speed)
      (if
        (< half-size (next-x-pos) (- wall-pos half-size))
        speed
        (- speed)))

    (define/override (get-image)
      (square size 
        (if selected? "solid" "outline")
        "green"))

    ;; square-specific:

    ;; in-this? : Integer Integer -> Boolean
    ;; GIVEN: a location on the canvas
    ;; RETURNS: true iff the location is inside this.
    (define/override (in-this? other-x other-y)
      (and
       (<= (- x half-size) other-x (+ x half-size))
       (<= (- y half-size) other-y (+ y half-size))))
    
    ))


