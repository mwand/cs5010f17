#lang racket

(require 2htdp/image)
(require "interfaces.rkt")

;; this module defines a superclass, from which other classes will
;; inherit. So we need to provide the class itself, not a constructor
;; function.

;; Since you can't create an object of this class, there is no
;; constructor template, either.

;; Any subclass will have to require this file, so it will get both
;; the superclass and the additional interface.

(provide DraggableWidget% DraggableWidgetHooks<%>)

;; constants for all draggable widgets:

(define INIT-WIDGET-X (/ CANVAS-HEIGHT 2))
(define INIT-WIDGET-Y (/ CANVAS-WIDTH 3))
(define INIT-WIDGET-SPEED 25)

(define DraggableWidget%
  (class* object%

    ;; the methods implemented in the superclass
    ; (DraggableWidget<%>)
    (SWidgetListener<%>)

    ;; the methods to be supplied by each subclass
    (abstract get-image ; WAS: add-to-scene
              next-x-pos
              next-speed
              in-this?)
    

    ;; the Wall that the ball should bounce off of
    (init-field w)  

    ;; initial values of x, y (center of widget)
    (init-field [x INIT-WIDGET-X])
    (init-field [y INIT-WIDGET-Y])
    (init-field [speed INIT-WIDGET-SPEED])

    ; is this selected? Default is false.
    (init-field [selected? false]) 

    ;; if this is selected, the position of
    ;; the last button-down event inside this, relative to the
    ;; object's center.  Else any value.
    (init-field [saved-mx 0] [saved-my 0])

    ;; register this ball with the wall, and use the result as the
    ;; initial value of wall-pos
    (field [wall-pos (send w register this)])
    
    (super-new)

    ;; Int -> Void
    ;; EFFECT: updates the widget's idea of the wall's position to the
    ;; given integer.
    (define/public (update-wall-pos n)
      (set! wall-pos n))

    ;; after-tick : -> Void
    ;; state of this ball after a tick.  A selected widget doesn't move.
    (define/public (after-tick)
      (if selected?
        this
        (let ((x1     (send this next-x-pos))
              (speed1 (send this next-speed)))
          (begin
            (set! speed speed1)
            (set! x x1)))))

    ;; instead of having add-to-scene an abstract method, we keep it
    ;; in the superclass, and make the differences abstract:

    (define/public (add-to-scene s)
      (place-image
        (send this get-image)
        x y s))

    ; after-button-down : Integer Integer -> Void
    ; GIVEN: the location of a button-down event
    ; STRATEGY: Cases on whether the event is in this
    (define/public (after-button-down mx my)
      (if (send this in-this? mx my)
        (begin
          (set! selected? true)
          (set! saved-mx (- mx x))
          (set! saved-my (- my y)))
        this))

    ; after-button-up : Integer Integer -> Void
    ; GIVEN: the (x,y) location of a button-up event
    ; STRATEGY: Cases on whether the event is in this
    ; If this is selected, then unselect it.
    (define/public (after-button-up mx my)
      (if (send this in-this? mx my)
        (set! selected? false)
        'error-276))

    ; after-drag : Integer Integer -> Void
    ; GIVEN: the (x, y) location of a drag event
    ; STRATEGY: Cases on whether the ball is selected.
    ; If it is selected, move it so that the vector from the center to
    ; the drag event is equal to (mx, my)
    (define/public (after-drag mx my)
      (if selected?
        (begin
          (set! x (- mx saved-mx))
          (set! y (- my saved-my)))
        'error-277))   

    ;; the widget ignores key events
    (define/public (after-key-event kev) this)

    (define/public (for-test:x)          x)
    (define/public (for-test:speed)      speed)
    (define/public (for-test:wall-pos)   wall-pos)
    (define/public (for-test:next-speed) (next-speed))
    (define/public (for-test:next-x)     (next-x-pos))
    
    ))

;; the abstract methods of DraggableWidget

(define DraggableWidgetHooks<%>
  (interface ()

    ;; Int Int -> Boolean
    ;; is the given location in this widget?
    in-this?

    ;; -> Int
    ;; RETURNS: the next x position or speed of this widget
    next-x-pos
    next-speed

    ;; -> Image
    ;; RETURNS: the image of this widget for display
    get-image

    ))
