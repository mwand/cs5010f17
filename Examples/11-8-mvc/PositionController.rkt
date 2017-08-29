#lang racket

;; displays as an outline rectangle with text showing the x
;; coordinate and velocity of the particle.

;; the rectangle is draggable

;; +,- increments or decrements location of the particle by 5

(require 2htdp/image)
(require 2htdp/universe)
(require "interfaces.rkt")

(provide make-position-controller)

;; make-position-controller : Model -> Controller
;; GIVEN: a model m
;; RETURNS: A position controller for m

(define (make-position-controller m)
  (new PositionController% [model m]))


;; Constructor template for PositionController%
;; (new PositionController% [model Model])

(define PositionController%
  (class* object% (Controller<%>)

    (init-field model)  ; the model

    ; Position of the center of the controller
    ; both of these are NonNegInts.
    (init-field [x 150] [y 100])   

    ; width and height of the controller.  Both PosInts.
    (init-field [width 120][height 50])

    (field [half-width  (/ width  2)])
    (field [half-height (/ height 2)])

    ;; controller's cache of the position and velocity of the
    ;; particle.
    ;; both Reals.
    (field [particle-x 0])
    (field [particle-v 0])

    ;; fields for dragging
    ;; if selected? then position of last button-down relative to
    ;; center of viewer; else any value
    (field [selected? false])
    (field [saved-mx 0])
    (field [saved-my 0])

    (super-new)

    ;; at initialization, register this controller with the model
    (send model register this)
    
    ;; Signal -> Void
    ;; decodes signal and updates local data
    (define/public (receive-signal sig)
      (cond
        [(report-position? sig)
         (set! particle-x (report-position-pos sig))]
        [(report-velocity? sig)
         (set! particle-v (report-velocity-v sig))]))

    ; after-button-down : Integer Integer -> Void
    ; GIVEN: the location of a button-down event
    ; EFFECT: makes the viewer selected
    ; The viewer stays selected until a button down somewhere else
    ; STRATEGY: Cases on whether the event is in this object
    (define/public (after-button-down mx my)
      (if (in-this? mx my)
        (begin
          (set! selected? true)
          (set! saved-mx (- mx x))
          (set! saved-my (- my y)))
        (set! selected? false)))
        
    ; after-button-up : Integer Integer -> Void
    ; GIVEN: the (x,y) location of a button-up event
    ; EFFECT: ignored

    (define/public (after-button-up mx my) 'ignored)


    ; after-drag : Integer Integer -> Void
    ; GIVEN: the location of a drag event
    ; STRATEGY: Cases on whether this is selected.
    ; If it is selected, move it so that the vector from its position to
    ; the drag event is equal to saved-mx.  Report the new position to
    ; the registered balls.

    (define/public (after-drag mx my)
      (if selected?
        (begin
          (set! x (- mx saved-mx))
          (set! y (- my saved-my)))
        'position-controller-after-drag-value))

    ;; the position controller doesn't respond to mouse move events
    (define/public (after-move mx my)
      'position-controller-after-move-value)

    ;; Int Int -> Boolean
    (define (in-this? other-x other-y)
      (and
        (<= (- x half-width) other-x (+ x half-width))
        (<= (- y half-height) other-y (+ y half-height))))

    ;; add-to-scene : Scene -> Scene
    ;; RETURNS: a scene like the given one, but with this wall painted
    ;; on it.
    ;; STRATEGY: place the image centered at x y

    (define/public (add-to-scene scene)
      (place-image (viewer-image) x y scene))
    
    ;; the controller doesn't respond to ticks
    (define/public (after-tick)
      'position-controller-after-tick-value)

    ;; KeyEvent -> Void
    ;; interpret +,- as commands to the model
    ;; +/- alter position of the particle
    (define/public (after-key-event kev)
      (if selected?
        (cond
          [(key=? "+" kev)
           (send model execute-command
             (make-set-position (+ particle-x 5)))]
          [(key=? "-" kev)
           (send model execute-command
             (make-set-position (- particle-x 5))
             )])
        'position-controller-after-key-event-value))

    ;; -> Image
    ;; RETURNS: the image of the viewer
    ;; STRATEGY: assemble the image from the data and rectangle
    (define (viewer-image)
      (let ((the-data-image (data-image)))
        (overlay 
          the-data-image
          (rectangle
            (max width (+ (image-width the-data-image) 10))
            (max height (+ (image-height the-data-image) 10))
            "outline"
            (current-color)))))

    ;; -> Image
    (define (data-image)
      (above
        (text "+/- : Change position" 10 "black")
        (text (string-append
                "X = "
                (number->string particle-x)
                " Velocity = "
                (number->string particle-v))
          12
          "black")))

    (define (current-color)
      (if selected? "red" "black"))

    ))

