#lang racket

(require 2htdp/image)

(require "interfaces.rkt")

(provide make-wall)

;; -> SWidgetPublisher
;; RETURNS: a wall at the initial position
(define (make-wall) (new SWall%))

(define INITIAL-WALL-POSITION 300)

;; this should be the same as in 
(define CANVAS-HEIGHT 300)


(define SWall%
  (class* object% (SWidgetPublisher<%>)

    (init-field [pos INITIAL-WALL-POSITION]) ; the x position of the wall

    ; is the wall selected? Default is false.
    (init-field [selected? false]) 

    ;; if the wall is selected, the x position of
    ;; the last button-down event near the wall, otherwise can be any
    ;; value. 
    (init-field [saved-mx 0])
       
    ;; the list of registered listeners
    ;; ListOf(WidgetListener<%>)
    (field [listeners empty])  

    (super-new)

    ;; WidgetListener<%> -> Int
    ;; EFFECT: registers the given listener
    ;; RETURNS: the current position of the wall
    (define/public (register b)
      (begin
        (set! listeners (cons b listeners))
        pos))

    ;; Mouse responses.  How much of this could be shared using
    ;; DraggableWidget? 

    ; after-button-down : Integer Integer -> Void
    ; GIVEN: the (x, y) location of a button-down event
    ; EFFECT: if the event is near the wall, make the wall selected.
    ; STRATEGY: Cases on whether the event is near the wall
    (define/public (after-button-down mx my)
      (if (near-wall? mx)
        (begin
          (set! selected? true)
          (set! saved-mx (- mx pos)))
        this))

    ; after-button-up : Integer Integer -> Void
    ; GIVEN: the (x,y) location of a button-up event
    ; EFFECT: makes the Wall unselected
    (define/public (after-button-up mx my)
      (set! selected? false))

    ; after-drag : Integer Integer -> Void
    ; GIVEN: the (x,y) location of a drag event
    ; STRATEGY: Cases on whether the wall is selected.
    ; If it is selected, move it so that the vector from its position to
    ; the drag event is equal to saved-mx.  Report the new position to
    ; the registered listeners
    (define/public (after-drag mx my)
      (if selected?
        (begin
          (set! pos (- mx saved-mx))
          (for-each
            (lambda (b) (send b update-wall-pos pos))
            listeners))
        this))


    ;; add-to-scene : Scene -> Scene
    ;; RETURNS: a scene like the given one, but with this wall painted
    ;; on it.
    (define/public (add-to-scene scene)
      (scene+line scene pos 0 pos CANVAS-HEIGHT  "black"))
    
    ;; is mx near the wall?  We arbitrarily say it's near if its
    ;; within 5 pixels.
    (define (near-wall? mx)
      (< (abs (- mx pos)) 5))

    ;; the wall has no other behaviors
    (define/public (after-tick) this)
    (define/public (after-key-event kev) this)
    
    ))


