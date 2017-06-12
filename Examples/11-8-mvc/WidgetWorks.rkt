#lang racket

(require 2htdp/universe)
(require 2htdp/image)

(provide 
 container-init
 Container<%>
 Widget<%>
 SWidget<%>)

;; A Container is an object of any class that implements Container<%>.
;; In Widgetworks, there is only one such class.

(define Container<%>
  (interface ()

   ; Widget -> Void
   ; GIVEN: A widget
   ; EFFECT: add the given widget to the world
   add-widget

   ; SWidget -> Void
   ; GIVEN: A stateful widget
   ; EFFECT: add the given widget to the world
   add-stateful-widget

   ; PosReal -> Void
   ; GIVEN: a framerate, in secs/tick
   ; EFFECT: runs this world at the given framerate
   run

    ))

;; A Widget is an object of any class that implements Widget<%>

(define Widget<%>
  (interface ()

    ; -> Widget
    ; GIVEN: no arguments
    ; RETURNS: the state of this object that should follow at time t+1.
    after-tick          

    ; Integer Integer -> Widget
    ; GIVEN: a location
    ; RETURNS: the state of this object that should follow the
    ; specified mouse event at the given location.
    after-button-down
    after-button-up
    after-drag
    after-move

    ; KeyEvent -> Widget
    ; GIVEN: a key event and a time
    ; RETURNS: the state of this object that should follow the
    ; given key event
    after-key-event     

    ; Scene -> Scene
    ; GIVEN: a scene
    ; RETURNS: a scene like the given one, but with this object
    ; painted on it.
    add-to-scene
    ))

;; An SWidget is an object of any class that implements the SWidget<%>
;; interface.

;; A SWidget is like a Widget, but it is stable (stateful).

(define SWidget<%>
  (interface ()

    ; -> Void
    ; GIVEN: no arguments
    ; EFFECT: updates this widget to the state it should have
    ; following a tick.
    after-tick          

    ; Integer Integer -> Void
    ; GIVEN: a location
    ; EFFECT: updates this widget to the state it should have
    ; following the specified mouse event at the given location.
    after-button-down
    after-button-up
    after-drag
    after-move

    ; KeyEvent -> Void
    ; GIVEN: a key event
    ; EFFECT: updates this widget to the state it should have
    ; following the given key event
    after-key-event     

    ; Scene -> Scene
    ; GIVEN: a scene
    ; RETURNS: a scene like the given one, but with this object
    ; painted on it.
    add-to-scene
    ))

; ListOfWidget ListOfStatefulWidget -> Container
(define (container-init w h)
  (new WWContainer% [canvas-width w][canvas-height h]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; note: WWContainer% is NOT provided.  Only Container<%> is provided.

(define WWContainer%
  (class* object% (Container<%>)

    (init-field canvas-width)
    (init-field canvas-height)
       
    (init-field [objs empty])  ; ListOfWidget
    (init-field [sobjs empty]) ; ListOfSWidget

    (field [EMPTY-CANVAS (empty-scene canvas-width canvas-height)])

    (super-new)

    ; run : PosReal -> World
    ; GIVEN: a frame rate, in secs/tick
    ; EFFECT: runs this world at the given frame rate
    ; RETURNS: the world in its final state of the world
    ; Note: the (begin (send w ...) w) idiom
    
    (define/public (run rate)
      (big-bang this
        (on-tick
          (lambda (w) (begin (after-tick) w))
          rate)
        (on-draw
          (lambda (w) (to-scene)))
        (on-key
          (lambda (w kev)
            (begin
              (after-key-event kev)
              w)))
        (on-mouse
          (lambda (w mx my mev)
            (begin
              (after-mouse-event mx my mev)
              w)))))

    ;; Widget -> Void
    (define/public (add-widget w)
      (set! objs (cons w objs)))

   ;; Widget -> Void
    (define/public (add-stateful-widget w)
      (set! sobjs (cons w sobjs)))

   ;; ((Widget -> Widget) && (SWidget -> Void)) -> Void
   ;; this means that fn must satisfy both (Widget -> Widget) and
   ;; (SWidget -> Void) 
   (define (process-widgets fn)
      (begin
        (set! objs (map fn objs))
        (for-each fn sobjs)))

    ;; after-tick : -> Void
    ;; Use map on the Widgets in this World; use for-each on the
    ;; stateful widgets

    (define (after-tick)
      (process-widgets
        (lambda (obj) (send obj after-tick))))

    ;; to-scene : -> Scene
    ;; Use HOFC foldr on the Widgets and SWidgets in this World
    ;; Note: the append is inefficient, but clear.  We expect that
    ;; most of the widgets in the world will be stateful.
      
    (define (to-scene)
      (foldr
        (lambda (obj scene)
          (send obj add-to-scene scene))
        EMPTY-CANVAS
        (append objs sobjs)))

    ;; after-key-event : KeyEvent -> Void
    ;; STRATEGY: Pass the KeyEvents on to the objects in the world.

    (define (after-key-event kev)
      (process-widgets
        (lambda (obj) (send obj after-key-event kev))))

    ;; after-mouse-event : Nat Nat MouseEvent -> Void
    ;; STRATGY: Cases on mev
    (define (after-mouse-event mx my mev)
      (cond
        [(mouse=? mev "button-down")
         (world-after-button-down mx my)]
        [(mouse=? mev "drag")
         (world-after-drag mx my)]
        [(mouse=? mev "button-up")
         (world-after-button-up mx my)]
        [else this]))

    ;; the next few functions are local functions, not in the interface.

    (define (world-after-button-down mx my)
      (process-widgets
       (lambda (obj) (send obj after-button-down mx my))))
    
     
    (define (world-after-button-up mx my)
      (process-widgets
        (lambda (obj) (send obj after-button-up mx my))))


    (define (world-after-drag mx my)
      (process-widgets
        (lambda (obj) (send obj after-drag mx my))))

    ))
