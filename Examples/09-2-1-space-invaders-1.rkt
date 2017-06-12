#lang racket

;; space-invaders-1.rkt

;; the world will consist of a list of Widget<%>'s, and a tick
;; counter to indicate the current time.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; start with (run framerate).  Typically: (run 0.25)

;; Press "b" to drop a new bomb.  
;; Bombs fall at a constant rate. 

;; Helicopter rises at a constant rate.
;; the helicopter is smoothly draggable

;; there is no interaction between the helicopter and the bombs.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require rackunit)
(require 2htdp/universe)
(require 2htdp/image)
(require "extras.rkt")

;;; CONSTANTS

(define CANVAS-WIDTH 200)
(define CANVAS-HEIGHT 400)

(define EMPTY-CANVAS (empty-scene 200 400))

;; some arbitrary choices
(define BOMB-INITIAL-X 75)  
(define BOMB-INITIAL-Y 0)
(define BOMB-INITIAL-RADIUS 10)

(define HELI-INITIAL-X 100)
(define HELI-INITIAL-Y 300)

(define NEW-BOMB-EVENT "b")
(define NEW-HELI-EVENT "h")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Data Definitions

;; A Time is a NonNegative Integer

;; A Widget is an object whose class implements the Widget<%>
;; interface. 

(define-struct world (widgets time))

;; A World is a (make-world ListOfWidget Time)
;; INTERP: (make-world lst t) represents a world containing the
;; widgets in lst at time t (in ticks).

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; INTERFACEs

;; Every object that lives in the world must implement the Widget<%>
;; interface.

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

    ; KeyEvent -> Widget
    ; GIVEN: a key event
    ; RETURNS: the state of this object that should follow the
    ; given key event
    after-key-event     

    ; Scene -> Scene
    ; GIVEN: a scene
    ; RETURNS: a scene like the given one, but with this object
    ; painted on it.
    add-to-scene
    ))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; initial-world : -> World
;; RETURNS: a world with a helicopter and no bombs
(define (initial-world)
  (make-world
    (list (new-heli))
    0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; run : PosReal -> World
; GIVEN: a frame rate, in secs/tick
; EFFECT: runs an initial world at the given frame rate
; RETURNS: the final state of the world
(define (run rate)
  (big-bang (initial-world)
    (on-tick world-after-tick rate)
    (on-draw world-to-scene)
    (on-key  world-after-key-event)
    (on-mouse world-after-mouse-event)))

;; GP: Let's say the world should launch a new helicopter every 10
;; ticks.  How could we accomplish that?

;; world-after-tick : World -> World
;; Use HOFC map on the Widget's in w
(define (world-after-tick w)
  (let ((objs (world-widgets w))
        (t (world-time w)))
    (make-world
      (map
        (lambda (obj) (send obj after-tick))
        objs)
      (+ 1 t))))

;; world-to-scene : World -> Scene
;; Use HOFC foldr on the Widgets in w
(define (world-to-scene w)
  (foldr
    ;; Widget Scene -> Scene
    (lambda (obj scene)
      (send obj add-to-scene scene))
    EMPTY-CANVAS
    (world-widgets w)))


;; world-after-key-event : World KeyEvent -> World
;; STRATEGY: Cases on kev
;; "b" creates a new bomb
;; "h" creates a new heli
;; other keystrokes are passed on to the widgets in the world.

(define (world-after-key-event w kev)
  (let ((objs (world-widgets w))
        (t (world-time w)))
    (cond
      [(key=? kev NEW-BOMB-EVENT)
       (make-world
        (cons (new-bomb t) objs)
        t)]
      [(key=? kev NEW-HELI-EVENT)
       (make-world
        (cons (new-heli) objs)
        t)]
      [else
       (make-world
        (map
         (lambda (obj) (send obj after-key-event kev))
         (world-widgets w))
        t)])))

;; world-after-mouse-event : World Nat Nat MouseEvent -> World
;; STRATGY: Cases on mev
(define (world-after-mouse-event w mx my mev)
  (cond
    [(mouse=? mev "button-down")
     (world-after-button-down w mx my)]
    [(mouse=? mev "drag")
     (world-after-drag w mx my)]
    [(mouse=? mev "button-up")
     (world-after-button-up w mx my)]
    [else w]))

; World Nat Nat -> World
; STRATEGY: Use HOF map on the widgets in w
(define (world-after-button-down w mx my)
  (let ((objs (world-widgets w))
        (t (world-time w)))
    (make-world
      (map
        (lambda (obj) (send obj after-button-down mx my))
        objs)
      t)))
    
     
(define (world-after-button-up w mx my)
  (let ((objs (world-widgets w))
        (t (world-time w)))
    (make-world
      (map
        (lambda (obj) (send obj after-button-up mx my))
        objs)
      t)))

(define (world-after-drag w mx my)
  (let ((objs (world-widgets w))
        (t (world-time w)))
    (make-world
      (map
        (lambda (obj) (send obj after-drag mx my))
        objs)
      t)))

;; Challenge question: Could these 3 functions be generalized? 


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Here's what a class definition looks like:

;; classes are like data definitions.  They should have a purpose statement
;; describing what information they are supposed to represent, and
;; interpretations of the fields describing the meaning of each piece of data.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; We have two classes that implement Widget<%>: Heli% and Bomb%

;; Helicopters start near the bottom of the screen and rise slowly.
;; They are selectable and draggable.

;; Constructor template for Heli%:
;; (new Heli% [x Integer][y Integer]
;;            [selected? Boolean][mx Integer][my Integer])
;; the last 3 arguments are optional
;; Interpretation: An object of class Heli% represents a helicopter.

(define Heli%
  (class* object% (Widget<%>)

    ;; the init-fields are the values that may vary from one helicopter to
    ;; the next.

    ; the x and y position of the center of the helicopter
    (init-field x y)   

    ; is the helicopter selected? Default is false.
    (init-field [selected? false]) 

    ;; if the helicopter is selected, the position of
    ;; the last button-down event inside the helicopter, relative to the
    ;; helicopter's center.  Else any value.
    (init-field [saved-mx 0] [saved-my 0])

    ;; private data for objects of this class.
    ;; these can depend on the init-fields.

    ; the helicopter's radius
    (field [r 15])   

    ; image for displaying the helicopter
    (field [HELI-IMG (circle r "outline" "blue")])
    ; the helicopter's speed, in pixels/tick
    (field [HELISPEED -4])                      
       
    (super-new)
    
    ;; after-tick : -> Widget
    ;; RETURNS: A helicopter like this one, but as it should be after a tick
    ;; a selected helicopter doesn't move.
    ;; STRATEGY: Cases on selected?
    (define/public (after-tick)
      (if selected?
        this
        (new Heli%
          [x x]
          [y (+ y HELISPEED)]
          [selected? selected?]
          [saved-mx saved-mx]
          [saved-my saved-my])))
    
    ;; after-key-event : KeyEvent -> Widget
    ;; RETURNS: A helicopter like this one, but as it should be after the
    ;; given key event.
    ;; DETAILS: a helicopter ignores key events
    (define/public (after-key-event kev)
      this)      
    
    ; after-button-down : Integer Integer -> Widget
    ; GIVEN: the location of a button-down event
    ; STRATEGY: Cases on whether the event is in the helicopter
    (define/public (after-button-down mx my)
      (if (in-heli? mx my)
        (new Heli%
          [x x][y y]
          [selected? true]
          [saved-mx (- mx x)]
          [saved-my (- my y)])
        this))

    ; after-button-up : Integer Integer -> Widget
    ; GIVEN: the location of a button-up event
    ; STRATEGY: Cases on whether the event is in the helicopter.
    ; If the helicopter is selected, then unselect it.
    (define/public (after-button-up mx my)
      (if (in-heli? mx my)
        (new Heli%
          [x x][y y]
          [selected? false]
          [saved-mx saved-mx]
          [saved-my saved-my])
        this))   

    ; after-drag : Integer Integer -> Widget
    ; GIVEN: the location of a drag event
    ; STRATEGY: Cases on whether the helicopter is selected.
    ; If it is selected, move it so that the vector from the center to
    ; the drag event is equal to (mx, my)
    (define/public (after-drag mx my)
      (if selected?
        (new Heli%
          [x (- mx saved-mx)]
          [y (- my saved-my)]
          [selected? true]
          [saved-mx saved-mx]
          [saved-my saved-my])
        this))   


    ;; to-scene : Scene -> Scene
    ;; RETURNS: a scene like the given one, but with this helicopter painted
    ;; on it.
    (define/public (add-to-scene scene)
      (place-image HELI-IMG x y scene))
    
    ;; in-heli? : Integer Integer -> Boolean
    ;; GIVEN: a location on the canvas
    ;; RETURNS: true iff the location is inside this helicopter.
    (define (in-heli? other-x other-y)
      (<= (+ (sqr (- x other-x)) (sqr (- y other-y)))
          (sqr r)))

    ;; test methods, to probe the helicopter state.  Note that we don't have
    ;; a probe for radius.
    ;; -> Int
    (define/public (for-test:x) x)
    ;; -> Int
    (define/public (for-test:y) y)
    ;; -> Boolean
    (define/public (for-test:selected?) selected?)

    ;; -> (list Int Int Boolean)
    (define/public (for-test:heli-state) (list x y selected?))
    
    ))

;; make-heli: -> Widget
;; GIVEN: no arguments
;; RETURNS: a new object of class Heli% near the bottom of the screen.

;; NOTE: the contract says Widget, because our contracts are ALWAYS in
;; terms of interfaces (remember, a Widget is an object that
;; implements Widget<%>).  The purpose statement gives the additional
;; information that the Widget returned by make-heli happens to be an
;; object of class Heli%.

(define (new-heli)
  (new Heli% [x HELI-INITIAL-X][y HELI-INITIAL-Y]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Bombs start near the top of the screen.  They just fall.

;; Constructor template for Bomb%:
;; (new Bomb% [x Integer][y Integer])
;; Interpretation: An object of class Bomb% represents a bomb.

;; make-bomb : Time -> Widget
;; GIVEN: A time t
;; RETURNS: a new object of class Bomb% near the top of the screen.
;; The time argument is ignored.

;; SEE NOTE ABOVE ABOUT THIS CONTRACT

(define (new-bomb t)
  (new Bomb% [x BOMB-INITIAL-X][y BOMB-INITIAL-Y]))

(define Bomb%

  (class* object% (Widget<%>)
    (init-field x y)  ; the bomb's x and y position
    
    ;; private data for objects of this class.
    ;; these can depend on the init-fields.

    ;; image for displaying the bomb
    (field [BOMB-IMG (circle 10 "solid" "red")])
    ; the bomb's speed, in pixels/tick
    (field [BOMB-SPEED 8])
   
    (super-new)
    
    ;; after-tick : -> Widget
    ;; RETURNS: A bomb like this one, but as it should be after a tick
    ;; DETAILS: the bomb moves vertically by BOMB-SPEED
    (define/public (after-tick)
      (new Bomb% [x x][y (+ y BOMB-SPEED)]))
    
    ;; to-scene : Scene -> Scene
    ;; RETURNS: a scene like the given one, but with this bomb painted
    ;; on it.
    (define/public (add-to-scene scene)
      (place-image BOMB-IMG x y scene))   

    ;; the bomb doesn't have any other behaviors
    (define/public (after-button-down mx my) this)
    (define/public (after-drag mx my) this)
    (define/public (after-button-up mx my) this)
    (define/public (after-key-event kev) this)
    
    ;; test methods, to probe the bomb state.
    (define/public (for-test:x) x)
    (define/public (for-test:y) y)

    ))








