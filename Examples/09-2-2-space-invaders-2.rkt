#lang racket

;; space-invaders-2.rkt

;; just like space-invaders-1, but in this version, the world will be an object

;; the world will consist of a list of Widget<%>'s, and a tick
;; counter to indicate the current time.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; start with (run framerate).  Typically: (run 0.25)

;; Press "b" to drop a new bomb.  
;; Bombs fall at a constant rate. 

;; Press "h" to start a new helicopter
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

(define EMPTY-CANVAS (empty-scene CANVAS-WIDTH CANVAS-HEIGHT))

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

;; A World is an object whose class implements the World<%>
;; interface. 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; INTERFACES

;; big-bang will communicate with the world through the World<%>
;; interface. 

(define World<%>
  (interface ()

    ; -> World
    ; GIVEN: no arguments
    ; RETURNS: the state of the world at the next tick
    after-tick          

    ; Integer Integer MouseEvent-> World
    ; GIVEN: a location
    ; RETURNS: the state of the world that should follow the
    ; given mouse event at the given location.
    after-mouse-event

    ; KeyEvent -> World
    ; GIVEN: a key event
    ; RETURNS: the state of the world that should follow the
    ; given key event
    after-key-event     

    ; -> Scene
    ; GIVEN: a scene
    ; RETURNS: a scene that depicts this World
    to-scene
    ))


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
    (on-tick
      (lambda (w) (send w after-tick))
      rate)
    (on-draw
      (lambda (w) (send w to-scene)))
    (on-key
      (lambda (w kev)
        (send w after-key-event kev)))
    (on-mouse
      (lambda (w mx my mev)
        (send w after-mouse-event mx my mev)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The World% class

;; We will have only one class that implements World<%>: World%

;; Constructor template for World%:
;; (new World% [objs ListOfWidget][t Time])
;; Interpretation: An object of class World% takes signals from
;; big-bang and distributes them to its objects as appropriate.


;; make-world : ListOfWidget Time -> World
;; GIVEN: a list of widgets and a time
;; RETURNS: an object of class World% containing the given list of
;; widgets and time.
(define (make-world objs t)
  (new World% [objs objs][t t]))

(define World%
  (class* object% (World<%>)

    (init-field objs) ;  ListOfWidget
    (init-field t)    ;  Time

    (super-new)

    ;; after-tick : -> World
    ;; Use HOFC map on the Widget's in this World

    (define/public (after-tick)
      (make-world
        (map
          (lambda (obj) (send obj after-tick))
          objs)
        (+ 1 t)))

    ;; to-scene : -> Scene
    ;; Use HOFC foldr on the Widgets in this World

    (define/public (to-scene)
      (foldr
        (lambda (obj scene)
          (send obj add-to-scene scene))
        EMPTY-CANVAS
        objs))


    ;; after-key-event : KeyEvent -> World
    ;; STRATEGY: Cases on kev
    ;; "b" and "h" create new bomb and new helicopter;
    ;; other keystrokes are passed on to the objects in the world.

    (define/public (after-key-event kev)
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
              objs)
            t)]))

    ;; after-mouse-event : Nat Nat MouseEvent -> World
    ;; STRATEGY: Cases on mev
    (define/public (after-mouse-event mx my mev)
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
      (make-world
        (map
          (lambda (obj) (send obj after-button-down mx my))
          objs)
        t))
    
     
    (define (world-after-button-up mx my)
      (make-world
        (map
          (lambda (obj) (send obj after-button-up mx my))
          objs)
        t))

    (define (world-after-drag mx my)
      (make-world
        (map
          (lambda (obj) (send obj after-drag mx my))
          objs)
        t))

    ))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; We have two classes of Widgets:  Helicopters and Bombs

;; Helicopters start near the bottom of the screen and rise slowly.
;; They are selectable and draggable.

;; Constructor template for Heli%:
;; (new Heli% [x Integer][y Integer]
;;            [selected? Boolean][mx Integer][my Integer])
;; the last 3 arguments are optional
;; Interpretation: An object of class Heli% represents a helicopter.


;; unchanged from 09-2-1.
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
    
    ;; after-tick : Time -> Widget
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
    ;; RETURNS: A world like this one, but as it should be after the
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
    ;; DETAILS: the bombcopter moves vertically by BOMB-SPEED
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




