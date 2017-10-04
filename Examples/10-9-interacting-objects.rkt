#lang racket

;; 10-8-interacting-objects.

;; Based on 10-7-push-model-fixed.rkt

;; In this version, we'll allow the balls to interact with the wall
;; directly. When a ball is selected, "a" attracts the wall-- moves
;; the wall 50% closer to the ball; "r" repels the wall-- moves the
;; wall 50% farther away.

;; To do this, we'll add a move-to method to the wall

(require rackunit)
(require 2htdp/universe)
(require 2htdp/image)
(require "extras.rkt")


;; start with (run framerate).  Typically: (run 0.25)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; CONSTANTS

(define CANVAS-WIDTH 400)
(define CANVAS-HEIGHT 300)

(define EMPTY-CANVAS (empty-scene CANVAS-WIDTH CANVAS-HEIGHT))


(define INIT-BALL-X (/ CANVAS-HEIGHT 2))
(define INIT-BALL-Y (/ CANVAS-WIDTH 3))

;; balls will move slower to make them easier to select.
(define INIT-BALL-SPEED 20)

(define INITIAL-WALL-POSITION 300)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; INTERFACES

;; An SWorld is an object of any class that implements SWorld<%>

(define SWorld<%>
  (interface ()

    ; -> Void
    ; GIVEN: no arguments
    ; EFFECT: updates this world to its state after a tick
    after-tick          

    ; Integer Integer MouseEvent-> Void
    ; GIVEN: a location
    ; EFFECT: updates this world to the state that should follow the
    ; given mouse event at the given location.
    after-mouse-event

    ; KeyEvent -> Void
    ; GIVEN: a key event
    ; EFFECT: updates this world to the state that should follow the
    ; given key event
    after-key-event     

    ; -> Scene
    ; GIVEN: a scene
    ; RETURNS: a scene that depicts this World
    to-scene

   ; Widget -> Void
   ; GIVEN: A widget
   ; EFFECT: add the given widget to the world
   add-widget

   ; SWidget -> Void
   ; GIVEN: A stateful widget
   ; EFFECT: add the given widget to the world
   add-stateful-widget

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


;; An SBall is an object of any class that implements SBall<%>.

;; An SBall is like a Ball, but it is stateful (stable), so its
;; interface extends SWidget<%> rather than Widget<%>.

;; It has one extra method, which updates the ball's copy of the
;; wall's position.

(define SBall<%>
  (interface (SWidget<%>)

    ; Int -> Void
    ; EFFECT: updates the ball's cached value of the wall's position
    update-wall-pos

    ))

;; The wall will be stable (stateful), so its interface
;; extends SWidget<%> instead of Widget<%>.

;; An SWall is an object of any class that implements SWall<%>.
;; There will be only one such class.

;; SWall<%> extends SWidget<%> instead of Widget<%>.

;; Instead of waiting to be asked, in this version the wall publishes
;; its position to anyone who puts themselves on the list to be
;; notified.  It does so by calling the the recipient's
;; 'update-wall-pos' method.

;; So SWall<%> has a 'register' method, which allows any SBall to sign up
;; for notifications.

;; for 10-9-interacting-objects, we add a move-to method, which allows
;; any object that knows about the wall to move it to a given
;; x-position.

(define SWall<%>
  (interface (SWidget<%>)

    ; SBall -> Int
    ; GIVEN: An SBall
    ; EFFECT: registers the ball to receive position updates from this wall.
    ; RETURNS: the x-position of the wall
    register

    ; Int -> Void
    ; EFFECT: moves the wall to the given position.  Notifies all the
    ; registered balls about the change.
    move-to

    ))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; initial-world : -> World
;; RETURNS: a world with a wall, a ball, and a factory

(define (initial-world)
  (local
    ((define the-wall (new SWall%))
     (define the-ball (new SBall% [w the-wall]))
     (define the-world
       (make-sworld 
         empty
         (list the-ball the-wall)))
     (define the-factory
       (new BallFactory% [wall the-wall][world the-world])))
    (begin
      ;; put the factory in the world
      (send the-world add-stateful-widget the-factory)
      ;; tell the factory to start a ball
      (send the-factory after-key-event "b")
      the-world)))
     
     
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; run : PosReal -> World
; GIVEN: a frame rate, in secs/tick
; EFFECT: runs an initial world at the given frame rate
; RETURNS: the world in its final state of the world
; Note: the (begin (send w ...) w) idiom
(define (run rate)
  (big-bang (initial-world)
    (on-tick
      (lambda (w) (begin (send w after-tick) w))
      rate)
    (on-draw
      (lambda (w) (send w to-scene)))
    (on-key
      (lambda (w kev)
        (begin
          (send w after-key-event kev)
          w)))
    (on-mouse
      (lambda (w mx my mev)
        (begin
          (send w after-mouse-event mx my mev)
          w)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The SWorld% class



; ListOfWidget ListOfSWidget -> World
(define (make-sworld objs sobjs)
  (new SWorld% [objs objs][sobjs sobjs]))

(define SWorld%
  (class* object% (SWorld<%>)
    
    (init-field objs)   ; ListOfWidget
    (init-field sobjs)  ; ListOfSWidget

    (super-new)

    (define/public (add-widget w)
      (set! objs (cons w objs)))

   (define/public (add-stateful-widget w)
      (set! sobjs (cons w sobjs)))

    ;; ((Widget -> Widget) && (SWidget -> Void)) -> Void
    (define (process-widgets fn)
      (begin
        (set! objs (map fn objs))
        (for-each fn sobjs)))

    ;; after-tick : -> Void
    ;; Use map on the Widgets in this World; use for-each on the
    ;; stateful widgets

    (define/public (after-tick)
      (process-widgets
        (lambda (obj) (send obj after-tick))))

    ;; to-scene : -> Scene
    ;; Use HOFC foldr on the Widgets and SWidgets in this World
    ;; Note: the append is inefficient, but clear..
      
    (define/public (to-scene)
      (foldr
        (lambda (obj scene)
          (send obj add-to-scene scene))
        EMPTY-CANVAS
        (append objs sobjs)))

    ;; after-key-event : KeyEvent -> Void
    ;; STRATEGY: Pass the KeyEvents on to the objects in the world.

    (define/public (after-key-event kev)
      (process-widgets
        (lambda (obj) (send obj after-key-event kev))))

    ;; world-after-mouse-event : Nat Nat MouseEvent -> Void
    ;; STRATGY: Cases on mev
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
      (process-widgets
       (lambda (obj) (send obj after-button-down mx my))))
    
     
    (define (world-after-button-up mx my)
      (process-widgets
        (lambda (obj) (send obj after-button-up mx my))))


    (define (world-after-drag mx my)
      (process-widgets
        (lambda (obj) (send obj after-drag mx my))))

    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; The BallFactory% class

;; accepts "b" key events and adds them to the world.
;; gets the world as an init-field

;; 10-6: in the push model, the ball is a stateful widget

(define BallFactory%
  (class* object% (SWidget<%>)

    (init-field world)  ; the world to which the factory adds balls
    (init-field wall)   ; the wall that the new balls should bounce
                        ; off of.

    (super-new)

    (define/public (after-key-event kev)
      (cond
        [(key=? kev "b")
         (send world add-stateful-widget (new SBall% [w wall]))]))

    ;; the Ball Factory has no other behavior

    (define/public (after-tick) this)
    (define/public (after-button-down mx my) this)
    (define/public (after-button-up mx my) this)
    (define/public (after-drag mx my) this)
    (define/public (add-to-scene s) s)

    ))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The SBall% class

;; Constructor template for SBall%:
;; (new SBall% [x Int][y Int][speed Int]
;;            [saved-mx Integer][saved-my Integer][selected? Boolean]
;;            [w Wall])

;; As of 10-6, the Ball is now a stateful widget

(define SBall%
  (class* object% (SWidget<%>)

    (init-field w)  ;; the Wall that the ball should bounce off of

    ;; initial values of x, y (center of ball)
    (init-field [x INIT-BALL-X])
    (init-field [y INIT-BALL-Y])
    (init-field [speed INIT-BALL-SPEED])

    ; is this selected? Default is false.
    (init-field [selected? false]) 

    ;; if this is selected, the position of
    ;; the last button-down event inside this, relative to the
    ;; ball's center.  Else any value.
    (init-field [saved-mx 0] [saved-my 0])
   
    (field [radius 20])

    ;; register this ball with the wall, and use the result as the
    ;; initial value of wall-pos
    (field [wall-pos (send w register this)])
    
    (super-new)

    ;; Int -> Void
    ;; EFFECT: updates the ball's idea of the wall's position to the
    ;; given integer.
    (define/public (update-wall-pos n)
      (set! wall-pos n))

    
    ;; after-tick : -> Void
    ;; state of this ball after a tick.  A selected ball doesn't move.
    (define/public (after-tick)
      (if selected?
        this
        ;; (new Ball%
        ;;   [x (next-x-pos)]
        ;;   [y y]
        ;;   [speed (next-speed)]
        ;;   [selected? selected?]
        ;;   [saved-mx saved-mx]
        ;;   [saved-my saved-my]
        ;;   [w w])
        (let ((x1 (next-x-pos))
              (speed1 (next-speed)))
          ;; (next-speed) depends on x, and (next-x-pos) depends on
          ;; speed, so they have to be done independently before doing
          ;; any assignments.
          (begin
            (set! speed speed1)
            (set! x x1)))))

    ;; -> Integer
    ;; position of the ball at the next tick
    ;; STRATEGY: use the ball's cached copy of the wall position to
    ;; set the upper limit of motion
    (define (next-x-pos)
      (limit-value
        radius
        (+ x speed)
        (-  wall-pos    ; (send w get-pos) 
          radius)))

    ;; Number^3 -> Number
    ;; WHERE: lo <= hi
    ;; RETURNS: val, but limited to the range [lo,hi]
    (define (limit-value lo val hi)
      (max lo (min val hi)))

    ;; -> Integer
    ;; RETURNS: the velocity of the ball at the next tick
    ;; STRATEGY: if the ball will be at its limit, negate the
    ;; velocity, otherwise return it unchanged
    ;; (define (next-speed)
    ;;   (if (or
    ;;         (= (next-x-pos) radius)
    ;;         (= (next-x-pos) (- wall-pos ; (send w get-pos) 
    ;;                           radius)))
    ;;     (- speed)
    ;;     speed))

    (define (next-speed)
      (if
        (< radius (next-x-pos) (- wall-pos radius))
        speed
        (- speed)))

    (define/public (add-to-scene s)
      (place-image
        (circle radius 
          "outline"
          "red")
        x y s))

    ; after-button-down : Integer Integer -> Void
    ; GIVEN: the location of a button-down event
    ; STRATEGY: Cases on whether the event is in this
    (define/public (after-button-down mx my)
      (if (in-this? mx my)
        ;; (new Ball%
        ;;   [x x][y y][speed speed]
        ;;   [selected? true]
        ;;   [saved-mx (- mx x)]
        ;;   [saved-my (- my y)]
        ;;   [w w])
        (begin
          (set! selected? true)
          (set! saved-mx (- mx x))
          (set! saved-my (- my y)))
        this))

    ;; in-this? : Integer Integer -> Boolean
    ;; GIVEN: a location on the canvas
    ;; RETURNS: true iff the location is inside this.
    (define (in-this? other-x other-y)
      (<= (+ (sqr (- x other-x)) (sqr (- y other-y)))
          (sqr radius)))

    ; after-button-up : Integer Integer -> Void
    ; GIVEN: the location of a button-up event
    ; STRATEGY: Cases on whether the event is in this
    ; If this is selected, then unselect it.
    (define/public (after-button-up mx my)
      (if (in-this? mx my)
        ;; (new Ball%
        ;;   [x x][y y][speed speed]
        ;;   [selected? false]
        ;;   [saved-mx 127]
        ;;   [saved-my 98]   ; the invariant says that if selected? is
        ;;                    ; false, you can put anything here.
        ;;   [w w])
        (set! selected? false)
        'error-276))

    ;; In Racket, an 'if' must have two arms.  #lang racket also has a
    ;; form called 'when', which only requires one arm.  You can use
    ;; that in your code if you want.

    ; after-drag : Integer Integer -> Void
    ; GIVEN: the location of a drag event
    ; STRATEGY: Cases on whether the ball is selected.
    ; If it is selected, move it so that the vector from the center to
    ; the drag event is equal to (mx, my)
    (define/public (after-drag mx my)
      (if selected?
        ;; (new Ball%
        ;;   [x (- mx saved-mx)]
        ;;   [y (- my saved-my)]
        ;;   [speed speed]
        ;;   [selected? true]
        ;;   [saved-mx saved-mx]
        ;;   [saved-my saved-my]
        ;;   [w w])
        (begin
          (set! x (- mx saved-mx))
          (set! y (- my saved-my)))
        this))   

    ;; KeyEvent -> Void
    (define/public (after-key-event kev)
      (if selected?
        (cond
          [(key=? kev "a") (attract-wall)]
          [(key=? kev "r") (repel-wall)])
        this))
      
    ;; -> Void
    (define (attract-wall)
      (send w move-to (- wall-pos (/ (- wall-pos x) 2))))

    ;; -> Void
    (define (repel-wall)
      (send w move-to (+ wall-pos (/ (- wall-pos x) 2))))


    (define/public (for-test:x)          x)
    (define/public (for-test:speed)      speed)
    (define/public (for-test:wall-pos)   wall-pos)
    (define/public (for-test:next-speed) (next-speed))
    (define/public (for-test:next-x)     (next-x-pos))
    

    ))

;; unit test for ball:

(begin-for-test
  (local
    ((define wall1 (new SWall% [pos 200]))
     (define ball1 (new SBall% [x 110][speed 50][w wall1])))

    (check-equal? (send ball1 for-test:speed) 50)
    (check-equal? (send ball1 for-test:wall-pos) 200)

    (check-equal? (send ball1 for-test:next-speed) 50)
    (check-equal? (send ball1 for-test:next-x) 160)

    (send ball1 after-tick)

    (check-equal? (send ball1 for-test:x) 160)
    (check-equal? (send ball1 for-test:speed) 50)

    (send ball1 after-tick)

    (check-equal? (send ball1 for-test:x) 180)
    (check-equal? (send ball1 for-test:speed) -50)

    ))

(begin-for-test
  (local
    ((define wall1 (new SWall% [pos 200]))
     (define ball1 (new SBall% [x 160][speed 50][w wall1])))

    (check-equal? (send ball1 for-test:x) 160)
    (check-equal? (send ball1 for-test:speed) 50)

    (check-equal? (send ball1 for-test:next-x) 180)
    (check-equal? (send ball1 for-test:next-speed) -50)

    (send ball1 after-tick)

    (check-equal? (send ball1 for-test:x) 180)
    (check-equal? (send ball1 for-test:speed) -50)

    ))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; The SWall% class

;; Constructor Template for SWall%
;; (new SWall% [pos Integer]
;;             [saved-mx Integer]
;;             [selected? Boolean])
;; all these fields have default values

(define SWall%
  (class* object% (SWall<%>)

    (init-field [pos INITIAL-WALL-POSITION]) ; the x position of the wall

    ; is the wall selected? Default is false.
    (init-field [selected? false]) 

    ;; if the wall is selected, the x position of
    ;; the last button-down event near the wall
    (init-field [saved-mx 0])
       
    ;; the list of registered balls
    ;; ListOfBall
    (field [balls empty])  

    (super-new)

    ;; the extra behavior for Wall<%>
    ;; (define/public (get-pos) pos)

    ;; SBall -> Int
    ;; EFFECT: registers the given ball
    ;; RETURNS: the current position of the wall
    (define/public (register b)
      (begin
        (set! balls (cons b balls))
        pos))

    ; after-button-down : Integer Integer -> Void
    ; GIVEN: the (x, y) location of a button-down event
    ; EFFECT: if the event is near the wall, make the wall selected.
    ; STRATEGY: Cases on whether the event is near the wall
    (define/public (after-button-down mx my)
      (if (near-wall? mx)
        ;; (new Wall%
        ;;   [pos pos]
        ;;   [selected? true]
        ;;   [saved-mx (- mx pos)])
        (begin
          (set! selected? true)
          (set! saved-mx (- mx pos)))
          ;; don't need to worry about returning this
        this))  ;; but an if needs an else clause :-(

    ; after-button-up : Integer Integer -> Void
    ; GIVEN: the (x,y) location of a button-up event
    ; EFFECT: makes the Wall unselected
    (define/public (after-button-up mx my)
      ;; (new Wall%
      ;;   [pos pos]
      ;;   [selected? false]
      ;;   [saved-mx saved-mx])
      (set! selected? false))
      

    ; after-drag : Integer Integer -> Void
    ; GIVEN: the location of a drag event
    ; STRATEGY: Cases on whether the wall is selected.
    ; If it is selected, move it so that the vector from its position to
    ; the drag event is equal to saved-mx.  Report the new position to
    ; the registered balls.
    (define/public (after-drag mx my)
      (if selected?
        ;; (new Wall%
        ;;   [pos (- mx saved-mx)]
        ;;   [selected? true]
        ;;   [saved-mx saved-mx])
        (begin
          (set! pos (- mx saved-mx))
          ;; NEW in push-model:
          (for-each
            (lambda (b) (send b update-wall-pos pos))
            balls))
        this))

    ; move-to : Integer -> Void
    ; EFFECT: moves the wall to the specified position, and report the
    ; new position to the registered balls
    (define/public (move-to n)
      (set! pos n)
      (for-each
        (lambda (b) (send b update-wall-pos pos))
        balls))


    ;; probably should combine these for-each's into a single private
    ;; function. 


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

