;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname 03-3-two-draggable-cats) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; two draggable cats.
;; like draggable cat, but there are TWO cats.  They are individually
;; draggable.  But space pauses or unpauses the entire system.

;; draggable cat.
;; like falling cat, but user can drag the cat with the mouse.
;; button-down to select, drag to move, button-up to release.

;; falling cat.  
;; A cat falls from the top of the scene.
;; The user can pause/unpause the cat with the space bar.

;; start with (main 0)

(require rackunit)
(require "extras.rkt")
(require 2htdp/universe)
(require 2htdp/image)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; MAIN FUNCTION.

;; main : Integer -> World
;; GIVEN: the initial y-position of the cats
;; EFFECT: runs the simulation, starting with the cats falling
;; RETURNS: the final state of the world
(define (main initial-pos)
  (big-bang (initial-world initial-pos)
            (on-tick world-after-tick 0.5)
            (on-draw world-to-scene)
            (on-key world-after-key-event)
            (on-mouse world-after-mouse-event)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; CONSTANTS

(define CAT-IMAGE (bitmap "cat.png"))

;; how fast the cat falls, in pixels/tick
(define CATSPEED 8)

;; dimensions of the canvas
(define CANVAS-WIDTH 450)
(define CANVAS-HEIGHT 400)
(define EMPTY-CANVAS (empty-scene CANVAS-WIDTH CANVAS-HEIGHT))
(define CAT1-X-COORD (/ CANVAS-WIDTH 3))
(define CAT2-X-COORD (* 2 CAT1-X-COORD))

;; dimensions of the cat
(define HALF-CAT-WIDTH  (/ (image-width  CAT-IMAGE) 2))
(define HALF-CAT-HEIGHT (/ (image-height CAT-IMAGE) 2))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; DATA DEFINITIONS

(define-struct world (cat1 cat2 paused?))
;; A World is a (make-world Cat Cat Boolean)
;; cat1 and cat2 are the two cats
;; paused? describes whether or not the world is paused

;; template:
;; world-fn : World -> ??
;; (define (world-fn w)
;;   (... (world-cat1 w) (world-cat2 w) (world-paused? w)))


(define-struct cat (x-pos y-pos selected?))
;; A Cat is a (make-cat Integer Integer Boolean)
;; Interpretation: 
;; x-pos, y-pos give the position of the cat. 
;; selected? describes whether or not the cat is selected.

;; template:
;; cat-fn : Cat -> ??
;(define (cat-fn c)
; (... (cat-x-pos w) (cat-y-pos w) (cat-selected? w)))

;; examples of cats, for testing
(define selected-cat1-at-20 (make-cat CAT1-X-COORD 20 true))
(define unselected-cat1-at-20 (make-cat CAT1-X-COORD 20 false))

(define selected-cat1-at-28 (make-cat CAT1-X-COORD 28 true))
(define unselected-cat1-at-28 (make-cat CAT1-X-COORD 28 false))

(define selected-cat2-at-35 (make-cat CAT2-X-COORD 35 true))
(define unselected-cat2-at-35 (make-cat CAT2-X-COORD 35 false))

;; examples of worlds, for testing

(define paused-world-at-20
  (make-world
    unselected-cat1-at-20
    selected-cat2-at-35
    true))

(define unpaused-world-at-20
  (make-world
    unselected-cat1-at-20
    selected-cat2-at-35
    false))

;; in an unpaused world, the unselected cat falls, but the selected
;; cat stays pinned to the mouse.
(define unpaused-world-at-20-after-tick
  (make-world
    unselected-cat1-at-28
    selected-cat2-at-35
    false))
  
;; help function for key event
;; is-pause-key-event? : KeyEvent -> Boolean
;; GIVEN: a KeyEvent
;; RETURNS: true iff the KeyEvent represents a pause instruction
(define (is-pause-key-event? ke)
  (key=? ke " "))

;; examples KeyEvents for testing
(define pause-key-event " ")
(define non-pause-key-event "q")   


;; example MouseEvents for testing:
(define button-down-event "button-down")
(define drag-event "drag")
(define button-up-event "button-up")
(define other-event "enter")

;;; END DATA DEFINITIONS

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; world-after-tick : World -> World
;; GIVEN: a world w
;; RETURNS: the world that should follow w after a tick.
;; STRATEGY: Use template for World on w
(define (world-after-tick w)
  (if (world-paused? w)
    w
    (make-world
      (cat-after-tick (world-cat1 w))
      (cat-after-tick (world-cat2 w))
      (world-paused? w))))



;; cat-after-tick : Cat -> Cat
;; GIVEN: the state of a cat c
;; RETURNS: the state of the given cat after a tick if it were in an
;; unpaused world.

;; examples: 
;; cat selected
;; (cat-after-tick selected-cat1-at-20) = selected-cat1-at-20
;; cat paused:
;; (cat-after-tick unselected-cat1-at-20) = unselected-cat-at-28

;; STRATEGY: use template for Cat on c

(define (cat-after-tick c)
  (if (cat-selected? c)
    c
    (make-cat
      (cat-x-pos c)
      (+ (cat-y-pos c) CATSPEED)          
      (cat-selected? c))))

;; tests:
(begin-for-test
  ;; cat selected
  (check-equal?
    (cat-after-tick selected-cat1-at-20)
    selected-cat1-at-20
    "selected cat shouldn't move")

  ;; cat unselected
  (check-equal? 
    (cat-after-tick unselected-cat1-at-20)
    unselected-cat1-at-28
    "unselected cat should fall CATSPEED pixels and remain unselected")

  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; world-to-scene : World -> Scene
;; RETURNS: a Scene that portrays the given world.
;; EXAMPLE: (world-to-scene paused-world-at-20) should return a canvas with
;; two cats, one at (150,20) and one at (300,28)
;;          
;; STRATEGY: Use template for World on w
(define (world-to-scene w)
  (place-cat
    (world-cat1 w)
    (place-cat
      (world-cat2 w)
      EMPTY-CANVAS)))

(define image-of-paused-world-at-20
  (place-image CAT-IMAGE 150 20
    (place-image CAT-IMAGE 300 35
      EMPTY-CANVAS)))

(begin-for-test
  (check-equal?
    (world-to-scene paused-world-at-20)
    image-of-paused-world-at-20
    "(world-to-scene paused-world-at-20) returned incorrect image"))

;; place-cat : Cat Scene -> Scene
;; RETURNS: a scene like the given one, but with the given cat painted
;; on it.
(define (place-cat c s)
  (place-image
    CAT-IMAGE
    (cat-x-pos c) (cat-y-pos c)
    s))

;; tests

;;; check this visually to make sure it's what you want
(define image-at-20 (place-image CAT-IMAGE CAT1-X-COORD 20 EMPTY-CANVAS))

;;; note: these only test whether world-to-scene calls place-image properly.
;;; it doesn't check to see whether image-at-20 is the right image!
(begin-for-test
 (check-equal? 
   (place-cat selected-cat1-at-20 EMPTY-CANVAS)
   image-at-20
   "(place-cat selected-cat1-at-20 EMPTY-CANVAS) returned unexpected image or value")

 (check-equal?
   (place-cat unselected-cat1-at-20 EMPTY-CANVAS)   
   image-at-20
   "(place-cat unselected-ca1t-at-20 EMPTY-CANVAS) returned unexpected image or value"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; world-after-key-event : World KeyEvent -> World
;; GIVEN: a world w
;; RETURNS: the world that should follow the given world
;; after the given key event.
;; on space, toggle paused?-- ignore all others
;; EXAMPLES: see tests below
;; STRATEGY: Cases on whether the kev is a pause event
(define (world-after-key-event w kev)
  (if (is-pause-key-event? kev)
    (world-with-paused-toggled w)
    w))

;; world-with-paused-toggled : World -> World
;; RETURNS: a world just like the given one, but with paused? toggled
;; STRATEGY: use template for World on w
(define (world-with-paused-toggled w)
  (make-world
   (world-cat1 w)
   (world-cat2 w)
   (not (world-paused? w))))


;; for world-after-key-event, we need 4 tests: a paused world, and an
;; unpaused world, and a pause-key-event and a non-pause key event.

(begin-for-test
  (check-equal?
    (world-after-key-event paused-world-at-20 pause-key-event)
    unpaused-world-at-20
    "after pause key, a paused world did not become unpaused")

  (check-equal?
    (world-after-key-event unpaused-world-at-20 pause-key-event)
    paused-world-at-20
    "after pause key, an unpaused world did not become paused")

  (check-equal?
    (world-after-key-event paused-world-at-20 non-pause-key-event)
    paused-world-at-20
    "after a non-pause key, a paused world was not unchanged")

  (check-equal?
    (world-after-key-event unpaused-world-at-20 non-pause-key-event)
    unpaused-world-at-20
    "after a non-pause key, an unpaused world was not unchanged"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; world-after-mouse-event : World Integer Integer MouseEvent -> World
;; GIVEN: a world and a description of a mouse event
;; RETURNS: the world that should follow the given mouse event
;; STRATEGY: use template for World on w
(define (world-after-mouse-event w mx my mev)
  (make-world
    (cat-after-mouse-event (world-cat1 w) mx my mev)
    (cat-after-mouse-event (world-cat2 w) mx my mev)
    (world-paused? w)))



;; cat-after-mouse-event : Cat Integer Integer MouseEvent -> Cat
;; GIVEN: a cat and a description of a mouse event
;; RETURNS: the cat that should follow the given mouse event
;; examples:  See slide on life cycle of dragged cat
;; strategy: Cases on mouse event mev
(define (cat-after-mouse-event c mx my mev)
  (cond
    [(mouse=? mev "button-down") (cat-after-button-down c mx my)]
    [(mouse=? mev "drag") (cat-after-drag c mx my)]
    [(mouse=? mev "button-up") (cat-after-button-up c mx my)]
    [else c]))

;; how many tests do we need here?
;; 3 mouse events (+ a test for the else clause)
;; cats selected or unselected  (do we need to worry about being
;; paused?)
;; event inside cat or not.

(begin-for-test

  ;; button-down:

  ;; button-down inside cat1
  (check-equal?
    (world-after-mouse-event 
      (make-world
        unselected-cat1-at-20
        unselected-cat2-at-35
        false)
      (+ CAT1-X-COORD 5) 15    ;; a coordinate inside cat1
      "button-down")
    (make-world
      selected-cat1-at-20
      unselected-cat2-at-35
      false)
    "button down inside cat1 should select it but didn't")


  ;; button-down inside cat2
  (check-equal?
    (world-after-mouse-event 
      (make-world
        unselected-cat1-at-20
        unselected-cat2-at-35
        false)
      (+ CAT2-X-COORD 5) 15    ;; a coordinate inside cat2
      "button-down")
    (make-world
      unselected-cat1-at-20
      selected-cat2-at-35
      false)
    "button down inside cat2 should select it but didn't")

  ;; button-down not inside any cat
  (check-equal?
    (world-after-mouse-event 
      (make-world
        unselected-cat1-at-20
        unselected-cat2-at-35
        false)
      (+ CAT1-X-COORD 5) 115    ;; a coordinate not inside cat1 or cat2
      "button-down")
    (make-world
      unselected-cat1-at-20
      unselected-cat2-at-35
      false)
    "button down outside any cat should leave world unchanged, but didn't")

  ;; Question: is it possible to do a button-down on an
  ;; already-selected cat?  Is it possible to do a button-down on a
  ;; world in which ANY cat is selected?

  ;; tests for drag

  ;; don't care about paused, care only about which cat is selected. 

  ;; no cats selected: drag should not change anything
  (check-equal?
    (world-after-mouse-event
      (make-world
        unselected-cat1-at-20
        unselected-cat2-at-35
        false)
      (+ CAT1-X-COORD 100) 15    ;; a large motion
      "drag")
    (make-world
        unselected-cat1-at-20
        unselected-cat2-at-35
        false)
    "drag with no cat selected didn't leave world unchanged")
    
  ;; cat1 selected
  (check-equal?
    (world-after-mouse-event
      (make-world
        selected-cat1-at-20
        unselected-cat2-at-35
        false)
      (+ CAT1-X-COORD 100) 15    ;; a large motion
      "drag")
    (make-world
      (make-cat (+ CAT1-X-COORD 100) 15 true)
      unselected-cat2-at-35
      false)
    "drag when cat1 is selected should just move cat1, but didn't")

  ;; cat2 selected
  (check-equal?
    (world-after-mouse-event
      (make-world
        unselected-cat2-at-35
        selected-cat1-at-20
        false)
      (+ CAT1-X-COORD 100) 15    ;; a large motion
      "drag")
    (make-world
      unselected-cat2-at-35
      (make-cat (+ CAT1-X-COORD 100) 15 true)
      false)
    "drag when cat2 is selected should just move cat2, but didn't")

  ;; Question: is it possible to have both cat1 and cat2 selected?  If
  ;; so, what happens when they are both selected?

  ;; tests for button-up

  ;; button-up always unselects both cats

  ;; unselect cat1
  (check-equal?
    (world-after-mouse-event
      (make-world
        selected-cat2-at-35
        unselected-cat1-at-20
        true)
      (+ CAT1-X-COORD 100) 15    ;; arbitrary location
      "button-up")
    (make-world
        unselected-cat2-at-35
        unselected-cat1-at-20
        true)
    "button-up failed to unselect cat1")



  ;; unselect cat2
  (check-equal?
    (world-after-mouse-event
      (make-world
        unselected-cat1-at-20
        selected-cat2-at-35
        true)
      (+ CAT1-X-COORD 100) 15    ;; arbitrary location
      "button-up")
    (make-world
        unselected-cat1-at-20
        unselected-cat2-at-35
        true)
    "button-up failed to unselect cat2")
  
  ;; unselect cat2
  (check-equal?
    (world-after-mouse-event
      (make-world
        unselected-cat1-at-20
        unselected-cat2-at-35
        true)
      (+ CAT1-X-COORD 100) 15    ;; arbitrary location
      "button-up")
    (make-world
        unselected-cat1-at-20
        unselected-cat2-at-35
        true)
    "button-up with two unselected cats failed.")



  

  ;; tests for other mouse events

  (check-equal?
    (world-after-mouse-event unpaused-world-at-20 
      (+ CAT1-X-COORD 100) 15    ;; arbitrary coordinate
      "move")
    unpaused-world-at-20
    "other mouse events should leave the world unchanged, but didn't")

  )

;; helper functions:

;; cat-after-button-down : Cat Integer Integer -> Cat
;; RETURNS: the cat following a button-down at the given location.
;; STRATEGY: Use template for Cat on c
(define (cat-after-button-down c x y)
  (if (in-cat? c x y)
      (make-cat (cat-x-pos c) (cat-y-pos c) true)
      c))

;; cat-after-drag : Cat Integer Integer -> Cat
;; RETURNS: the cat following a drag at the given location
;; STRATEGY: Use template for Cat on c
(define (cat-after-drag c x y)
  (if (cat-selected? c)
      (make-cat x y true)
      c))

;; cat-after-button-up : Cat Integer Integer -> Cat
;; RETURNS: the cat following a button-up at the given location
;; STRATEGY: Use template for Cat on c
(define (cat-after-button-up c x y)
  (if (cat-selected? c)
      (make-cat (cat-x-pos c) (cat-y-pos c) false)
      c))
  

;; in-cat? : Cat Integer Integer -> Cat
;; RETURNS true iff the given coordinate is inside the bounding box of
;; the given cat.
;; EXAMPLES: see tests below
;; STRATEGY: Use template for Cat on c
(define (in-cat? c x y)
  (and
    (<= 
      (- (cat-x-pos c) HALF-CAT-WIDTH)
      x
      (+ (cat-x-pos c) HALF-CAT-WIDTH))
    (<= 
      (- (cat-y-pos c) HALF-CAT-HEIGHT)
      y
      (+ (cat-y-pos c) HALF-CAT-HEIGHT))))

(begin-for-test
  
  ;; inside the cat
  (check-equal?
    (in-cat? unselected-cat1-at-20 (+ CAT1-X-COORD 5) 15)
    true
    "test of in-cat? with nearby point")

  (check-equal?
    (in-cat? unselected-cat1-at-20 
      (+ CAT1-X-COORD 100) 15)    ;; a coordinate not inside the cat
    false
    "test of in-cat? with distant point")

  )

;; discussion question: are these tests sufficient to test in-cat?

;; initial-world : Integer -> World
;; RETURNS: a world with two unselected cats at the given y coordinate
(define (initial-world y)
  (make-world
    (make-cat CAT1-X-COORD y false)
    (make-cat CAT2-X-COORD y false)
    false))



      

