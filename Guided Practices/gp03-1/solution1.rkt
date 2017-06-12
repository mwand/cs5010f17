;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname solution1) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;; falling cat.  
;; A cat falls from the top of the scene.
;; The user can pause/unpause the cat with the space bar.

;; start with (main 0)

(require rackunit)
(require "extras.rkt")

(require 2htdp/universe)
(require 2htdp/image)

;; start with (main 0)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; MAIN FUNCTION.

;; main : Number -> World
;; GIVEN: the initial y-position in the cat
;; EFFECT: runs the simulation, starting with the cat falling
;; RETURNS: the final state of the world
(define (main initial-pos)
  (big-bang (make-world initial-pos false)
            (on-tick world-after-tick 0.5)
            (on-key world-after-key-event)
            (on-draw world-to-scene)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; CONSTANTS

(define CAT-IMAGE (bitmap "cat.png"))

;; how fast the cat falls, in pixels/tick
(define CATSPEED 8)

;; dimensions of the canvas
(define CANVAS-WIDTH 200)
(define CANVAS-HEIGHT 400)
(define EMPTY-CANVAS (empty-scene CANVAS-WIDTH CANVAS-HEIGHT))
(define CAT-X-COORD (/ CANVAS-WIDTH 2))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; DATA DEFINITIONS

(define-struct world (pos paused?))
;; A World is a (make-world Number Boolean)
;; Interpretation: 
;; pos describes how far the cat has fallen, in pixels. 
;; paused? describes whether or not the cat is paused.

;; template:
;; world-fn : World -> ??
;(define (world-fn w)
;  (... (world-pos w) (world-paused? w)))

;;examples of worlds, for testing
(define unpaused-world-at-20 (make-world 20 false))  
(define paused-world-at-20   (make-world 20 true))
(define unpaused-world-at-28 (make-world 28 false))  
(define paused-world-at-28   (make-world 28 true))

(define cat-at-bottom
  (make-world 
   (- CANVAS-HEIGHT (/ (image-height CAT-IMAGE) 2)) 
   false))

;; half CATSPEED above the bottom
(define cat-near-bottom  
  (make-world
    (- CANVAS-HEIGHT
      (/ (image-height CAT-IMAGE) 2)
      (/ CATSPEED 2))
    false))

;; We put in another data definition to indicate how KeyEvents should
;; be interpreted: 

;; A FallingCatKeyEvent is a KeyEvent, which is one of
;; -- " "                (interp: pause/unpause)
;; -- any other KeyEvent (interp: ignore)

;; template:
;; falling-cat-kev-fn : FallingCatKeyEvent -> ??
;(define (falling-cat-kev-fn kev)
;  (cond 
;    [(key=? kev " ") 
;     ...]
;    [else 
;     ...]))

;; examples for testing
(define pause-key-event " ")
(define non-pause-key-event "q")   


;;; END DATA DEFINITIONS

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; world-after-tick : World -> World
;; GIVEN: a world w
;; RETURNS: the world that should follow w after a tick.

;; EXAMPLES: 
;; cat falling:
;; (world-after-tick unpaused-world-at-20) = unpaused-world-at-28
;; cat paused:
;; (world-after-tick paused-world-at-20) = paused-world-at-20
;; cat near bottom:
;; (world-after-tick cat-near-bottom) = cat-at-bottom

;; STRATEGY: structural decomposition on w : World
(define (world-after-tick w)
  (world-after-tick-helper (world-pos w) (world-paused? w)))

;; tests: tests follow help function.

;; world-after-tick-helper : Number Boolean -> World
;; GIVEN the position of the cat and paused?
;; RETURNS: the next World
;; STRATEGY: function composition
(define (world-after-tick-helper pos paused?)
  (if paused? 
      (make-world pos paused?)
      (if (would-cat-go-past-bottom? pos)
        cat-at-bottom
        (make-world (+ pos CATSPEED) paused?))))

;; would-cat-go-past-bottom? : Number -> Boolean
;; would a cat at the given go past the bottom if it continued at CATSPEED?
(define (would-cat-go-past-bottom? pos)
  (>
   (+ pos CATSPEED (/ (image-height CAT-IMAGE) 2))
   CANVAS-HEIGHT))

;; tests:
(begin-for-test
  (check-equal? 
    (world-after-tick unpaused-world-at-20) 
    unpaused-world-at-28
    "in unpaused world, the cat should fall CATSPEED pixels and world should still be unpaused")

  (check-equal? 
    (world-after-tick paused-world-at-20)
    paused-world-at-20
    "in paused world, cat should be unmoved")

  (check-equal?
    (world-after-tick cat-near-bottom)
    cat-at-bottom
    "cat near bottom should stop at bottom")

  (check-equal?
    (world-after-tick cat-at-bottom)
    cat-at-bottom
    "cat at bottom should stay there")

  )




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; world-to-scene : World -> Scene
;; RETURNS: a Scene that portrays the given world.
;; EXAMPLE: (world-to-scene (make-world 20 ??))
;;          = (place-image CAT-IMAGE CAT-X-COORD 20 EMPTY-CANVAS)
;; STRATEGY: structural decomposition on w : World
(define (world-to-scene w)
  (place-image CAT-IMAGE CAT-X-COORD
               (world-pos w)
               EMPTY-CANVAS))

;; tests

;; an image showing the cat at Y = 20
;; check this visually to make sure it's what you want
(define image-at-20 (place-image CAT-IMAGE CAT-X-COORD 20 EMPTY-CANVAS))

;; note: these only test whether world-to-scene calls place-image properly.
;; it doesn't check to see whether that's the right image!
;; these are not very good test strings!
(begin-for-test
  (check-equal? 
    (world-to-scene unpaused-world-at-20)
    image-at-20
    "test of (world-to-scene unpaused-world-at-20)")

  (check-equal?
    (world-to-scene paused-world-at-20)
    image-at-20
    "test of (world-to-scene paused-world-at-20)"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; world-after-key-event : World FallingCatKeyEvent -> World
;; GIVEN: a world w
;; RETURNS: the world that should follow the given world
;; after the given key event.
;; on space, toggle paused?-- ignore all others
;; EXAMPLES: see tests below
;; STRATEGY: structural decomposition on kev : FallingCatKeyEvent
(define (world-after-key-event w kev)
  (cond
    [(key=? kev " ")
     (world-with-paused-toggled w)]
    [else w]))

;; world-with-paused-toggled : World -> World
;; RETURNS: a world just like the given one, but with paused? toggled
;; STRATEGY: structural decomposition on w : World
(define (world-with-paused-toggled w)
  (make-world
   (world-pos w)
   (not (world-paused? w))))

;; for world-after-key-event, we need 4 tests: a paused world, and an
;; unpaused world, and a pause-key-event and a non-pause key event.

(begin-for-test
  (check-equal?
    (world-after-key-event paused-world-at-20 pause-key-event)
    unpaused-world-at-20
    "after pause key, a paused world should become unpaused")

  (check-equal?
    (world-after-key-event unpaused-world-at-20 pause-key-event)
    paused-world-at-20
    "after pause key, an unpaused world should become paused")

  (check-equal?
    (world-after-key-event paused-world-at-20 non-pause-key-event)
    paused-world-at-20
    "after a non-pause key, a paused world should be unchanged")

  (check-equal?
    (world-after-key-event unpaused-world-at-20 non-pause-key-event)
    unpaused-world-at-20
    "after a non-pause key, an unpaused world should be unchanged"))


