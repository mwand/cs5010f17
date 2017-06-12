;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname 01-11-falling-cat-stops-ugly) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;; falling cat.  
;; A cat falls from the top of the scene.
;; The user can pause/unpause the cat with the space bar.


(require rackunit)
(require rackunit/text-ui)
(require 2htdp/universe)
(require 2htdp/image)

;; start with (main 0)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; MAIN FUNCTION.

;; main : Number -> World
;; start the simulation with the cat in the given position and falling.
(define (main initial-pos)
  (big-bang (make-world initial-pos false)
            (on-tick world-after-tick 0.5)
            (on-draw world->scene)
            (on-key world-after-key-event)))

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

;; An FallingCatKeyEvent is a KeyEvent, which is one of
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

;;; END DATA DEFINITIONS

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; world-after-tick : World -> World
;; produce the world that should follow the given world after a tick

;; examples: 
;; cat falling:
;; (world-after-tick unpaused-world-at-20) = unpaused-world-at-28
;; cat paused:
;; (world-after-tick paused-world-at-20) = paused-world-at-20
;; cat near bottom:
;; (world-after-tick cat-near-bottom) = cat-at-bottom

;; strategy: structural decomposition [World]
(define (world-after-tick w)
  (world-after-tick-helper (world-pos w) (world-paused? w)))

;; tests: tests follow help function.

;; world-after-tick-helper : Number Boolean -> World
;; given a position and paused?, produce the next World
;; strategy: function composition
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
(define-test-suite world-after-tick-tests
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



(run-tests world-after-tick-tests)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; world->scene : World -> Scene
;; produce an Scene that portrays the given world.
;; example: (world->scene (make-world 20 ??)) = (place-image CAT-IMAGE CAT-X-COORD 20 EMPTY-CANVAS)
;; strategy: structural decomposition [World]
(define (world->scene w)
  (place-image CAT-IMAGE CAT-X-COORD
               (world-pos w)
               EMPTY-CANVAS))

;; tests

;; an image showing the cat at Y = 20
;; check this visually to make sure it's what you want
(define image-at-20 (place-image CAT-IMAGE CAT-X-COORD 20 EMPTY-CANVAS))

;; note: these only test whether world->scene calls place-image properly.
;; it doesn't check to see whether that's the right image!
;; these are not very good test strings!
(define-test-suite world->scene-tests
  (check-equal? 
    (world->scene unpaused-world-at-20)
    image-at-20
    "test of (world->scene unpaused-world-at-20)")

  (check-equal?
    (world->scene paused-world-at-20)
    image-at-20
    "test of (world->scene paused-world-at-20)"))

(run-tests world->scene-tests)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; world-after-key-event : World FallingCatKeyEvent -> World
;; produce the world that should follow the given world
;; after the given key event.
;; on space, toggle paused?-- ignore all others
;; examples: see tests below
;; strategy: structural decomposition [Enumeration on KeyEvent]
(define (world-after-key-event w ake)
  (cond
    [(key=? ake " ")
     (world-with-paused-toggled w)]
    [else w]))

;; world-with-paused-toggled : World -> World
;; produce a world just like the given one, but with paused? toggled
;; strategy: structural decomposition [w : World]
(define (world-with-paused-toggled w)
  (make-world
   (world-pos w)
   (not (world-paused? w))))

(define pause-key-event " ")
(define non-pause-key-event "q")   ; a sample

;; for world-after-key-event, we need 4 tests: a paused world, and an
;; unpaused world, and a pause-key-event and a non-pause key event/

(define-test-suite world-after-key-event-tests
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

(run-tests world-after-key-event-tests)


