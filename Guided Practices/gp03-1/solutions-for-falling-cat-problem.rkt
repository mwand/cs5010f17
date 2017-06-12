;; here are two solutions, one ok and one better.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The first thing we need are tests for this new behavior.  Following
;; our general pattern, we create example worlds for the tests and give
;; them mnemonic names.

;; Then we add the tests.

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

;; Then we add tests to world-after-tick-tests :

(define-test-suite world-after-tick-tests

  ;; ...keep old tests...

  (check-equal?
    (world-after-tick cat-near-bottom)
    cat-at-bottom
    "cat near bottom should stop at bottom")

  (check-equal?
    (world-after-tick cat-at-bottom)
    cat-at-bottom
    "cat at bottom should stay there")

  )

;; Now we change world-after-tick-helper.  We include the definition
;; of world-after-tick here for completeness.

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

;; Note that we've encapsulated the complicated arithmetic into a help
;; function, so we know what the arithmetic is intended to mean.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; A better solution:

;;; We observe that there is a fixed upper limit on the y-position of
;;; the cat, so we define it as a constant, and then in
;;; world-after-tick helper we use the function min to enforce this limit.

(define CAT-Y-LIMIT (- CANVAS-HEIGHT (/ (image-height CAT-IMAGE 2))))

;; world-after-tick-helper : Number Boolean -> World
;; given a position and paused?, produce the next World
;; strategy: function composition
(define (world-after-tick-helper pos paused?)
  (if paused? 
      (make-world pos paused?)
      (make-world 
        (min (+ pos CATSPEED) 
             CAT-Y-LIMIT)
        paused?)))

;; of course we use the new tests in this solution also.

;; This solution is cleaner for the falling cat, but the general idea
;; of would-cat-go-past-bottom? is useful in many circumstances:
;; balls bouncing off walls, etc.  We call the
;; would-cat-go-past-bottom? test a GUARD-- it guards against the cat
;; going past the bottom.  In the old days, we would have tests like
;; end-of-file? which answered the question: if you tried to do a read
;; now, would you go past the end of the file?





