;; using invariants to keep assumptions straight

;; Consider a ball bouncing in closed box in the x-direction
;; The ball is a point mass.
;; If, on the next tick, the ball would hit the wall, then on the next
;; tick the ball starts out _at_ the wall, with its velocity reversed.

; the location of the left-hand end of the box
(define XMIN 0)         

; the location of the right-hand end of the box
(define XMAX 200)       

; the initial ball speed in pixels/tick
(define BALLSPEED 8)

;; a ball is represented as a Ball, which is a struct
;; (make-ball x-pos x-vel)
;; with fields
;; x-pos : Int[0,200]    represents the x position of the ball
;; x-vel : Int           represents the x-velocity of the ball, in pixels/tick.

(define-struct ball (x-pos x-vel))

;; constructor template
;; (make-ball Int[0,200] Int)

;; ball-after-tick : Ball -> Ball
;; GIVEN: the state of a ball
;; RETURNS: the state of the ball after the next tick
(define (ball-after-tick b)
  (if (ball-would-hit-wall b)
      (ball-after-bounce b)
      (ball-normal-motion b)))

;; WISHLIST

;; ball-would-hit-wall : Ball -> Boolean
;; GIVEN: the state of a Ball
;; RETURNS: true iff the ball, in its normal motion, would hit the
;; wall on the next tick

;; ball-after-bounce : Ball -> Ball
;; GIVEN: the state of a Ball
;; WHERE: the ball, in its normal motion, would hit the wall on the
;; next tick
;; RETURNS: the state of the ball after the next tick

;; ball-normal-motion : Ball -> Ball
;; GIVEN: the state of a Ball
;; WHERE: the ball, in its normal motion, would not hit the wall on the
;; next tick
;; RETURNS: the state of the ball after the next tick

