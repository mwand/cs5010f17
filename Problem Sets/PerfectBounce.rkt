#lang racket

(require rackunit)
(require "extras.rkt")

(define version "Sat Nov 19 15:07:54 2016")

(printf "PerfectBounce.rkt ~a~n" version)

(provide
  (struct-out particle)
  (struct-out rect))

(provide
 (contract-out
  [particle-after-tick
   (->i ([p particle?][r rect?])
        #:pre (p r) (particle-inside? p r)
        [result particle?])]))


         

;; computing the geometry of a perfect bounce

;; a particle is bouncing inside a two-dimensional rectangle,
;; travelling at a constant velocity.

;; given its state at time t, compute its state at time t+1.

;; rules: if the particle would cross any side of the rectangle, at
;; time t+1 it appears on the side of the rectangle at the place it
;; would have hit the rectangle, and its velocity in the direction it
;; hits the wall is reversed.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; data:

(define-struct particle (x y vx vy) #:transparent)  
(define-struct rect (xmin xmax ymin ymax) #:transparent)
;; all fields are Real.
;; We assume xmin < xmax, ymin < ymax.
;; Furthermore, we assume that the particle is either inside the
;; rectangle or is at the wall and travelling inward.
;; The specifications for the drag operation are supposed to ensure
;; this.

(define (particle-inside? p r)
  (let ((px (particle-x p))
        (py (particle-y p))
        (vx (particle-vx p))
        (vy (particle-vy p))
        (xmin (rect-xmin r))
        (xmax (rect-xmax r))
        (ymin (rect-ymin r))
        (ymax (rect-ymax r)))
    (and
     (or (< xmin px xmax)
         (and (= px xmin) (>= vx 0))
         (and (= px xmax) (<= vx 0)))
     (or (< ymin py ymax)
         (and (= py ymin) (>= vy 0))
         (and (= py ymax) (<= vy 0))))))
  


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; EXAMPLE:

;; The particle is at (10 20 30 -40).  There is a wall at y = 0.
;; If there were no wall, the particle's next state would be
;; (50 -20 30 -30).  But there is a wall.  The particle reaches y=0
;; at t = 20/40 = 0.5.  So it hits the wall at x = 10 + 0.5*30 = 25,
;; and its y-velocity is reversed.  So the next state is (25 0 30 40),
;; with impact at t=0.5

;; If there were a wall at x=20, it would hit that wall first.
;; Similar arithmetic would apply, and the x-velocity would be
;; reversed. 

;; If there were a wall at x=25, the particle would hit exactly in the
;; corner, at (25,0), and both the x- and y-velocities would be
;; reversed.

;; How to manage multiple walls?  If the x-velocity is > 0, then the
;; ball is guaranteed to hit the xmax wall if it travels long enough.
;; If the x-velocity is < 0, the particle is guaranteed to hit the
;; xmin wall if it travels long enough (and it will never hit the xmax
;; wall). 

;; So we can figure out when the ball will hit an x-wall, and when it
;; will hit a y-wall.

;; Choose the wall it hits first.  If there's a tie, that means the
;; particle will hit in a corner, so it doesn't matter which one we
;; choose. 

;; Find the time of the first collision (or 1 if none), and return the
;; state of the ball at time t.  If the ball is at a wall, reverse the
;; velocity in the direction of the wall.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; FUNCTION DEFINITIONS:

;; particle-after-tick : Particle Rectangle -> Particle
;; GIVEN: a particle inside a rectangle
;; RETURNS: the state of the particle after the next tick.
;; STRATEGY: cases on whether the particle is at rest.  If it is not
;; at rest, it is guaranteed to collide with a wall eventually.
;; Calculate the time t of the next collision (or 1 if the particle
;; doesn't hit a wall within the next tick), and return its state at
;; time t.

(define (particle-after-tick p r)
  (if (at-rest? p) p
    (let ((t (min 1 (time-of-next-collision p r))))
      (particle-after-time p r t))))

;; time-of-next-collision : Particle Rectangle -> Time
;; WHERE: the particle is in the rectangle and has some non-zero velocity
;; RETURNS: the time at which the particle will first hit any wall.

(define (time-of-next-collision p r)
  (let ((px (particle-x p))
        (py (particle-y p))
        (vx (particle-vx p))
        (vy (particle-vy p))
        (xmin (rect-xmin r))
        (xmax (rect-xmax r))
        (ymin (rect-ymin r))
        (ymax (rect-ymax r)))
    ;; take the minimum of the positive times
    (list12-min
      (filter 
        (lambda (t) (> t 0))
        (list
          ;; find next collision with an x boundary. This is where we
          ;; use the assumption that xmin <= x <= xmax.  Checking the
          ;; sign of vx tells us which wall the particle is travelling
          ;; towards. 
          (cond
            [(> vx 0) (/ (- xmax px) vx)]
            [(< vx 0) (/ (- xmin px) vx)]
            ;; if vx=0 you can't have an x-collision, return a 0 which
            ;; will be filtered out
            [else 0])    
          ;; similarly find next collision with a y boundary.
          (cond
            [(> vy 0) (/ (- ymax py) vy)]
            [(< vy 0) (/ (- ymin py) vy)]
            ;; if vy=0 you can't have an y-collision, return a 0 which
            ;; will be filtered out
            [else 0]))))))

(define (at-rest? p)
  (and (zero? (particle-vx p)) (zero? (particle-vy p))))

;; GIVEN: A list of either 1 or 2 PosReals, return the smallest
(define (list12-min lst)         
  (if (null? (rest lst))
    (first lst)
    (min (first lst) (second lst))))


;; particle-after-time : Particle Rectangle Time -> Particle
;; WHERE: t is either 1 of the time of next collision, whichever is
;; smaller. 
;; RETURNS: the state of the particle after time t.
;; If the particle is at a wall, it reverses its velocity in the
;; dimension of the wall.  (Or both, if it's at a corner).

(define (particle-after-time p r t)
  (let ((px (particle-x p))
        (py (particle-y p))
        (vx (particle-vx p))
        (vy (particle-vy p))
        (xmin (rect-xmin r))
        (xmax (rect-xmax r))
        (ymin (rect-ymin r))
        (ymax (rect-ymax r)))
    (let ((next-x (+ px (* vx t)))
          (next-y (+ py (* vy t))))
      (make-particle
        next-x
        next-y
        (maybe-reverse-velocity next-x vx xmin xmax)
        (maybe-reverse-velocity next-y vy ymin ymax)))))

(define (maybe-reverse-velocity x vx xmin xmax)
  (if (or
        (= x xmin)
        (= x xmax))
    (- vx)
    vx))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; TESTS

(define particle1 (make-particle 10 20 30 -40))

(define rect1 (make-rect 0 100 0 150))
(define rect2 (make-rect 0  20 0 150))
(define rect3 (make-rect 0  25 0 150))

#;(begin-for-test
    ;; particle bounces off y = 0
    (check-equal?
     (particle-after-tick particle1 rect1)
     (make-particle 25 0 30 40))
    
    ;; particle bounces off x = 20
    (check-equal?
     (particle-after-tick particle1 rect2)
     (make-particle 20 (- 20 40/3) -30 -40))
    
    ;; the particle bounces off y = 0 at the corner x = 25
    (check-equal?
     (particle-after-tick particle1 rect3)
     (make-particle 25 0 -30 40))
    )
 



