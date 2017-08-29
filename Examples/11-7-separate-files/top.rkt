#lang racket

(require rackunit)
(require 2htdp/universe)
(require 2htdp/image)
(require "extras.rkt")

(require "interfaces.rkt")
(require "SWall.rkt")
(require "SBall.rkt")
(require "SWorld.rkt")
(require "WidgetFactory.rkt")


;; start with (run framerate).  Typically: (run 0.25)

;; 11-6-after-review was 838 lines.
;; When broken up into separate files, each file is <= 150 lines--
;; much more manageable!!

;; initial-world : -> World
;; RETURNS: a world with a wall, a ball, and a factory
(define (initial-world)
  (local
    ((define the-wall (make-wall))
     (define the-ball (make-ball the-wall))
     (define the-world
       (make-sworld
         empty ; (list the-ball)  -- the ball is now stateful
         (list the-wall)))
     (define the-factory (make-widget-factory the-wall the-world)))
    (begin
      ;; put the factory in the world
      (send the-world add-stateful-widget the-factory)
      ;; tell the factory to start a ball
      (send the-factory after-key-event "b")
      the-world)))

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
