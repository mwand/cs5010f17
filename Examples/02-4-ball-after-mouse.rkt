;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname 02-4-ball-after-mouse) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; Use of a template on partition data, followed by using of template
;; on another value.  The data is handed off to one of several help
;; functions, depending on the value of the partition data.  

(require rackunit)
(require 2htdp/universe)
(require "extras.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; DATA DEFINITIONS

(define-struct ball (x y radius selected?))

;; A Ball is a (make-ball Integer Integer Real Boolean)
;; x and y are the coordinates of the center of the ball, in pixels,
;; relative to the origin of the scene.
;; radius is the radius of the ball, in pixels
;; selected? is true iff the ball has been selected for dragging.

;; TEMPLATE:
;; (define (ball-fn b)
;;   (...
;;     (ball-x b)
;;     (ball-y b)
;;     (ball-radius b)
;;     (ball-selected? b)))

(define unselected-ball-at-12-25 (make-ball 12 25 10 false))
(define unselected-ball-at-17-24 (make-ball 17 24 10 false))

(define selected-ball-at-12-25 (make-ball 12 25 10 true))
(define selected-ball-at-17-24 (make-ball 17 24 10 true))

;; MouseEvent is defined in the 2htdp/universe module. Every MouseEvent is a
;; string, but not every string is a legal mouse event.  The predicate for 
;; comparing mouse events is mouse=? .

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    

;; ball-after-mouse : Ball Integer Integer MouseEvent -> Ball
;; GIVEN: a ball, a location and a mouse event
;; RETURNS: the ball after the given mouse event at the given location
;; STRATEGY: Cases on MouseEvent
(define (ball-after-mouse b mx my mev)
  (cond [(mouse=? mev "button-down") (ball-after-button-down b mx my)]
        [(mouse=? mev "drag")        (ball-after-drag b mx my)]
        [(mouse=? mev "button-up")   (ball-after-button-up b mx my)]
        [else b]))

;; ball-after-drag : Ball Integer Integer -> Ball
;; GIVEN: a ball and a location
;; RETURNS: the ball after a drag event at the given location.
;; STRATEGY: Use template for Ball on b
(define (ball-after-drag b x y)
  (if (ball-selected? b)
      (ball-moved-to b x y)
      b))


;; This test exercises ball-after-mouse, ball-after-drag, and
;; ball-moved-to. 
(begin-for-test
  (check-equal?
    (ball-after-mouse selected-ball-at-12-25 17 24 "drag")
    selected-ball-at-17-24)
  (check-equal?
   (ball-after-mouse unselected-ball-at-12-25 17 24 "drag")
   unselected-ball-at-12-25))

;; ball-moved-to : Ball Integer Integer -> Ball
;; GIVEN: a ball and a set of coordinates
;; RETURNS: a ball like the given one, except that its center has been
;; moved to the given coordinates. 
;; STRATEGY: Use te
(define (ball-moved-to b x y)
  (make-ball
   x
   y
   (ball-radius b)
   (ball-selected? b)))

;; Wishlist:

;; ball-after-button-down : Ball Integer Integer -> Ball
;; GIVEN: a ball and a set of coordinates
;; RETURNS: if the given coordinates are inside the ball, returns a ball just
;; like the given one, except that selected? is true.
;; if they are not inside the ball, returns the ball unchanged.
;; STRATEGY: Use template for Ball on b

;; ball-after-button-up : Ball Integer Integer -> Ball
;; GIVEN: a ball and a set of coordinates
;; RETURNS: a ball just like the given one, except that selected? is
;; false. 
;; STRATEGY: Use template for Ball on b

(define (ball-after-button-down b x y) "stub")
(define (ball-after-button-up b x y) "stub")

