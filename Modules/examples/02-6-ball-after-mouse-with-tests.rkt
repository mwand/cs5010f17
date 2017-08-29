;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname 02-6-ball-after-mouse-with-tests) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; testing for itemization-then-compound

(require rackunit)
(require "extras.rkt")    ; for begin-for-test
(require 2htdp/universe)  ; for mouse=?, key=?

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; DATA DEFINITIONS

(define-struct ball (x y radius selected?))

;; A Ball is a (make-ball Number Number Number Boolean)
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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    

;; ball-after-mouse : Ball Number Number MouseEvent -> Ball
;; GIVEN: a ball, a location and a mouse event
;; RETURNS: the ball after the given mouse event at the given location
;; STRATEGY: Cases on MouseEvent
(define (ball-after-mouse b mx my mev)
  (cond
    [(mouse=? mev "button-down") (ball-after-button-down b mx my)]
    [(mouse=? mev "drag") (ball-after-drag b mx my)]
    [(mouse=? mev "button-up") (ball-after-button-up b mx my)]
    [else b]))

;; ball-after-drag : Ball Number Number -> Ball
;; GIVEN: a ball and a location
;; RETURNS: the ball after a drag event at the given location.
;; STRATEGY: Use template for Ball
(define (ball-after-drag b x y)
  (if (ball-selected? b)
    (ball-moved-to b x y)
    b))

;; ball-moved-to : Ball Number Number -> Ball
;; GIVEN: a ball and a set of coordinates
;; RETURNS: a ball like the given one, except that it has been moved to
;; the given coordinates
;; STRATEGY: Use template for Ball
(define (ball-moved-to b x y)
  (make-ball
    x
    y
    (ball-radius b)
    (ball-selected? b)))

;; ball-after-button-down : Ball Number Number -> Ball
;; GIVEN: a ball and a set of coordinates
;; RETURNS: if the given coordinates are inside the ball, returns a ball just
;; like the given one, except that selected? is true.
;; if they are not inside the ball, return the ball unchanged.
;; STRATEGY: Use template for Ball
(define (ball-after-button-down b mx my)
  (if (inside-ball? mx my b)
    (ball-make-selected b)
    b))

;; ball-make-selected : Ball -> Ball
;; RETURNS: a ball just like the given one, except that selected? is
;; true.
;; STRATEGY: Use template for Ball
(define (ball-make-selected b)
  (make-ball
    (ball-x b)
    (ball-y b)
    (ball-radius b)
    true))

;; inside-ball? : Number Number Ball -> Boolean
;; RETURNS: true if the given coordinates are inside the given ball.
;; STRATEGY: Use template for Ball
(define (inside-ball? mx my b)
  (<= 
    (+ 
      (sqr (- (ball-x b) mx))
      (sqr (- (ball-y b) my)))
    (sqr (ball-radius b))))

;; ball-after-button-up : Ball Number Number -> Ball
;; RETURNS: the ball that should follow the given one after a button up.
;; STRATEGY: function composition
(define (ball-after-button-up b mx my) 
  (ball-make-unselected b))


;; ball-make-unselected : Ball -> Ball
;; RETURNS: a ball just like the given one, except that selected? is
;; false. 
;; STRATEGY: Use template for Ball
(define (ball-make-unselected b)
  (make-ball
    (ball-x b)
    (ball-y b)
    (ball-radius b)
    false))  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TESTS

;; we have a total of twelve possibilities:

;; 1.  The ball is selected or unselected
;; 2.  The mouse event is inside or outside the current ball
;; 3.  The mouse event is button-up, button-down, or drag.

(define ball-unselected (make-ball 20 30 10 false))
(define ball-selected   (make-ball 20 30 10 true))


(define point-inside-x 22)
(define point-inside-y 28)

(define point-outside-x 31)  ;; 20+10 = 30, so 31 is outside
(define point-outside-y 19)  ;; 30-10 = 20, so 19 is outside

(define ball-moved-to-point-inside 
  (make-ball point-inside-x point-inside-y 10 true))

(define ball-moved-to-point-outside
  (make-ball point-outside-x point-outside-y 10 true))

(begin-for-test

  ;; ball-unselected
  ;; point-inside
  (check-equal?
    (ball-after-mouse ball-unselected point-inside-x point-inside-y
      "button-down")
    ball-selected)

  (check-equal?
    (ball-after-mouse ball-unselected point-inside-x point-inside-y
      "drag")
    ball-unselected)

  (check-equal?
    (ball-after-mouse ball-unselected point-inside-x point-inside-y
      "button-up")
    ball-unselected)

  ;; ball-unselected
  ;; point-outside
  (check-equal?
    (ball-after-mouse ball-unselected point-outside-x point-outside-y
      "button-down")
    ball-unselected)

  (check-equal?
    (ball-after-mouse ball-unselected point-outside-x point-outside-y
      "drag")
    ball-unselected)

  (check-equal?
    (ball-after-mouse ball-unselected point-outside-x point-outside-y
      "button-up")
    ball-unselected)

  ;; ball-selected
  ;; point-inside
  (check-equal?
    (ball-after-mouse ball-selected point-inside-x point-inside-y
      "button-down")
    ball-selected)

  (check-equal?
    (ball-after-mouse ball-selected point-inside-x point-inside-y
      "drag")
     ball-moved-to-point-inside)

  (check-equal?
    (ball-after-mouse ball-selected point-inside-x point-inside-y
      "button-up")
    ball-unselected)

  ;; ball-selected
  ;; point-outside
  (check-equal?
    (ball-after-mouse ball-selected point-outside-x point-outside-y
      "button-down")
    ball-selected)

  (check-equal?
    (ball-after-mouse ball-selected point-outside-x point-outside-y
      "drag")
    ball-moved-to-point-outside)

  (check-equal?
    (ball-after-mouse ball-selected point-outside-x point-outside-y
      "button-up")
    ball-unselected)

)

;; extra tests added during debugging:

(begin-for-test

  (check-equal?
    (ball-after-mouse 
      ball-unselected 
      point-inside-x point-inside-y
      "button-down")
    (ball-after-button-down
      ball-unselected 
      point-inside-x point-inside-y))

  (check-equal?
    (ball-after-button-down
      ball-unselected 
      point-inside-x point-inside-y)
    (ball-make-selected ball-unselected)))



