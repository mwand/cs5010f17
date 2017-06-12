#lang racket

(require rackunit)
(require "extras.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Balls Communicating with an external agent
;;; this is considered poor OO design
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; A Ball0 is an object of any class that implements Ball0<%>

(define Ball0<%>
  (interface ()
    ;; -> Integer
    ;; return x, y coords of center and radius, all in pixels
    get-x
    get-y
    get-r))

;; Constructor Template for Ball1%:
;;   (new Ball1% [x Integer][y Integer][r Integer])

(define Ball0%
  (class* object% (Ball0<%>)
    (init-field x y r)  ; interpretation omitted...
    (super-new)
    (define/public (get-x) x)
    (define/public (get-y) y)
    (define/public (get-r) r)))

;; Ball0 Ball0 -> Boolean
;; Do these two balls intersect?
;; This is an ordinary function, outside of any class.
(define (intersects? b1 b2)
  (circles-intersect?
    (send b1 get-x) (send b1 get-y) (send b1 get-r)
    (send b2 get-x) (send b2 get-y) (send b2 get-r)))

;; Would balls with these coordinates intersect?
(define (circles-intersect? x1 y1 r1 x2 y2 r2)
  (<=
    (+ (sqr (- x1 x2)) (sqr (- y1 y2)))
    (sqr (+ r1 r2))))

(begin-for-test

  (check-true
    (let ((ball1 (new Ball0% [x 0][y 0][r 10]))
          (ball2 (new Ball0% [x 0][y 10][r 10])))
      (and
        (intersects? ball1 ball2)
        (intersects? ball2 ball1))))

  (check-false
    (let ((ball1 (new Ball0% [x 0][y 0][r 10]))
          (ball2 (new Ball0% [x 20][y 10][r 10])))
      (or
        (intersects? ball1 ball2)
        (intersects? ball2 ball1)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; BALLS COMMUNICATING BY PULL
;; computation gets done in this ball
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; A Ball1 is an object of any class that implements Ball1<%>

(define Ball1<%>
  (interface ()
    ;; -> Integer
    ;; return x, y coords of center and radius, all in pixels
    get-x
    get-y
    get-r

    ;; Ball1 -> Boolean
    ;; Does this ball intersect the other one?
    intersects?
    ))

;; Constructor Template for Ball1%:
;;   (new Ball1% [x Integer][y Integer][r Integer])

(define Ball1%
  (class* object% (Ball-Pull<%>)
    (init-field x y r)  ; interpretation omitted...
    (super-new)

    ;; Does the other ball intersect this one?
    ;; STRATEGY: Ask the other ball for its data
    (define/public (intersects? other-b)
      (coordinates-intersect?
        (send other-b get-x)
        (send other-b get-y)
        (send other-b get-r)))

    ;; Integer^3 -> Boolean
    ;; GIVEN: the coordinates of some ball
    ;; RETURNS: would that ball intersect this one?
    ;; This is a private method (an ordinary function, but inside the
    ;; class).  Note that it refers to the fields of this object.
    (define (coordinates-intersect? other-x other-y other-r)
      (<= (+ (sqr (- x other-x)) (sqr (- y other-y)))
        (sqr (+ r other-r))))

    (define/public (get-x) x)
    (define/public (get-y) y)
    (define/public (get-r) r)

))

(begin-for-test

  (check-true
    (let ((ball1 (new Ball1% [x 0][y 0][r 10]))
          (ball2 (new Ball1% [x 0][y 10][r 10])))
      (and
        (send ball1 intersects? ball2)
        (send ball2 intersects? ball1))))

  (check-false
    (let ((ball1 (new Ball1% [x 0][y 0][r 10]))
          (ball2 (new Ball1% [x 20][y 10][r 10])))
      (or
        (send ball1 intersects? ball2)
        (send ball2 intersects? ball1)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; BALLS COMMUNICATING BY PUSH
;;; computation gets done in other-ball
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define Ball2<%>
  (interface ()

    ;; not needed for this example!
    ;; ;; -> Integer
    ;; ;; return x, y coords of center and radius, all in pixels
    ;; get-x
    ;; get-y
    ;; get-r

    ;; Ball2 -> Boolean
    ;; does this ball intersect the other one?
    intersects?

    ;; Integer^3 -> Boolean
    ;; Would a ball with the given x,y,r intersect this one?
    intersect-responder

    ))

;; Constructor Template for Ball2%:
;;   (new Ball2% [x Integer][y Integer][r Integer])

(define Ball2% 
  (class* object% (Ball-Push<%>)
    (init-field x y r) ; interpretation omitted...
    (super-new)

    ;; Ball2 -> Boolean
    (define/public (intersects? other-b)
      (send other-b intersect-responder x y r))

    ;; Integer^3 -> Boolean
    (define/public (intersect-responder other-x other-y other-r)
      (<= (+ (sqr (- x other-x)) (sqr (- y other-y)))
        (sqr (+ r other-r))))
    ))


(begin-for-test

  (check-true
    (let ((ball1 (new Ball2% [x 0][y 0][r 10]))
          (ball2 (new Ball2% [x 0][y 10][r 10])))
      (and
        (send ball1 intersects? ball2)
        (send ball2 intersects? ball1))))

  (check-false
    (let ((ball1 (new Ball2% [x 0][y 0][r 10]))
          (ball2 (new Ball2% [x 20][y 10][r 10])))
      (or
        (send ball1 intersects? ball2)
        (send ball2 intersects? ball1)))))



