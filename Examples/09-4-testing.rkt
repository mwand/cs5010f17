#lang racket

(require "extras.rkt")
(require rackunit)

;; A StupidRobot is an object of any class that implements
;; StupidRobot<%>

;; Interpretation: A StupidRobot represents a robot moving along a
;; one-dimensional line, starting at position 0.

(define StupidRobot<%>
  (interface ()

    ;; a new StupidRobot<%> is required to start at position 0
    
    ;; -> StupidRobot
    ;; RETURNS: a Robot just like this one, except moved one 
    ;; position to the right
    move-right

    ;; -> Integer
    ;; RETURNS: the current x-position of this robot
    get-pos

    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Client Code of Interface

;; move-right-by-distance : Robot<%> Nat -> Robot<%>
(define (move-right-by-distance r n)
  (cond
    [(zero? n) r]
    [else (move-right-by-distance
            (send r move-right)
            (- n 1))])) 

;;; tests

(begin-for-test
  (local
    ((define r0 (new-robot))
     ;; move r0 right twice
     (define r1 (send (send r0 move-right) move-right)))
    ;; get-pos should then return 2
    (check-equal?
      (send r1 get-pos)
      2)))

(begin-for-test
  (local
    ((define r0 (new-robot))
     (define r1 (move-right-by-distance r0 3)))
    (check-equal?
      (send r1 get-pos)
      3)))

;; let's create a bunch of robots and see if our tests pass for all of them
(begin-for-test
  (let ((the-robots (map (lambda (n) (new-robot)) '(1 2 3 4 5 6 7 8 9 10))))
    (map
     (lambda (r) (let ((r1 (move-right-by-distance r 13)))
                   (check-equal?
                    (send r1 get-pos)
                    13)))
     the-robots)))


(begin-for-test
  (let ((r1 (new Robot1%))
        (r2 (new Robot1%)))
    (check-equal? r1 r2 "This is an expected failure")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Implementations

;; Constructor Template for Robot1%:
;; (new Robot1% [x NonNegInt])
;; x is optional; default is 0

(define Robot1%
  (class* object% (StupidRobot<%>)

    (init-field [x 0])  
    ;; interp: the position of the robot, initially 0

    (super-new)

    (define/public (move-right)
      (new Robot1% [x (+ x 1)]))

    (define/public (get-pos)
      x)

    ))

;; Constructor Template for Robot2%:
;; (new Robot2% [blerch NonNegInt])
;; blerch is optional; default is 0

(define Robot2%
  (class* object% (StupidRobot<%>)

    (init-field [blerch 0])  
    ;; interp: the position of the robot.

    (super-new)

    (define/public (move-right)
      (new Robot2% [blerch (+ blerch 1)]))

    (define/public (get-pos)
      blerch)

    ))

;; Constructor Template for Robot3%:
;; (new Robot3% [y NonNegInt])
;; y is the negative of the position of the robot
;; y is optional; default is 0

(define Robot3%
  (class* object% (StupidRobot<%>)

    (init-field [y 0])   
    ;; interp: the negative of the position of the robot.

    (super-new)

    (define/public (move-right)
      (new Robot3% [y (- y 1)]))

    ;; RETURNS: the x-position of the robot
    (define/public (get-pos)
      (- y))

    ))

;; Constructor Template for Robot4%:
;; (new Robot4% [y ListOfRacketValue])
;; x is any Racket list whose length is equal to the position of the
;; robot. 
;; x is optional; default is 0

(define Robot4%
  (class* object% (StupidRobot<%>)

    (init-field [x empty])    
    ;; Interp:
    ;; a list whose length is equal to the position of the robot
    
    (super-new)

    (define/public (move-right)
      (new Robot4% [x (cons 99 x)]))

    ;; RETURNS: the x-position of the robot
    (define/public (get-pos)
      (length x))

    ))

;; -> StupidRobot<%>
(define (new-robot)
  (local
    ((define i (random 3)))
    (cond
      [(= i 0) (new Robot1%)]
      [(= i 1) (new Robot2%)]
      [(= i 2) (new Robot3%)]
      [(= i 3) (new Robot4%)])))




