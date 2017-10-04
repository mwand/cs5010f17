#lang racket

;; the model consists of a particle, bouncing with its center from x=0
;; to x=200.  It accepts commands and reports when its status changes

(require rackunit)
(require "extras.rkt")
(require "interfaces.rkt")

(provide make-model)

;; -> Model
(define (make-model) (new Model%))

;; Constructor template for Model%:
;; (new Model%)

(define Model%
  (class* object% (Model<%>)

    ;; boundaries of the field
    (field [lo 0])
    (field [hi 200])

    ;; position and velocity of the object
    (init-field [x (/ (+ lo hi) 2)])
    (init-field [v 0])

    ; ListOfController.  The list of registered controllers
    (init-field [controllers empty])   

    (super-new)

    ;; -> Void
    ;; moves the object by v.
    ;; limits the resulting x to [0, 200].
    ;; publishes x at every tick
    ;; publishes velocity only when it changes
    (define/public (after-tick)
      (set! x (within-limits lo (+ x v) hi))
      (publish-position)
      (if (or (= x hi) (= x lo))
        (begin
          (set! v (- v))
          (publish-velocity))
        "somebody tried to use the value of model.rkt after-tick"))

    (define (within-limits lo val hi)
      (max lo (min val hi)))

    ;; Controller -> Void
    ;; register the new controller and send it some data
    (define/public (register c)
      (begin
        (set! controllers (cons c controllers))
        (send c receive-signal (make-report-position x))
        (send c receive-signal (make-report-velocity v))))

    ;; Command -> Void
    ;; decodes the command, executes it, and sends updates to the
    ;; controllers. 
    (define/public (execute-command cmd)
      (cond
        [(set-position? cmd)
         (begin
           (set! x (set-position-pos cmd))
           (publish-position))]
        [(incr-velocity? cmd)
         (begin
           (set! v (+ v (incr-velocity-dv cmd)))
           (publish-velocity))]))

    ;; report position or velocity to each controller:

    ;; -> Void
    (define (publish-position)
      (let ((msg (make-report-position x)))
        (for-each
          (lambda (obs) (send obs receive-signal msg))
          controllers)
        ))

    ;; -> Void
    (define (publish-velocity)
      (let ((msg (make-report-velocity v)))
        (for-each
          (lambda (obs) (send obs receive-signal msg))
          controllers)))

    ;; The model responds to after-tick, but not to any of the other
    ;; SWidget messages
    (define/public (after-button-down mx my) 'trap)
    (define/public (after-button-up mx my) 'trap)
    (define/public (after-drag mx my) 'trap)
    (define/public (after-move mx my) 'trap)
    (define/public (after-key-event kev) 'trap)
    (define/public (add-to-scene s) s)

    ;; test methods
    (define/public (for-test:get-x) x)
    (define/public (for-test:get-v) v)
    
    ))

;;; tests

;; check to see if the model responds to incr-velocity commands (it
;; does) and to after-tick messages

(begin-for-test

  (let*
      ((m (make-model)))
    (begin
      (check-equal? (send m for-test:get-x) 100)
      (check-equal? (send m for-test:get-v) 0)
      (send m after-tick)
      (check-equal? (send m for-test:get-x) 100)
      (check-equal? (send m for-test:get-v) 0)

      (send m execute-command (make-incr-velocity 2))
      (check-equal? (send m for-test:get-v) 2)

      (send m after-tick)
      (check-equal? (send m for-test:get-x) 102)
      (check-equal? (send m for-test:get-v) 2)

      (send m after-tick)
      (check-equal? (send m for-test:get-x) 104)
      (check-equal? (send m for-test:get-v) 2)
      
      )))

;; m is definitely responding to after-tick messages.  Is it not
;; getting the after-tick messages from big-bang?

;; Ans: yes, that's the problem:  In top.rkt, I created the model, but
;; never added it to the Container (duh!)





    
  




    

    
