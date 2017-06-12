#lang racket

(require "interfaces.rkt")

(provide make-reporter)

;; Model -> Reporter
(define (make-reporter m)
  (new Reporter% [model m]))

;; set up a little object that just receives signals from a model.

(define Reporter%
  (class* object% ()  ;; too lazy to write a Reporter<%> interface
    (init-field model)
    ;; the position and velocity reported by the model
    (field [x 0][v 0])
    (super-new)
    (send model register this)

   ;; Signal -> Void
    ;; decodes signal and updates local data
    (define/public (receive-signal sig)
      (cond
        [(report-position? sig)
         (set! x (report-position-pos sig))]
        [(report-velocity? sig)
         (set! v (report-velocity-v sig))]))

    (define/public (model-x) x)
    (define/public (model-v) v)

    (define/public (model-state)
      (list 'Position: x 'Velocity: v))

    ))
