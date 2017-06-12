#lang racket

(require "WidgetWorks.rkt")
(require "Interfaces.rkt")
(require "Model.rkt")
(require "ControllerFactory.rkt")

;; run with (run 0.5)

;; create a container, install a factory, and run.

(define (run rate)
  (let ((c (container-init CANVAS-WIDTH CANVAS-HEIGHT))
        (m (make-model)))
    (begin
      (send c add-stateful-widget m)
      (send c add-stateful-widget (make-controller-factory c m))
      (send c run rate))))
