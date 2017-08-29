#lang racket

;; interfaces for MVC example

(require "WidgetWorks.rkt")

(provide Controller<%> Model<%>)

;; structs for model command language
(provide 
  (struct-out set-position) 
  (struct-out incr-velocity)
  (struct-out report-position)
  (struct-out report-velocity))

;; A Controller is an object of any class that implements
;; Controller<%>

;; There will be several such classes, and there may be several
;; objects of each such class.

(define Controller<%>    
  (interface (SWidget<%>)

    ;; Signal -> Void
    ;; receive a signal from the model and adjust controller
    ;; accordingly 
    receive-signal
    
    ))

;; A Model is an object of any class that implements Model<%>.  Models
;; will receive signals from the Container, so they must implement the
;; SWidget<%> interface in order to do so.

(define Model<%>
  (interface (SWidget<%>)

    ;; Controller -> Void
    ;; Registers the given controller to receive signal
    register          

    ;; Command -> Void
    ;; Executes the given command
    execute-command   
))

;; CONTROLLER/MODEL PROTOCOL:

;; As soon as a controller registers with the model, the model sends
;; the controller a pair of Signals so the controller will know the
;; current state of the model.

;; The controller then sends the model commands, which the model is
;; supposed to execute.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; DATA DEFINITIONS FOR COMMUNICATING WITH MODEL

;; A Command is one of 
;; -- (make-set-position n)     
;; -- (make-incr-velocity dv)

;; A Signal is one of
;; -- (make-report-position n)
;; -- (make-report-velocity v)

;; n, v, dv are all Reals.

;; provide the structs for Command and Signal
;; the syntax of provide in #lang racket has more options in it.


(define-struct set-position (pos) #:transparent)
(define-struct incr-velocity (dv) #:transparent)
(define-struct report-position (pos) #:transparent)
(define-struct report-velocity (v) #:transparent)










