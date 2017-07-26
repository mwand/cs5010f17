;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname 02-2-3-traffic-light-with-timer1) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; A Traffic Light changes its color every 20 seconds, controlled by a
;; countdown timer.

;; A TrafficLight is represented as a struct
;;  (make-light color time-left)
;; with the fields
;; color : Color            represents the current color of the traffic light
;; time-left : TimerState   represents the current state of the timer

;; For the purposes of this example, we leave Color and TimerState
;; undefined.  For a working example, we would have to work this out.

;; IMPLEMENTATION
(define-struct light (color time-left))

;; CONSTRUCTOR TEMPLATE
;; (make-light Color TimerState)

;; OBSERVER TEMPLATE
;; light-fn : TrafficLight -> ?
(define (light-fn l)
  (...
   (light-color l)
   (light-time-left l)))

;; light-after-tick : TrafficLight -> TrafficLight
;; GIVEN: the state of a traffic light
;; RETURNS: the state of a traffic light after 1 second
;; EXAMPLES: (omitted)
;; DESIGN STRATEGY: Use constructor template for TrafficLight
(define (light-after-tick l)
  (make-light
   (color-after-tick l)
   (timer-after-tick l)))

(define (color-after-tick l) "stub")
(define (timer-after-tick l) "stub")