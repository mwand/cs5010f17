;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname 02-3-1-traffic-light-with-timer2) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; A Traffic Light changes its color every 20 seconds, controlled by a
;; countdown timer.

(require rackunit)
(require "extras.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CONSTANTS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; the interval between color changes
(define COLOR-CHANGE-INTERVAL 20) 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; DATA DEFINITIONS:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Color

;; a Color is represented by one of the strings
;; -- "red"
;; -- "yellow" 
;; -- "green"
;; INTERP: self-evident
;; EXAMPLES:
(define red-color "red")
(define yellow-color "yellow")
(define green-color "green")


;; countdown timer

;; A TimerState is represented a PositiveInteger
;; WHERE: 0 < t <= COLOR-CHANGE-INTERVAL
;; INTERP: number of seconds until the next color change.
;; If t = 1, then the color should change at the next second.


;; TrafficLight

;; A TrafficLight is represented as a struct
;;  (make-light color time-left)
;; with the fields
;; color : Color            represents the current color of the traffic light
;; time-left : TimerState   represents the current state of the timer

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; FUNCTIONS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; light-at-next-second : TrafficLight -> TrafficLight
;; GIVEN: the state of a traffic light
;; RETURNS: the state of a traffic light after 1 second
;; EXAMPLES: (omitted)
;; DESIGN STRATEGY: Use constructor template for TrafficLight

(define (light-at-next-second l)
  (make-light
   (color-at-next-second l)
   (timer-at-next-second l)))

;;;;;;;;;;;;;;;;

;; timer-at-next-second : TimerState -> TimerState
;; GIVEN: A TimerState
;; RETURNS: the TimerState at the next second
;; EXAMPLES:
;; (timer-at-next-second 17) = 16
;; (timer-at-next-second 1) = COLOR-CHANGE-INTERVAL
;; STRATEGY: if t = 1 then recycle, otherwise decrement
(define (timer-at-next-second t)
  (if (= t 1)
    COLOR-CHANGE-INTERVAL
    (- t 1)))

(check-equal? (timer-at-next-second 17) 16)
(check-equal? (timer-at-next-second 1) COLOR-CHANGE-INTERVAL)

;;;;;;;;;;;;;;;;

;; color-at-next-second needs to inspect both the current color and
;; current timer state!

;; color-at-next-second : TLColor TimerState -> TLColor
;; GIVEN: a TLColor c and a TimerState t
;; RETURNS: the color of the traffic light at the next second.
;; EXAMPLES:
;; (color-at-next-second red-color 7) = red-color
;; (color-at-next-second red-color 1) = green-color
;; (color-at-next-second green-color 1) = yellow-color
;; (color-at-next-second yellow-color 1) = red-color

; these tests really should have some error messages associated with them.
(begin-for-test
   (check-equal? (color-at-next-second red-color 7) red-color)
   (check-equal? (color-at-next-second red-color 1) green-color)
   (check-equal? (color-at-next-second green-color 1) yellow-color)
   (check-equal? (color-at-next-second yellow-color 1) red-color))
  

#|
Here we need to examine both the TLColor c and the timer state t.
Which shall we do first?  Let's try each one and see how each of them
works out
|#

;; VERSION 1
;; STRATEGY: Cases on c : Color

#;(define (color-at-next-second c t) 
  (cond
    [(string=? c "red")    
     (if (= t 1) "green" "red")]
    [(string=? c "yellow")
     (if (= t 1) "red" "yellow")]
    [(string=? c "green") 
     (if (= t 1) "yellow" "green")]))    

;; That's ugly!  Look at all the (if (= t 1) ...)'s.
;; It also violates "one function, one task", since it has to decide
;; WHEN to change color AND also what color to change to.

;; Let's try it the other way

;; VERSION 2
;; STRATEGY: Cases on t
(define (color-at-next-second c t)
  (if (= t 1) (next-color c) c))

;; next-color : TLColor -> TLColor
;; GIVEN: a TLColor
;; RETURNS: the TLColor that follows the given TLColor
;; (next-color "red") = "green"
;; (next-color "yellow") = "red"
;; (next-color "green") = "yellow"
;; STRATEGY: cases on c : TLColor

(define (next-color c) 
  (cond
    [(string=? c "red")    
     "green"]
    [(string=? c "yellow")
     "red"]
    [(string=? c "green")  
     "yellow"]))    

;; That was better.  
;; Each function has its own task: next-color knows about color, and
;; color-at-next-second knows about the timer.  

(begin-for-test
  (check-equal? (next-color "green") "yellow"
    "next color after green was not yellow")

  (check-equal? (next-color "yellow") "red"
    "next color after yellow was not red")

  (check-equal? (next-color "red") "green"
    "next color after red was not green"))



;; EXERCISE:
;; The (= t 1) is still repeated: it's in both timer-at-next-second
;; and color-at-next-second
;; Break up timer-at-next-second the same way--

;; The first student in Fall 2017 who posts the code 346827 to Piazza
;; gets a prize.

;; ready-to-change? : TimerState -> Boolean
;; RETURNS: true iff the given TimerState should recycle at the
;; next second.
;; EXAMPLES:
;; (ready-to-change? 1) = true
;; (ready-to-change? 17) = false

;; Then rewrite timer-at-next-second and color-at-next-second to use
;; ready-to-change? .




