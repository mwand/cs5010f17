;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname 02-4-traffic-light-with-timer) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(require rackunit)
(require "extras.rkt")

;; A more refined version of the next-color problem.

;; A traffic light changes color every 20 seconds.  Given a traffic
;; light color and the number of seconds until the next change, write
;; functions that return the color at the next second, and the
;; time-until-change at the next second. 

;; DATA DEFINITIONS:
;; a TrafficLightColor (TLColor) is one of
;; -- "red"
;; -- "yellow" 
;; -- "green"
;; INTERP: self-evident
;; EXAMPLES
(define red-color "red")
(define yellow-color "yellow")
(define green-color "green")

;; TEMPLATE
;; tlc-fn : TLColor -> ??
;(define (tlc-fn c)
;  (cond
;    [(string=? c "red")    
;     ...]
;    [(string=? c "yellow")
;     ...]
;    [(string=? c "green")  
;     ...]))    

;; countdown timer
;; A TimerState is a NonNegativeInteger
;; WHERE: 0 < t <= COLOR-CHANGE-INTERVAL
;; INTERP: number of seconds until the next color change.
;; If t = 1, then the color should change at the next second.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; constants

;; the interval between color changes
(define COLOR-CHANGE-INTERVAL 20) 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; timer-at-next-second : TimerState -> TimerState
;; GIVEN: A TimerState
;; RETURNS: the TimerState at the next second
;; EXAMPLES:
;; (timer-at-next-second 17) = 16
;; (timer-at-next-second 1) = COLOR-CHANGE-INTERVAL
;; STRATEGY: Cases
(define (timer-at-next-second t)
  (if (= t 1)
    COLOR-CHANGE-INTERVAL
    (- t 1)))

(check-equal? (timer-at-next-second 17) 16)
(check-equal? (timer-at-next-second 1) COLOR-CHANGE-INTERVAL)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
;; STRATEGY: Structural Decomposition on c

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
;; STRATEGY: use template for TLColor on c

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
    "next color after green not yellow")

  (check-equal? (next-color "yellow") "red"
    "next color after yellow not red")

  (check-equal? (next-color "red") "green"
    "next color after red not green"))



;; EXERCISE:
;; The (= t 1) is still repeated: it's in both timer-at-next-second
;; and color-at-next-second
;; Break up timer-at-next-second the same way--

;; ready-to-change? : TimerState -> Boolean
;; RETURNS: true iff the given TimerState should recycle at the
;; next second.
;; EXAMPLES:
;; (ready-to-change? 1) = true
;; (ready-to-change? 17) = false

;; Then rewrite timer-at-next-second and color-at-next-second to use
;; ready-to-change? .




