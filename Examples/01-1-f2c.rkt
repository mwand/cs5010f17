;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname 01-1-f2c) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; f2c.rkt : Simple Example showing the elements of the design recipe

;; Goal: convert Fahrenheit to Celsius

(require rackunit)
(require "extras.rkt") 

;; DATA DEFINITIONS:

;; A FarenTemp   is represented as a Real.
;; A CelsiusTemp is represented as a Real.

;; f2c: FarenTemp -> CelsiusTemp         
;; GIVEN: a temperature in Fahrenheit, 
;; RETURNS: the equivalent temperature in Celsius.
;; EXAMPLES:
;; (f2c 32) = 0
;; (f2c 212) = 100
;; DESIGN STRATEGY: Transcribe Formula

(define (f2c x)
  (+ (* 5/9 x) -160/9))

;; TESTS
(begin-for-test
  (check-equal? (f2c 32) 0 
    "32 Fahrenheit should be 0 Celsius")
  (check-equal? (f2c 212) 100 
    "212 Fahrenheit should be 100 Celsius"))

;; f2mars : FarenTemp -> CelsiusTemp
;; GIVEN: A temperature in Fahrenheit
;; RETURNS: The mean temperature on the surface of Mars, in Celsius
;; EXAMPLE:
;; (f2mars <anything>) = -451
;; DESIGN STRATEGY: Return the constant answer
;; NOTE: I haven't looked this up, so I don't know if this is actually
;; the temperature on the surface of Mars.

(define (f2mars x)
  -451)

;; TESTS:
;; 1. Look up the actual temperature on the surface of mars.
;;    Insert it into the "..."s below.
;; 2. Run the test
(begin-for-test
  (check-equal? (f2mars 0) ...)
  (check-equal? (f2mars 100) ...))

;; Note: if you don't fill in the blanks, you will get a message like
;; ...: expected a finished expression, but found a template

;; Note: -451 is almost certainly the wrong answer.  Don't just fill in -451.


