;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname 01-1-f2c) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;; f2c.rkt : Simple Example showing the elements of the design recipe

;; Goal: convert Fahrenheit to Celsius

(require rackunit)
(require "extras.rkt") 

;; DATA DEFINITIONS: none

;; f2c: Real -> Real          
;; GIVEN: a temperature in Fahrenheit, 
;; RETURNS: the equivalent in Celsius.
;; EXAMPLES:
;; (f2c 32) = 0
;; (f2c 212) = 100
;; DESIGN STRATEGY: Combine simpler functions

(define (f2c x)
  (+ (* 5/9 x) -160/9))

;; TESTS
(begin-for-test
  (check-equal? (f2c 32) 0 
    "32 Fahrenheit should be 0 Celsius")
  (check-equal? (f2c 212) 100 
    "212 Fahrenheit should be 100 Celsius"))
