;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname 06-4-1-mapreduce) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; examples using mapreduce

(require rackunit)
(require "extras.rkt")

;; Here is a useful generalization of foldr:

;; mapreduce : (Y Y -> Y) Y (X -> Y) XList -> Y
;; GIVEN: f v g (list x1 ... xn))
;; RETURNS: (f (g x1) (f (g x2) (f (g x3) ... v)))
(define (mapreduce f v g lst)
  (foldr f v (map g lst)))

;; This generalization is useful because many problems can be stated
;; in this form WITH f ASSOCIATIVE, and v the identity of f.

;; When f is associative, we can write f using infix notation (let's
;; denote it [f]).

;; Then we can say:

;; RETURNS: (g x1) [f] (g x2) [f] (g x3) ... [f] (g x4)

;; For example, instead of writing

;; strategy: combine simpler functions
(define (add1-if-true b n)
  (if b (+ n 1) n))

;; strategy: Use HOF foldr on lob
(define (count-trues lob) 
  (foldr add1-if-true 0 lob))

;; we can write:
(define (bool->num b) (if b 1 0))

(define (count-true2 lob)
  (foldr + 0 (map bool->num lob)))

;; or

(define (count-true3 lob)
  (let ((mapper (lambda (b) (if b 1 0))))
    (mapreduce + 0 mapper lob)))




