;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname 05-1-find-dog) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require rackunit)
(require "extras.rkt")

;; An Animal is represented as a String (any string will do)
;; Here, by "Animal" we mean an animal species, not an individual animal.

;; An AnimalSequence is represented as a list of Animal.

;; CONSTRUCTOR TEMPLATE:
;; empty                Interp: the empty sequence
;; (cons a as)
;;   WHERE:
;;    a  is an Animal    Interp: the first animal in the sequence
;;    as is an AnimalSequence
;;            Interp: the rest of the animals in the sequence

;; OBSERVER TEMPLATE:
;; animal-seq-fn : AnimalSequence -> ??
(define (animal-seq-fn as)
  (cond
    [(empty? as) ...]
    [else (... (first as)
               (animal-seq-fn (rest as)))]))

;; find-dog : AnimalSequence -> Boolean
;; RETURNS: true if "dog" is in the given sequence
;; STRATEGY: Use observer template for AnimalSequence
#;(define (find-dog as)
  (cond
    [(empty? as) false]
    [else (or 
           (string=? (first as) "dog")           
           (find-dog (rest as)))]))

(begin-for-test
  (check-equal? (find-dog (list "cat" "dog" "weasel")) true)
  (check-equal? (find-dog (list "cat" "elephant" "weasel")) false))

;; find-cat : AnimalSequence -> Boolean
;; RETURNS: true if "cat" is in the given sequence
;; STRATEGY: Use observer template for AnimalSequence
#;(define (find-cat as)
  (cond
    [(empty? as) false]
    [else (or
           (string=? (first as) "cat")
           (find-cat (rest as)))]))

(begin-for-test
  (check-equal? (find-cat (list "cat" "dog" "weasel")) true)
  (check-equal? (find-cat (list "elephant" "weasel")) false))

;; find-animal : AnimalSequence Animal -> Boolean
;; GIVEN: A list of strings and a string
;; RETURNS: true iff the given Animal is in the given los.
;; STRATEGY: Use template for on as
(define (find-animal as a)
  (cond
    [(empty? as) false]
    [else (or 
           (string=? (first as) a)           
           (find-animal (rest as) a))]))

(begin-for-test
  "find-animal"
  (check-equal? (find-animal (list "cat" "elephant" "weasel") "elephant")
    true)
  (check-equal? (find-animal (list "cat" "elephant" "weasel") "beaver")
    false))

;; STRATEGY: Call a more general function
(define (find-dog as)
  (find-animal as "dog"))

;; STRATEGY: Call a more general function
(define (find-cat as)
  (find-animal as "cat"))
