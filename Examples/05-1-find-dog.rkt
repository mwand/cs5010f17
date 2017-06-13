;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname 05-1-find-dog) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(require rackunit)
(require "extras.rkt")

;; Data Definition for Lists:

;; A ListOfX is either
;; -- empty  
;; -- (cons X ListOfX)   
;; INTERP:
;; empty             represents the sequence with no X's
;; (cons x xs)       represents the sequence whose first element is x
;;                   and the rest of the sequence is represented by xs

;; TEMPLATE:
;; lox-fn : ListOfX -> ?
;; (define (lox-fn lox)
;;   (cond
;;     [(empty? lox) ...]
;;     [else (...
;;             (first lox)
;;             (lox-fn (rest lox)))]))


;; find-dog : ListOfString -> Boolean
;; RETURNS: true if "dog" is in the given list.
;; STRATEGY: Use template for ListOfString on los
#;(define (find-dog los)
  (cond
    [(empty? los) false]
    [else (or 
           (string=? (first los) "dog")           
           (find-dog (rest los)))]))

(begin-for-test
  (check-equal? (find-dog (list "cat" "dog" "weasel")) true)
  (check-equal? (find-dog (list "cat" "elephant" "weasel")) false))

;; find-cat : ListOfString -> Boolean
;; RETURNS: true if "cat" is in the given list
;; STRATEGY: Use template for ListOfString on los
#;(define (find-cat los)
  (cond
    [(empty? los) false]
    [else (or
           (string=? (first los) "cat")
           (find-cat (rest los)))]))

(begin-for-test
  (check-equal? (find-cat (list "cat" "dog" "weasel")) true)
  (check-equal? (find-cat (list "elephant" "weasel")) false))

;; find-animal : ListOfString String -> Boolean
;; GIVEN: A list of strings and a string
;; RETURNS: true iff the given string is in the given los.
;; STRATEGY: Use template for ListOfString on los
(define (find-animal los str)
  (cond
    [(empty? los) false]
    [else (or 
           (string=? (first los) str)           
           (find-animal (rest los) str))]))

(begin-for-test
  "find-animal"
  (check-equal? (find-animal (list "cat" "elephant" "weasel") "elephant")
    true)
  (check-equal? (find-animal (list "cat" "elephant" "weasel") "beaver")
    false))

;; STRATEGY: Call a more general function
(define (find-dog los)
  (find-animal los "dog"))

;; STRATEGY: Call a more general function
(define (find-cat los)
  (find-animal los "cat"))
