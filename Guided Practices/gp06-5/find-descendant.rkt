;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname 06-6-find-descendant) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t write repeating-decimal #f #t none #f ())))
;; find-descendant.rkt

(require rackunit)
(require "extras.rkt")

;; DATA DEFINITIONS

(define-struct person (name children))

;; A Person is a 
;; (make-person String ListOfPerson)

;; A ListOfPerson is one of
;; -- empty
;; -- (cons Person ListOfPerson)

;; TEMPLATES
#|
;; person-fn : Person -> ??
(define (person-fn p)
  (... (person-name p) 
       (lop-fn (person-children p))))

;; lop-fn : ListOfPerson -> ??
(define (lop-fn ps)
  (cond
    [(empty? ps) ...]
    [else (... (person-fn (first ps))
               (lop-fn (rest ps)))]))

|#

;; person-descendant? : Person String -> Boolean
;; GIVEN: a person and a name
;; RETURNS: true iff that person or any of his/her descendants has
;; that name.
;; EXAMPLES: See below
;; STRATEGY: Use template for Person on p
(define (person-descendant? p str)
  (if (string=? (person-name p) str)
    true
    (lop-any-descendant? (person-children p) str)))

;; this could have been 
;; (or
;;   (string=? (person-name p) str)
;;   (lop-any-descendant? (person-children p) str))


;; lop-any-descendant? : ListOfPerson String -> Boolean
;; GIVEN: A list of persons and a string
;; RETURNS: true iff any of the persons or any of their descendants
;; has that name. 
;; STRATEGY: Use template for ListOfPerson on ps
(define (lop-any-descendant? ps str)
  (cond
    [(empty? ps) false]
    [else (or
            (person-descendant? (first ps) str)
            (lop-any-descendant? (rest ps) str))]))

;; or with the HOF:

;; STRATEGY: Use template for ListOfPerson on ps + HOF ormap on
;; (person-children p)

#;(define (person-descendant? p str)
  (or
    (string=? (person-name p) str)
    (ormap
      (lambda (q) (person-descendant? q))
      (person-children p))))

;; test suite
;; from the slides
(define alice (make-person "alice" empty))
(define bob (make-person "bob" empty))
(define chuck (make-person "chuck" (list alice bob)))

(define dave (make-person "dave" empty))
(define eddie (make-person "eddie" (list dave)))

(define fred (make-person "fred" (list chuck eddie)))

;; Examples:
(define person1 
  (make-person
    "fred"
    (list
      (make-person "chuck"
        (list (make-person "alice" empty) (make-person "bob" empty)))
      (make-person "eddie" 
        (list (make-person "dave" empty))))))

#|
(person-descendant? person1 "fred") = true
(person-descendant? person1 "dave") = true
(person-descendant? person1 "eve")  = false
|#



(begin-for-test
  (check-true (person-descendant? fred "bob"))
  (check-true (person-descendant? chuck "bob"))
  (check-false (person-descendant? chuck "eddie"))
  (check-false (person-descendant? fred "eve")))





