;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname 06-6-double-names) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;; double-names.rkt

(require rackunit)
(require "extras.rkt")

;; DATA DEFINITIONS

(define-struct person (name children))

;; A Person is a 
;; (make-person String ListOfPerson)

;; A ListOfPerson is one of
;; -- empty
;; -- (cons Person ListOfPerson)

;; TEMPLATE:

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


;; Note that there are 3 versions here.  Be sure test them all!

;; person-double-name : Person -> Person
;; RETURNS: a Person like the given one, except that the names of the
;; person and all of his descendants have their names doubled.

;; lop-double-names : ListOfPerson -> ListOfPerson
;; Returns a ListOfPerson like the given one, except that the names of each
;; of the persons and all of their descendants have their names doubled.

;; STRATEGY: Use template for Person/ListOfPerson 

(define (person-double-name p)
  (make-person
    (string-append (person-name p) (person-name p))
    (lop-double-names (person-children p))))


(define (lop-double-names ps)
  (cond
    [(empty? ps) ps]
    [else (cons 
            (person-double-name (first ps))
            (lop-double-names (rest ps)))]))

;; or with the list abstractions:
;; STRATEGY: Use HOF map on ps
#;(define (lop-double-names ps)
    (map person-double-name ps))

;; or with the list abstractions (version 2).  
;; STRATEGY: Use template for Person on p + HOF map on
;; (person-children p)

#;(define (person-double-name p)
   (make-person
    (string-append (person-name p) (person-name p))
    (map person-double-name (person-children p))))

;; note that this version is appropriate when all your really care
;; about is the person.



(begin-for-test
  (check-equal?
    (person-double-name
      (make-person "chuck" 
        (list (make-person "alice" empty) 
          (make-person "bob" empty))))
    (make-person "chuckchuck" 
      (list (make-person "alicealice" empty) 
        (make-person "bobbob" empty)))))
