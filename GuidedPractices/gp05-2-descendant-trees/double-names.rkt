;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname 06-6-double-names) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;; double-names.rkt

(require rackunit)
(require "extras.rkt")

;; A Person is represented as a struct
;; (make-person name children)

;; INTERPRETATION
;; name   :   String (any string will do)  --the name of the person
;; children : PersonList                   --the children of the
;;                                           person

;; IMPLEMENTATION:
(define-struct person (name children))

;; CONSTRUCTOR TEMPLATES:
;; For Person:
;;   (make-person String PersonList)
;; For PersonList:
;;   empty
;;   (cons Person PersonList)

;; OBSERVER TEMPLATE:

;; person-fn : Person -> ??
;; (define (person-fn p)
;;  (... (person-name p) (plist-fn (person-children p))))
;;
;; plist-fn : PersonList -> ??
;; (define (plist-fn ps)
;;  (cond
;;    [(empty? ps) ...]
;;    [else (... (person-fn (first ps))
;;               (plist-fn (rest ps)))]))

;; person-double-name : Person -> Person
;; RETURNS: a Person like the given one, except that the names of the
;; person and all of his descendants have their names doubled.

;; personlist-double-names : PersonList -> PersonList
;; Returns a PersonList like the given one, except that the names of each
;; of the persons and all of their descendants have their names doubled.

;; STRATEGY: Use template for Person/PersonList 

(define (person-double-name p)
  (make-person
    (string-append (person-name p) (person-name p))
    (personlist-double-names (person-children p))))


(define (personlist-double-names ps)
  (cond
    [(empty? ps) ps]
    [else (cons 
            (person-double-name (first ps))
            (personlist-double-names (rest ps)))]))

(begin-for-test
  (check-equal?
    (person-double-name
      (make-person "chuck" 
        (list (make-person "alice" empty) 
          (make-person "bob" empty))))
    (make-person "chuckchuck" 
      (list (make-person "alicealice" empty) 
        (make-person "bobbob" empty)))))
