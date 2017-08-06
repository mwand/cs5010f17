;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname 05-2-descendants) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

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

(define alice (make-person "alice" empty))
(define bob   (make-person "bob" empty))
(define chuck (make-person "chuck" (list alice bob)))

(define dave  (make-person "dave" empty))
(define eddie (make-person "eddie" (list dave)))

(define fred  (make-person "fred" (list chuck eddie)))

;; grandchildren : Person -> PersonList
;; RETURNS: a list of the grandchildren of the given person.
;; (grandchildren fred) = (list alice bob dave)
;; STRATEGY: Use template for Person/LoP

(define (grandchildren p)
  (all-children (person-children p)))

;; all-children : PersonList -> PersonList
;; GIVEN: a list of persons, 
;; RETURNS: the list of all their children.
;; (all-children (list fred eddie)) = (list chuck eddie dave)
;; STRATEGY: Use observer template for PersonList

(define (all-children lop)
  (cond
    [(empty? lop) empty]
    [else (append
           (person-children (first lop))
           (all-children (rest lop)))]))

;;; Hmm... We should make sure that our tests accept the answers in
;;; any order (including duplicates!), so we'll need a help function
;;; 'same-people?' . We'll have to test that one, too, of course...

(begin-for-test
  (check same-people?
   (all-children (list fred eddie)) 
   (list chuck eddie dave))
  ;; same test, but with the answer in a different order
  (check same-people?
         (all-children (list fred eddie)) 
         (list eddie chuck dave))
  ;; the spec seems to allow duplicates, so we'll test that, too
   (check same-people?
         (all-children (list fred eddie)) 
         (list eddie chuck dave chuck))
  (check same-people?
   (grandchildren fred) 
   (list alice bob dave))
)



;; descendants : Person -> LoP
;; GIVEN: a Person, 
;; RETURNS: the list of his/her descendants
;; all-descendants : LoP -> LoP
;; GIVEN: a PersonList,
;; RETURNS: the list of all their descendants
;; (descendants fred) = (list chuck eddie alice bob dave)
;; (all-descendants (list chuck eddie)) = (list alice bob eddie)
;; STRATEGY: Use template for Person/LoP

(define (descendants p)
  (append
   (person-children p)
   (all-descendants (person-children p))))

(define (all-descendants lop)
  (cond
    [(empty? lop) empty]
    [else (append
           (descendants (first lop))
           (all-descendants (rest lop)))]))

(begin-for-test
  (check same-people?
    (descendants fred)
    (list chuck eddie alice bob dave))
  (check same-people?
    (all-descendants (list chuck eddie)) (list alice bob dave)))

;;; help function for testing:

;; same-people? : PersonList PersonList -> Bool
;; RETURNS: true iff the two personlists contain exactly the same
;; people.  Duplicates are allowed.
;; STRATEGY: Combine simpler functions

(define (same-people? lst1 lst2)
  (and
    (people-subset? lst1 lst2)
    (people-subset? lst2 lst1)))

;; people-subset? : PersonList PersonList -> Bool
;; RETURNS: true iff every person in the first list occurs in the
;; second list.
;; STRATEGY: Observer template for PersonList

(define (people-subset? lst1 lst2)
  (cond
    [(empty? lst1) true]
    [(member (first lst1) lst2)
     (people-subset? (rest lst1) lst2)]
    [else false]))

;; Observe that there's not much here that is specific to Person.
;; That suggests that we should generalize these functions.  We'll do
;; that in Module 06.

(begin-for-test
  (check-true
    (same-people?
      (list chuck eddie alice bob dave)
      (list chuck alice eddie bob dave chuck))
    "reorderings and duplicates in second arg shouldn't change answer
from same-people?")
  (check-true
    (same-people?
      (list chuck alice eddie bob dave chuck)
      (list chuck eddie alice bob dave))
    "reorderings and duplicates in first arg shouldn't change answer
from same-people?")
  (check-false
    (same-people?
      (list chuck alice eddie bob dave chuck)
      (list chuck alice       bob dave chuck))
    "deletion in 2nd argument should return false")

  (check-false
    (same-people?
      (list chuck alice       bob dave chuck)
      (list chuck alice eddie bob dave chuck))
    "deletion in 1st argument should return false"))



    

