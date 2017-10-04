;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname 08-4-implicit-graphs) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(require rackunit)
(require "extras.rkt")

;; Node = Int

;; Int -> SetOfInt
;; GIVEN: an integer
;; RETURNS: the list of its successors in the implicit graph.
;; For this graph, this is always a set (no repetitions)
(define (successors1 n)
  (if (<= n 0) 
      empty
      (local
        ((define n1 (quotient n 3)))
        (list n1 (+ n1 5)))))

(begin-for-test
  (check set-equal? (successors1 6) (list 2 7))
  (check set-equal? (successors1 2) (list 0 5))
  (check set-equal? (successors1 0) empty)
  (check set-equal? (successors1 5) (list 1 6))
  (check set-equal? (successors1 1) (list 0 5)))

;; SetOfInt -> SetOfInt
;; GIVEN: A set of nodes
;; RETURNS: the set of all their successors in our implicit graph
;; STRATEGY: Use HOFs map, then unionall.

(define (all-successors1 ns)
  (unionall (map successors1 ns)))

(begin-for-test
  (check set-equal? (all-successors1 empty) empty)
  (check set-equal? (all-successors1 (list 2 6))
                    (list 2 7 0 5)))

;; SetOfInt (SetOfInt -> SetOfInt)
;; STRATEGY: recur on (intset-union candidates nodes)
(define (reachables nodes all-successors-fn)
  (local
    ((define candidates (all-successors-fn nodes)))
    (cond
      [(subset? candidates nodes) nodes]
      [else (reachables
             (intset-union candidates nodes)
             all-successors-fn)])))

(begin-for-test 
  (check set-equal? (reachables (list 6) all-successors1)
         (list 0 1 2 5 6 7)))


;;;;; of course, we could generalize all-successors1:

;; (Int -> SetOfInt) -> (SetOfInt -> SetOfInt)
;; GIVEN: A successors function
;; RETURNS: The corresponding all-successors function
(define (make-all-successors successors-fn)
  (local
    ((define (all-successors-fn ns)
       (unionall (map successors-fn ns))))
    all-successors-fn))

;; or even

(define (make-all-successors.v2 successors-fn)
  (lambda (ns)
    (unionall (map successors-fn ns))))

;; Try:
(define all-successors1.v2 (make-all-successors successors1))
;; or
(define all-successors1.v3 (make-all-successors.v2 successors1))

    
    
  





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Functions for SetOfInt

;; ListOf(SetOfInt) -> SetOfInt
;; GIVEN: a list of sets
;; RETURNS: the union of all the sets in the list
;; EXAMPLE
;; (unionall (list (list 1 2 3) (list 2 4 6) (list 3 5))
;;   a list representing the set {1 2 3 4 6 5}.  
;; Any such list is acceptable. 
(define (unionall sets)
  (foldr intset-union empty sets))

;; the other functions here are the same as in sets.rkt

(define (intset-union set1 set2)
  (foldr intset-cons set1 set2))

(define (intset-cons n set1)
  (if (member n set1) 
      set1
      (cons n set1)))

(define (subset? set1 set2)
  (andmap
   (lambda (n) (member n set2))
   set1))

(define (set-equal? set1 set2)
  (and
   (subset? set1 set2)
   (subset? set2 set1)))



      


  
  
   
