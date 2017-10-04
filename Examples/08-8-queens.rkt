;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname 08-XX-queens) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;; N-queens problem

(require rackunit)
(require "extras.rkt")
(require "sets.rkt")

;; A classic problem:

;; Place N queens on an NxN chessboard such that no two queens
;; threaten each other.

;; Queens at (r1,c1) and (r2,c2) threaten each other iff either
;; r1=r2, c1=c2, r1+c1 = r2+c2 or r1-c1 = r2-c2

;; Configurations:
;; A configuration is a set of queens 
;; We will only represent configurations of the form
;; {(1,c1),(2,c2),...} 

;; A Configuration is _legal_ iff no queen in the configuration
;; threatens any other queen.
;; A Configuration is _complete_ iff it has a queen on every row of the board.

;; Given a legal configuration 
;; config = {(1,c1),(2,c2),...(k, c_k)}
;; the legal successors of config are those configurations
;; {(1,c1),(2,c2),...(k, c_k), (k+1, c_k+1)}
;; that are legal

;; Data Design
;; The data design should accomodate boards of any size.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; QUEENS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-struct queen (row col))
;; A Queen is a (make-queen PosInt PosInt)

;; Queen Queen -> Boolean
;; STRATEGY: combine simpler functions
(define (threatens? q1 q2)
  (or
    (= (queen-row q1) (queen-row q2))
    (= (queen-col q1) (queen-col q2))
    (= 
      (+ (queen-row q1) (queen-col q1))
      (+ (queen-row q2) (queen-col q2)))
    (= 
      (- (queen-row q1) (queen-col q1))
      (- (queen-row q2) (queen-col q2)))))

;; Queen ListOfQueens -> Boolean
;; Strategy: Use HOF ormap on other-queens
(define (threatens-any? this-queen other-queens)
  (ormap
    (lambda (other-queen) (threatens? this-queen other-queen))
    other-queens))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; CONFIGURATIONS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; A Config is a ListOfQueen
;; WHERE: the queens are listed in _decreasing_ row order, eg
;; ((k, c_k), (k-1, c_k-1), ... (1, c1))

;; A LegalConfiguration is a Configuration
;; WHERE no queen in the configuration threaten any other queen.

;; A LegalConfiguration is _complete_ iff it has a queen on every row
;; of the board. 

;; : -> LegalConfig
(define empty-config empty)

;; legal-to-add-queen? : PosInt LegalConfig -> Bool
;; GIVEN: a column col and a legal configuration
;;   ((k, c_k), (k-1, c_k-1), ... (1, c1))
;; RETURNS: true iff ((k+1, col) (k, c_k), (k-1, c_k-1),
;; ... (1, c1)) is a legal configuration.
;; STRATEGY: Cases on whether the configuration is empty.

(define (legal-to-add-queen? col config)
  (or 
    (empty? config) ;; first queen is always legal
    (local
      ((define next-row (+ 1 (length config)))
       (define new-queen (make-queen next-row col)))
      (not (threatens-any? new-queen config)))))

;; place-queen : PosInt LegalConfig -> LegalConfig
;; GIVEN: a column col and a legal config of some length k 
;; WHERE: the new queen at (k+1, col)  doesn't threaten any of the
;; existing queens.
;; RETURNS: the given configuration with a new queen added at
;; (k+1,col)
;; STRATEGY: cases on whether the configuration is empty.

(define (place-queen col config)
  (if (empty? config)
      (list (make-queen 1 1))
      (local
        ((define next-row (+ 1 (length config)))
         (define new-queen (make-queen next-row col)))
        (cons new-queen config))))

;; Config PosInt -> Boolean
;; RETURNS: Is the configuration complete for a board of size n?
;; STRATEGY: combine simpler functions

(define (config-complete? config size)
  (= size (length config)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; BUILDING A CONFIGURATION

;; complete-configuration : LegalConfig PosInt-> MaybeLegalConfig
;; GIVEN: a legal configuration
;; RETURNS: an extension of the given configuration to the given size,
;; if there is one, otherwise false.
;; STRATEGY: Recur on  each legal placement of next queen.
;; DETAILS: Given ((k, c_k), (k-1, c_k-1), ... (1, c1)), we generate all the
;; configurations ((k+1, c_k+1), (k, c_k), (k-1, c_k-1), ... (1, c1))
;; a recur on each of them until we find one that works.
;; HALTING MEASURE: (- size (length config))

(define (complete-configuration config size)
  (cond
    [(config-complete? config size) config]
    [else
      (first-success
        (lambda (config1) (complete-configuration config1 size))
        (legal-successors config size))]))

;; LegalConfig Nat -> ListOfLegalConfig
;; GIVEN a legal configuration  ((k, c_k), (k-1, c_k-1), ... (1, c1))
;; RETURNS: the list of all legal configurations 
;;   ((k+1, c_k+1), (k, c_k), (k-1, c_k-1), ... (1, c1))
;; STRATEGY: Use HOF filter on [1,n] to find all places on which it is
;; legal to place next queen.  Use map on the result to construct each
;; such configuration. 

(define (legal-successors config size)
  (map
    (lambda (col) (place-queen col config))
    (filter
      (lambda (col) (legal-to-add-queen? col config))
      (integers-from 1 ncols))))

;; GIVEN: integers n and m
;; RETURNS: a list containing the integers in [n,m]
;; STRATEGY: recur on n+1;  halt when n > m.
;; HALTING MEASURE: max(0,m-n).

(define (integers-from n m)
  (cond
    [(> n m) empty]
    [else (cons n (integers-from (+ n 1) m))]))

;; (X -> MaybeY) ListOfX -> MaybeY
;; first elt of lst s.t. (f elt) is not false; else false
;; like ormap, but in ISL ormap requires booleans.
;; in #lang racket, we could just use ormap
;; STRATEGY: Use template for ListOfX on lst

(define (first-success f lst)
  (cond
    [(empty? lst) false]
    [else
     (local ((define y (f (first lst))))
       (if (not (false? y))
           y
           (first-success f (rest lst))))]))
   
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; TOP LEVEL

;; Nat -> MaybeLegalConfig
;; STRATEGY: Call a more general function
(define (nqueens n)
  (complete-configuration empty-config n))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; TESTING
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;; RETURNS: Is the configuration legal?
(define (config-legal? config)
  (cond
    [(empty? config) true]
    [else (and
            (not (threatens-any? (first config) (rest config)))
            (config-legal? (rest config)))]))

(begin-for-test
  "config-legal tests; "
  (check-true (config-legal? empty))
  (check-false (config-legal? (list (make-queen 1 2) (make-queen 1 3))))
  (check-false (config-legal? (list (make-queen 1 2) (make-queen 7 8) 
                                (make-queen 3 5))))
  (check-true (config-legal? 
                (list (make-queen 4 3) (make-queen 3 1) 
                  (make-queen 2 4)
                  (make-queen 1 2)))))


;; test-nqueens : PosInt -> Boolean
;; GIVEN: N
;; RETURNS: true iff (nqueens n) returns false OR its solution is
;; legal
(define (test-nqueens n)
  (local
    ((define result (time (nqueens n))))
    (if (false? result)
        true
        (and
         (complete? n result)
         (config-legal? result)))))

(begin-for-test
  (check-true (test-nqueens 1))
  (check-true (test-nqueens 2))
  (check-true (test-nqueens 3))
  (check-true (test-nqueens 4))
  (check-true (test-nqueens 5))
  (check-true (test-nqueens 6))
  (check-true (test-nqueens 7))
  (check-true (test-nqueens 8))
  (check-true (test-nqueens 9))
  (check-true (test-nqueens 10))
  (check-true (test-nqueens 15))
;;  (check-true (test-nqueens 18))
;;  (check-true (test-nqueens 20))
  )
  



