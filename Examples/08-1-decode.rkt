;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname 08-1-decode) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; decoding human-readable diffexps

(require "extras.rkt")
(require rackunit)

(define-struct diffexp (exp1 exp2))

;; A DiffExp is either
;; -- a Number
;; -- (make-diffexp DiffExp DiffExp)

;; An Atom is one of
;; -- a Number
;; -- a Symbol

;; An SexpOfAtom is either
;; -- an Atom
;; -- a ListOfSexpOfAtom

;; A ListOfSexpOfAtom is either
;; -- empty
;; -- (cons SexpOfAtom ListOfSexpOfAtom)

;; Observer Templates

#|
;; sexp-fn : SexpOfAtom -> ??
(define (sexp-fn sexp)
  (cond
    [(atom? sexp) (... sexp)]
    [else (... (los-fn sexp))]))

;; los-fn : ListOfSexpOfAtom -> ??
(define (los-fn los)
  (cond
    [(empty? los) ...]
    [else (... (sexp-fn (first los))
               (los-fn (rest los)))]))
|#

;; A MaybeX is one of
;; -- false
;; -- X

;; (define (maybex-fn mx)
;;   (cond
;;     [(false? mx) ...]
;;     [else (... mx)]))

;; succeeded? : MaybeX -> Boolean
;; RETURNS: Is the argument an X?
;; STRATEGY: Use the template for MaybeX
(define (succeeded? mx)
  (cond
    [(false? mx) false]
    [else true]))

;; decode : SexpOfAtom -> MaybeDiffExp

;; Algorithm: if the sexp looks like a diffexp at the top level,
;; recur, otherwise return false.  If either recursion fails, return
;; false.  If both recursions succeed, return the diffexp.

;; EXAMPLES:
#|
(- 3 5) => (make-diffexp 3 5)
(- 2 (- 3 5)) => (make-diffexp 
                   2 
                   (make-diffexp 3 5))
(- (- 2 4) (- 3 5)) 
  => (make-diffexp  
       (make-diffexp 2 4)
       (make-diffexp 3 5))

The following examples all return false
(- 3)               
(+ 3 5)             
(- (+ 3 5) 5)       
((1))               
((- 2 3) (- 1 0))   
(- 3 5 7)           
|#


;; STRATEGY: if the top level of sexp could be the top level of
;; a diffexp, recur on 2nd and 3rd elements. If either recursion
;; fails, return false.  If both recursions succeed, return the diffexp.

;; HALTING MEASURE: # of atoms in sexp

(define (decode sexp)
  (cond
    [(not (could-be-toplevel-of-diffexp? sexp)) false]
    [(number? sexp) sexp]
    [else
     (local
       ((define operand1 (decode (second sexp)))
        (define operand2 (decode (third sexp))))
       (if (and (succeeded? operand1)
                (succeeded? operand2))
           (make-diffexp operand1 operand2)
           false))]))

;; could-be-toplevel-of-diffexp? : SexpOfAtom -> Boolean
;; RETURNS: true iff the top level of the sexp could be the top level
;; of some diffexp.
;; STRATEGY: At the top level, a representation of a 
;; diffexp must be either a number or a list of
;; exactly 3 elements, beginning with the symbol -

(define (could-be-toplevel-of-diffexp? sexp)
  (or (number? sexp)
      (and
       (list? sexp)
       ;; at this point we know that
       ;; sexp is a list, so it is safe to call list functions on it.
       (= (length sexp) 3)
       (equal? (first sexp) '-))))

;;; TESTS

(begin-for-test
  (check-equal? (decode '(- 3 5)) (make-diffexp 3 5))
  (check-equal? (decode '(- 2 (- 3 5)))
    (make-diffexp 
      2 
      (make-diffexp 3 5)))
  (check-equal? (decode '(- (- 2 4) (- 3 5)))
    (make-diffexp  
      (make-diffexp 2 4)
      (make-diffexp 3 5)))
  (check-false (decode '(- 3)))
  (check-false (decode '(+ 3 5)))
  (check-false (decode '(- (+ 3 5) 5)))
  (check-false (decode '((1))))
  (check-false (decode '((- 2 3) (- 1 0))))
  (check-false (decode '(- 3 5 7)))
  )
