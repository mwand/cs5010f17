;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname solution) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

;; binary-search-count : Nat (Nat -> Real) -> Nat
;; GIVEN: a number N and a function f : Nat -> Real
;; WHERE: f is monotonic (ie, i<=j implies f(i) <= f(j))
;; RETURNS: the number of i in [0,N] s.t. f(i) = f(N)
;; STRATEGY: call a more general function

(define (binary-search-count N f)
  (binary-search-count-loop 0 N f))


;; binary-search-count-loop : Nat Nat (Nat -> Number) Number -> Nat
;; GIVEN: two numbers lo and hi, a function f and a target tgt
;; WHERE: f is monotonic (ie, i<=j implies f(i) <= f(j))
;; RETURNS: the number of i in [lo,hi] s.t. f(i) = f(hi)
;; STRATEGY: recur on left half of [lo,hi] if needed
;; HALTING MEASURE: (max (- hi lo) 0)
;; TERMINATION ARGUMENT: (max (- hi lo) 0) is guaranteed to be
;; non-negative,and it decreases at every recursive call, because p < hi
(define (binary-search-count-loop lo hi f)
  (cond
    [(> lo hi) 
     ;; the search range is empty, return false
     0]    
    [(= lo hi) 
     ;; constant range is of size 1, and f(lo) = f(hi)
     1]
    [else (local
            ((define tgt (f hi))
             ;; using floor guarantees that p < hi
             (define p   (floor (/ (+ lo hi) 2)))
             (define f-of-midpoint (f p)))
            (cond
              [(< f-of-midpoint tgt)
                ;; midpoint is not in the constant range
               (binary-search-count-loop (+ p 1) hi f)]
              ;; f-of-midpoint can't be greater than tgt
              ;; [(> f-of-midpoint tgt)
              ;;  ;; the tgt is in the left half
              ;; (binary-search-count-loop lo (- p 1) f tgt)]
              [else 
                ;; midpoint is in the constant range. Add right-hand
                ;; region to the constant range, and recur on (- p 1)
                ;; midpoint 1)
               (+ hi (- p)   ;; size of [p+1, hi]
                 (binary-search-count-loop lo p f))]))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; TESTS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(require rackunit)
(require "extras.rkt")

;; examples for testing

(define (list-to-fn lst)
  (lambda (n)
    (list-ref lst n)))

(define fn1 (list-to-fn '(3 4 5 5 5 6 6 6 7 7 7 8)))

(begin-for-test
  (check-equal?
    (binary-search-count 11 fn1)
    1)
  (check-equal?
    (binary-search-count 10 fn1)
    3)
  (check-equal?
    (binary-search-count 9 fn1)
    2)
  (check-equal?
    (binary-search-count 2 fn1)
    1)
  (check-equal?
    (binary-search-count 3 fn1)
    2)
  (check-equal?
   (binary-search-count 4 fn1)
   3)
  )
