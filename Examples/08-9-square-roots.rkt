;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname square-roots) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;; square-root programs

(require rackunit)
(require "extras.rkt")

;; Note: Nat means the natural numbers: {0, 1, 2, ...}
;; The purpose statements have been slightly improved over the
;; versions in the lesson

;; int-sqrt : Nat -> Nat
;; GIVEN: a natural number z
;; RETURNS: a natural number z such that z² ≤ N < (z+1)²

;; EXAMPLES:
;; (int-sqrt 25) = 5 
;; (int-sqrt 26) = 5 ... 
;; (int-sqrt 35) = 5
;; (int-sqrt 36) = 6

(begin-for-test
  (check-equal? (int-sqrt 25) 5)
  (check-equal? (int-sqrt 26) 5)
  (check-equal? (int-sqrt 35) 5)
  (check-equal? (int-sqrt 36) 6))


;; linear-search :  Nat [Nat -> Bool] -> Maybe<Nat>
;; GIVEN:  given a number N and a predicate p
;; RETURNS: the smallest number in [0,N) that satisfies p, or false if 
;; there is none. 
(define (linear-search N p)
  (local
    (;; inner-loop: Nat -> Maybe<Nat>
     ;; GIVEN a natural number i
     ;; WHERE 0 ≤ i < N
     ;; AND p(j) is false for 0 <= j < i
     ;; RETURNS: the smallest number in [0,N) that satisfies p, or false if 
     ;; there is none.
     ;; STRATEGY: if not done, then recur on i+1.
     ;; HALTING MEASURE: (N - i)
     (define (inner-loop i)
       (cond
         [(p i) i]
         [(= i (- N 1)) false]
         [else (inner-loop (+ 1 i))])))      
    (inner-loop 0)))



;; STRATEGY: Call more general function
(define (int-sqrt.v0 n)
  (linear-search n 
    (lambda (z) (< n (* (+ z 1) (+ z 1))))))

(define (int-sqrt.v1 n)
  (local
    ((define (inner-loop z)
       ;; GIVEN z : Nat
       ;; WHERE z² ≤ n
       ;; RETURNS: a natural number z such that z² ≤ n < (z+1)²
       ;; STRATEGY: recur on (+ z 1)
       ;; HALTING MEASURE (- n z)
       (cond
         [(< n (sqr (+ z 1))) z]
         [else (inner-loop (+ z 1))])))
    (inner-loop 0)))

;; STRATEGY: general recursion
(define (int-sqrt.v2 n)
  (local
    ((define (inner-loop z u)
       ;; GIVEN z : Nat
       ;; WHERE z² <= n
       ;; AND u = (z+1)²       
       ;; RETURNS: a natural number z such that z² ≤ n < (z+1)²
       ;; STRATEGY: recur on (+ z 1)
       ;; HALTING MEASURE: (- n z)
       (cond
         [(< n u) z]
         [else (inner-loop 
                (+ 1 z)
                (+ u (* 2 z) 3))])))      
    (inner-loop 0 1))) 

;; STRATEGY: general recursion
(define (int-sqrt.v3 n)
  (local
    ((define (inner-loop z u v)
       ;; GIVEN z : Nat
       ;; WHERE z^2 <= n
       ;; AND u = (z+1)^2
       ;; AND v = 2*z+3
       ;; RETURNS: a natural number z such that z² ≤ n < (z+1)²
       ;; STRATEGY: recur on (+ z 1)
       ;; HALTING MEASURE: (- n z)
       (cond
         [(< n u) z]
         [else (inner-loop 
                (+ 1 z)
                (+ u v)
                (+ v 2))])))
    (inner-loop 0 1 3)))

;; uncomment ONE of the following lines:
 "v0" (define (int-sqrt n) (int-sqrt.v0 n))
;; "v1" (define (int-sqrt n) (int-sqrt.v1 n))
;; "v2" (define (int-sqrt n) (int-sqrt.v2 n))
;; "v3" (define (int-sqrt n) (int-sqrt.v3 n))



