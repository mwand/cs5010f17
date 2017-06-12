;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname 08-2-binary-search) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;; binary search

;; binary search is another example of general recursion.

;; binary-search : Nat (Nat -> Number) Number -> Maybe<Nat>
;; GIVEN: a number N, a function f : Nat -> Number and a number tgt
;; WHERE: f is monotonic (ie, i<=j implies f(i) <= f(j))
;; RETURNS: a number k such that 0 <= k <= N and f(k) = tgt if there is such a k,
;; otherwise false.
;; EXAMPLES/TESTS: See below

;; STRATEGY: call a more general function
(define (binary-search N f tgt)
  (binary-search-loop 0 N f tgt))

;;  we generalize on 0 and N:

;; binary-search-loop : Nat Nat (Nat -> Number) Number -> Maybe<Nat>
;; GIVEN: two numbers lo and hi, a function f and a target tgt
;; WHERE: f is monotonic (ie, i<=j implies f(i) <= f(j))
;; RETURNS: a number k such that lo <= k <= hi and f(k) = tgt if there
;; is such a k, 
;; otherwise false.
;; STRATEGY: recur on either left or right half of [lo,hi].
;; HALTING MEASURE: (max (- hi lo) 0)
;; TERMINATION ARGUMENT: (max (- hi lo) 0) is guaranteed non-negative,
;; and it decreases at every recursive call because p is eliminated
;; See slides for more detailed argument

(define (binary-search-loop lo hi f tgt)
  (cond
    [(> lo hi) 
     ;; the search range is empty, return false
     false]    
    [(= lo hi) 
     ;; the search range has size 1
     (if (equal? (f lo) tgt) lo false)] 
    [else (local
            ((define p (floor (/ (+ lo hi) 2)))
             (define f-of-midpoint (f p)))
            (cond
              [(< f-of-midpoint tgt)
               ;; the tgt is in the right half
               (binary-search-loop (+ p 1) hi f tgt)]
              [(> f-of-midpoint tgt)
               ;; the tgt is in the left half
               (binary-search-loop lo (- p 1) f tgt)]
              [else 
                ;; p is the one we're looking for
                p]))]))

;; observe that we can say (+ p 1) and (- p 1) because
;; in these cases we know that p can't be the answer.  

;; Furthermore, without these, we wouldn't be guaranteed that 
;; (- hi lo) would decrease (think about a search range of size 2!) 

;; Also, note that the (= lo hi) line can be omitted-- 
;; since (- hi lo) decreases, eventually
;; it will be equal to 1, and we'll have p = lo = hi.

;; Another way of looking at this: if we leave off the (= lo hi) line,
;; there are no more recursive calls, so our termination argument is
;; still correct, so max(0,hi-lo) is still a correct halting measure.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; TESTS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(require rackunit)
(require "extras.rkt")

(begin-for-test
  ;; successful search
  (check-equal?
    (binary-search 12 sqr 49)
    7)
  ;; unsuccessful search
  (check-equal?
    (binary-search 12 sqr 48)
    false)
  ;; make sure we don't miss the endpoints
  (check-equal?
    (binary-search 12 sqr 0)
    0)
  (check-equal?
    (binary-search 12 sqr 144)
    12))
