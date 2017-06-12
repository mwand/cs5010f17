;;; Stress tests for ps08, from Will Clinger
;;; posted as: https://piazza.com/class/iqtyi26lqud5mu?cid=859

;; benchmarks for problem set 08
;; Efficiency is again an issue for problem set 08, so here are a
;; couple of benchmarks that may help you to determine whether your
;; solution is adequately efficient. As with problem set 07, you do
;; not need to add these benchmarks to your programs, and you should
;; not use these benchmarks to write tests that would take a long time
;; to run when we run our automated black-box tests on your programs. 
 
;; For the first question:
 
;;; make-stress-input-without-loops : PosInt -> Program
;;; GIVEN: an integer n
;;; RETURNS: an SGS program with no loops that defines n functions
;;; EXAMPLES:
;;;     (make-stress-input-without-loops 1)
;;;  => (list
;;;      (make-def 'f1 (list 'x 'y) (make-varexp 'x)))
;;;
;;;     (make-stress-input-without-loops 3)
;;;  => (list
;;;      (make-def 'f1 (list 'x 'y) (make-varexp 'x))
;;;      (make-def 'f2 (list 'x 'y)
;;;        (make-appexp 'f1 (list (make-varexp 'x) (make-varexp 'x))))
;;;      (make-def 'f3 (list 'x 'y)
;;;        (make-appexp
;;;         'f2
;;;         (make-appexp 'f2 (list (make-varexp 'x) (make-varexp 'x)))
;;;         (make-appexp 'f2 (list (make-varexp 'x) (make-varexp 'x))))))

(define (make-stress-input-without-loops n)
  (local (
          ;;; Returns a list of 1 through k.
          (define (iota k)
            (reverse (reverse-iota k)))
          (define (reverse-iota k)
            (if (= k 0)
                empty
                (cons k (reverse-iota (- k 1)))))

          ;;; Given the function names in reverse order,
          ;;; returns their bodies in reverse order.
          (define (make-bodies names)
            (if (empty? (rest names))
                (list (make-varexp 'x))
                (let* ((bodies (make-bodies (rest names)))
                       (body (first bodies))
                       (name (first (rest names))))
                  (cons (make-appexp name (list body body))
                        bodies)))))
    (let* ((nums (iota n))
           (syms (map (lambda (k)
                        (string->symbol (string-append "f"
                                                       (number->string k))))
                      nums))
           (bodies (reverse (make-bodies (reverse syms)))))
      (map (lambda (sym body)
             (make-def sym (list 'x 'y) body))
           syms
           bodies))))

;;; stress-benchmark1 : PosInt -> Boolean
;;; GIVEN: a positive integer n
;;; RETURNS: false
;;; EFFECT: reports how many milliseconds it takes to determine
;;;     (make-stress-input-without-loops n) has no loops

(define (stress-benchmark1 n)
  (time (any-loops? (make-stress-input-without-loops n))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 
;; For the second question:
 
;;; Note: The original version of this benchmark defined a function
;;; that didn't satisfy its contract.  That function
;;; (make-stress-input-unsat) was not needed
;;; by the benchmark and has been removed.  I have also added a call
;;; to make-clause.

;;; make-stress-input-sat : NonNegInt -> ListOfClause
;;; GIVEN: an integer n
;;; RETURNS: a satisfiable set of clauses of length n
;;; EXAMPLES:
;;;     (make-stress-input-sat 0) => empty
;;;     (make-stress-input-sat 3)
;;;  => (list (make-clause (list (make-pos 'p1)
;;;                              (make-neg 'p2)
;;;                              (make-neg 'p3)))
;;;           (make-clause (list (make-neg 'p1)
;;;                              (make-pos 'p2)
;;;                              (make-neg 'p3)))
;;;           (make-clause (list (make-neg 'p1)
;;;                              (make-neg 'p2)
;;;                              (make-pos 'p3))))

(define (make-stress-input-sat n)
  (local ((define (reverse-iota k)
            (if (= k 0)
                empty
                (cons k (reverse-iota (- k 1)))))
          (define (iota k)
            (reverse (reverse-iota k))))
    (let* ((nums (iota n))
           (syms (map (lambda (k)
                        (string->symbol (string-append "p"
                                                       (number->string k))))
                      nums)))
      (map (lambda (k)
             (make-clause   ; see note above
              (map (lambda (i)
                     ((if (= i k) make-pos make-neg)
                      (list-ref syms (- i 1))))
                   nums)))
           nums))))


;;; make-stress-input-unsat : PosInt -> ListOfClause
;;; GIVEN: an integer n
;;; RETURNS: an unsatisfiable set of clauses of length 2n

;;; EXAMPLES:
;;;     (make-stress-input-unsat 3)
;;;  => (list (make-clause (list (make-neg 'p1)
;;;                              (make-neg 'p2)
;;;                              (make-neg 'p3)))
;;;           (make-clause (list (make-pos 'p1)
;;;                              (make-neg 'p2)
;;;                              (make-neg 'p3)))
;;;           (make-clause (list (make-neg 'p1)
;;;                              (make-pos 'p2)
;;;                              (make-neg 'p3)))
;;;           (make-clause (list (make-neg 'p1)
;;;                              (make-neg 'p2)
;;;                              (make-pos 'p3))))

(define (make-stress-input-unsat n)
  (local ((define (reverse-iota k)
            (if (= k 0)
                empty
                (cons k (reverse-iota (- k 1)))))
          (define (iota k)
            (reverse (reverse-iota k))))
    (let* ((nums (iota n))
           (syms (map (lambda (k)
                        (string->symbol (string-append "p"
                                                       (number->string k))))
                      nums)))
      (cons (make-clause (list (make-neg (first syms))))
            (append
             (map (lambda (sym)
                    (make-clause (list (make-pos sym))))
                  (rest syms))
             (map (lambda (k)
                    (make-clause
                     (map (lambda (i)
                            ((if (= i k) make-pos make-neg)
                             (list-ref syms (- i 1))))
                          nums)))
                  nums))))))
 
;;; stress-benchmark2 : NonNegInt -> Boolean
;;; GIVEN: a non-negative integer n
;;; RETURNS: false
;;; EFFECT: reports how many milliseconds it takes to determine
;;;     (make-stress-input-sat n) is satisfiable

(define (stress-benchmark2 n)
  (time (is-null-derivable? (make-stress-input-sat n))))

;; Professor Wand wrote solutions for both questions, and I wrote a
;; solution for the second question. 
 
;; On my laptop, Professor Wand's solution runs (stress-benchmark1 14)
;; in less than a second, and runs (stress-benchmark1 20) in less than
;; a minute. 
 
;; For the second question, both of our solutions run
;; (stress-benchmark2 5) in less than a second, and run
;; (stress-benchmark2 10) in less than a minute. 
 
;; My program is almost twice as slow as Professor Wand's.  That's a
;; small constant factor, and we don't care about small constant
;; factors of efficiency.  If your solution is slower than ours by a
;; small constant factor, your solution is fast enough.  If your
;; solution is radically slower than ours, however, it probably isn't
;; fast enough. 
