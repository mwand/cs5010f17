#lang racket  ; was #lang racket/base
(require (for-syntax racket/base)
         racket/path
         rackunit
         rackunit/log)
(provide begin-for-test)
(provide provide rename-out struct-out check-error)



(define-syntax (check-error stx)
  (syntax-case stx ()
    [(_ code . msg) #`(check-exn exn:fail? (lambda () code) . msg)]))



;; tests : (Parameterof (Listof (-> Void)))
;; Testing thunks in reverse order.
(define tests (make-parameter null))

(begin-for-syntax
 ;; lifted-run-expr? : Boolean (mutable)
 ;; True iff we've already appended a (run-all-tests) expr to the end
 ;; of the current module.
 (define lifted-run-expr? #f))

;; (begin-test expr ...) : module-level form
(define-syntax (begin-for-test stx)
  (syntax-case stx ()
    [(_ expr ...)
     (case (syntax-local-context)
       [(module)
        ;; In module (eg, defns buffer), add (run-all-tests) expr to end
        ;; of module, unless previously added.
        (unless lifted-run-expr?
          (syntax-local-lift-module-end-declaration
           #`(run-all-tests (quote #,(syntax-source stx))))
          (set! lifted-run-expr? #t))
        #'(tests (cons (lambda () (#%expression expr) ... (void)) (tests)))]
       [(top-level)
        ;; At top level (eg, interactions buffer), just run tests immediately.
        ;; Alternative: disallow, like next case (but need different phrasing)
        #'(begin (#%expression expr) ...)]
       [else
        (raise-syntax-error #f
          "found a use of `begin-test' that is not at the top level"
          stx)])]))

;; run-all-tests : Any -> Void
;; Runs thunks in (tests) and clears (tests).
(define (run-all-tests src)
  (define filename (path->string
                    (find-relative-path (current-directory) src
                                        #:more-than-root? #t)))
  (define ts (reverse (tests)))
  (define msg
    (cond [(path? src)
           (format "Running tests from ~a...~n"
                   filename)]
          [else "Running tests...~n"]))
  (tests null)
  (display msg)
  (define start-failed+total (test-log))
  (define start-failed (car start-failed+total))
  (define start-total  (cdr start-failed+total))
  (for ([t (in-list ts)]) (t))
  (define end-failed+total (test-log))
  (define failed (- (car end-failed+total) start-failed))
  (define total  (- (cdr end-failed+total) start-total))
  (define (n-tests n) (format "~s ~a" n (if (= n 1) "test" "tests")))
  (cond [(zero? total)
         (printf "There were no tests.\n")]
        [(zero? failed)
         (printf "All tests passed (~a).\n" (n-tests total))]
        [else
         (printf "~a failed out of ~a total in ~a.\n"
           (n-tests failed)
           (n-tests total)
           filename)]))
