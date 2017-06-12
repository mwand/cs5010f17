#lang racket  ; was #lang racket/base
(require (for-syntax racket/base)
         racket/path
         rackunit
         rackunit/log)
(provide begin-for-test)
(provide provide rename-out struct-out check-error)
(provide check-location)
(provide check-within)

(define extras-version "Wed Sep 14 08:52:19 2016")

(printf "extras.rkt ~a~n" extras-version)

(define-syntax (check-error stx)
  (syntax-case stx ()
    [(_ code . msg) #`(check-exn exn:fail? (lambda () code) . msg)]))

(define-syntax (check-within stx)
  (syntax-case stx ()
    [(_ val1 val2 tol . msg)
;    #`(check-true (<= (abs (- val1 val2)) tol) .  msg)
     #`(check <= (abs (- val1 val2)) tol)]))



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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; check-location : String String -> Void
;; GIVEN: a 2 digit problem set number NN and the name of the file
;; being qualified.
;; EFFECT: throws an error if this file is not a directory of the form
;; pdp-*./setNN 
(define (check-location NN correct-file-name)
  (define path-elements (explode-path (current-directory)))
  (define path-len (length path-elements))
  (define correct-folder-name (string-append "set" NN))
  (cond
    [(>= path-len 2) 
     (define set-folder (path->string (last path-elements)))
     (define pdp-folder (path->string (list-ref path-elements (- path-len 2))))
     (define set-regexp (regexp correct-folder-name))
     (define pdp-regexp (regexp "pdp-.*"))
     (match* ((regexp-match? set-regexp set-folder)
              (regexp-match? pdp-regexp pdp-folder))
       [(_ #f)
        (error
         (format
         "File is in folder \"~a/~a\", which does not appear to be a local repo"
         pdp-folder set-folder))]
       [(#f _)
        (error 
         (format 
          "File should be in a folder named ~a, but is in a folder named ~a" 
          correct-folder-name
          set-folder))]
       [(#t #t)
        (printf
          "~a appears to be in a correctly named folder. Running tests...~n"
          correct-file-name)]
       [(_ _) (void)])]
    [else
     (error
      (format
       "File should be in a folder named pdp-*/~a"
       correct-folder-name))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; for Problem Set 6, render-expr problem

;; ListOf<String> -> Void
;; GIVEN: A List of Strings
;; EFFECT: displays the strings in separate lines
;; RETURNS: the empty string
(define (display-strings! strs)
  (if (empty? strs) (void)
      (let* ((d1 (display (first strs)))
             (d2 (display "\n")))
        (display-strings! (rest strs)))))

(provide display-strings!)


