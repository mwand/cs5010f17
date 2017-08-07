;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname solution) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; Sos and Loss Practice

(require rackunit)
(require "extras.rkt")

;; Constructor Templates
;; 
;; An Sexp is either
;; -- a String (any string will do)
;; -- an SexpList
;; 
;; An SexpList is either
;; -- empty
;; -- (cons Sexp SexpList)
;; 
;; Sexps:
;; 
;; "abcd"
;; "def"
;; ("abcd" "def")
;; ("abcd" ("abcd" "def"))
;; ("abc" "def" "ghi")
;; (("abc" "def" "ghi")
;;  ("abcd" ("abcd" "def"))
;;  "def"
;;  ("abcd" "def"))
;; 
;; Observer Template:
;; 
;; sexp-fn  : Sexp -> ??
;; slist-fn : SexpList -> ??
;; 
;; (define (sexp-fn s)
;;   (cond
;;     [(string? s) ...]
;;     [else (... (slist-fn s))]))
;; 
;; (define (slist-fn sexps)
;;   (cond
;;     [(empty? sexps) ...]
;;     [else (... (sos-fn (first sexps))
;;                (slist-fn (rest sexps)))]))

;; characters-in-sexp  : Sexp -> Number
;; characters-in-sexps : SexpList -> Number
;; RETURNS: the total number of characters in the strings in the given
;; Sexp or SexpList.
;; EXAMPLES/TESTS: see below
;; STRATEGY: Use template for Sexp/SexpList

(define (characters-in-sexp s)
  (cond
    [(string? s) (string-length s)]
    [else (characters-in-sexps s)]))

(define (characters-in-sexps los)
  (cond
    [(empty? los) 0]
    [else (+ (characters-in-sexp  (first los))
             (characters-in-sexps (rest los)))]))

(begin-for-test
  (check-equal?
   (characters-in-sexp
    (list "alice" 
          (list (list "alice" "bob") "carole")
          "dave"))
   23)
  )

;; subst          : Sexp String String -> Sexp
;; subst-in-sexps : SexpList String String -> SexpList
;; GIVEN: a Sexp or SexpList and two strings, old and new
;; RETURNS: a Sexp or SexpList just like the given one, except that
;; all instances of old are changed to new.

;; EXAMPLE/TEST:
(begin-for-test
  (check-equal? 
    (subst
      (list "alice" 
        (list (list "alice" "bob") "dave") 
        "eve"
        "bob")
      "bob"
      "ted")
    (list "alice" 
      (list (list "alice" "ted") "dave") 
      "eve"
      "ted")))

;; STRATEGY: Use template for Sexp/SexpList

(define (subst s old new)
  (cond
    [(string? s) (subst-in-string s old new)]
    [else        (subst-in-sexps s old new)]))

(define (subst-in-sexps sexps old new)
  (cond
    [(empty? sexps) empty]
    [else (cons
            (subst (first sexps) old new)
            (subst-in-sexps (rest sexps) old new))]))

;; subst-in-string : String String String -> String
;; GIVEN: 3 strings, str, old, and new
;; RETURNS: new if str = old, otherwise returns str
;; strategy: function composition
(define (subst-in-string str old new)
  (if (string=? str old) new str))


