;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname 05-3-sexps) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require rackunit)
(require "extras.rkt") 

;; S-expressions

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
;; sexp-fn : Sexp -> ??
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
;; 

;; occurs-in? : Sexp String -> Boolean
;; RETURNS: true if the given string occurs somewhere in the given Sexp.
;; occurs-in-loss? : Loss String -> Boolean
;; RETURNS: true if the given string occurs somewhere in the given
;; list of Sexps.
;; STRATEGY: Use templates for Sexp and SexpList

(define (occurs-in? sexp str) 
   (cond
     [(string? sexp) (string=? sexp str)]
     [else (occurs-in-slist? sexp str)]))

(define (occurs-in-slist? sexps str) 
   (cond
     [(empty? sexps) false]
     [else (or (occurs-in? (first sexps) str)
               (occurs-in-slist? (rest sexps) str))]))

(begin-for-test

  (check-equal? (occurs-in? "alice" "alice") true)
  (check-equal? (occurs-in? "bob"  "alice") false)
  
  (check-equal? 
   (occurs-in? (list "alice" "bob") "cathy") 
   false)
  
  (check-equal? 
   (occurs-in? 
    (list
     (list "alice" "bob") 
     "carole") 
    "bob") 
   true)
  
  (check-equal? 
   (occurs-in? 
    (list "alice" 
          (list (list "alice" "bob") "dave") 
          "eve")
    "bob")
   true))

;; number-of-strings : Sexp -> Number
;; number-of-strings-in-slist : SexpList -> Number
;; RETURNS: the number of strings in the given Sexp or SexpList
;; STRATEGY: Use templates for Sexp and SexpList

(define (number-of-strings s) 
  (cond
    [(string? s) 1]
    [else (number-of-strings-in-slist s)]))
  
(define (number-of-strings-in-slist sexps) 
  (cond
    [(empty? sexps) 0]
    [else (+ (number-of-strings (first sexps))
             (number-of-strings-in-slist (rest sexps)))]))

(begin-for-test
  
  (check-equal? 
   (number-of-strings
    (list "alice" 
          (list (list "alice" "bob") "dave") 
          "eve"
          "bob"))
   6)
  )

