;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname 06-5-sos-and-loss) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(require rackunit)
(require "extras.rkt") 

;; S-expressions
;; 
;; An Sexp-of-String (SoS) is either
;; -- a String
;; -- a List-of SoS
;; 
;; A List-of SoS (LoSS) is either
;; -- empty
;; -- (cons SoS LoSS)
;; 
;; SoS's:
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
;; Template:
;; 
;; sos-fn : SoS -> ??
;; loss-fn : LoSS -> ??
;; 
;; (define (sos-fn sos)
;;   (cond
;;     [(string? sos) ...]
;;     [else (... (loss-fn sos))]))
;; 
;; (define (loss-fn loss)
;;   (cond
;;     [(empty? loss) ...]
;;     [else (...   (sos-fn (first loss))
;;                  (loss-fn (rest loss)) ...)]))
;; 

;; occurs-in? : Sos String -> Boolean
;; RETURNS: true if the given string occurs somewhere in the given sos.
;; occurs-in-loss? : Loss String -> Boolean
;; RETURNS: true if the given string occurs somewhere in the given loss.
;; STRATEGY: Use templates for SoS and LoSS

(define (occurs-in? sos str) 
   (cond
     [(string? sos) (string=? sos str)]
     [else (occurs-in-loss? sos str)]))

(define (occurs-in-loss? loss str) 
   (cond
     [(empty? loss) false]
     [else (or (occurs-in? (first loss) str)
               (occurs-in-loss? (rest loss) str))]))

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

;; number-of-strings : Sos -> Number
;; number-of-strings-in-loss : Loss -> Number
;; RETURNS: the number of strings in the given sos or loss.
;; STRATEGY: Use templates for SoS and LoSS

(define (number-of-strings sos) 
  (cond
    [(string? sos) 1]
    [else (number-of-strings-in-loss sos)]))
  
(define (number-of-strings-in-loss loss) 
  (cond
    [(empty? loss) 0]
    [else (+ (number-of-strings (first loss))
             (number-of-strings-in-loss (rest loss)))]))

(begin-for-test
  
  (check-equal? 
   (number-of-strings
    (list "alice" 
          (list (list "alice" "bob") "dave") 
          "eve"
          "bob"))
   6)
  )

