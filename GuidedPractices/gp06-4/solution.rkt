;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname 06-5-sos-fns) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t write repeating-decimal #f #t none #f ())))
;; Sos and Loss Practice

(require rackunit)
(require "extras.rkt")

;; Data Definitions

;; An S-expression of Strings (SoS) is either
;; -- a String
;; -- a List of SoS's (LoSS)

;; A List of SoS's (LoSS) is either
;; -- empty
;; -- (cons SoS LoSS)

;; TEMPLATES
;; ;; sos-fn : SoS -> ??
;; (define (sos-fn s)
;;   (cond
;;     [(string? s) ...]
;;     [else (... (loss-fn s)]))

;; ;; loss-fn : LoSS -> ??
;; (define (loss-fn los)
;;   (cond
;;     [(empty? los) ...]
;;     [else (... (sos-fn (first los))
;;                (loss-fn (rest los)))]))

;; characters-in : Sos -> Number
;; characters-in-loss : LoSS -> Number
;; RETURNS: the total number of characters in the strings in the 
;; given sos or loss.
;; EXAMPLES/TESTS: see below
;; STRATEGY: Use template for SoS/LoSS

(define (characters-in s)
  (cond
    [(string? s) (string-length s)]
    [else (characters-in-loss s)]))

(define (characters-in-loss los)
  (cond
    [(empty? los) 0]
    [else (+ (characters-in (first los))
             (characters-in-loss (rest los)))]))

(begin-for-test
  (check-equal?
   (characters-in
    (list "alice" 
          (list (list "alice" "bob") "carole")
          "dave"))
   23)
  )

;; subst : SoS String String -> SoS
;; subst-in-loss : LoSS String String -> LoSS
;; GIVEN: a SoS or LoSS and two strings, old and new
;; RETURNS: a Sos or Loss just like the given one, except that
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

;; STRATEGY: Use template for SoS/LoSS 

(define (subst sos old new)
  (cond
    [(string? sos) (subst-in-string sos old new)]
    [else (subst-in-loss sos old new)]))

(define (subst-in-loss loss old new)
  (cond
    [(empty? loss) empty]
    [else (cons
            (subst (first loss) old new)
            (subst-in-loss (rest loss) old new))]))

;; subst-in-string : String String String -> String
;; GIVEN: 3 strings, str, old, and new
;; RETURNS: new if str = old, otherwise returns str
;; strategy: function composition
(define (subst-in-string str old new)
  (if (string=? str old) new str))


