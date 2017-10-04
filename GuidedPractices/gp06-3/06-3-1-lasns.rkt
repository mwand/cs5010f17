;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname 06-4-lasns) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(require rackunit)
(require "extras.rkt")

;; DATA DEFINITIONS:

;; A ListOfAlternatingNumbersAndStrings (LANS) is one of:
;; -- empty
;; -- (cons Number LASN)

;; A ListOfAlternatingStringsAndNumbers (LASN) is one of:
;; -- empty
;; -- (cons String LANS)

;; TEMPLATES:

;; lans-fn : LANS -> ??
;; (define (lans-fn lans)
;;   (cond
;;     [(empty? lans) ...]
;;     [else (...
;;             (first lans)
;;             (lasn-fn (rest lans)))]))

;; ;; lasn-fn : LASN -> ??
;; (define (lasn-fn lasn)
;;   (cond
;;     [(empty? lasn) ...]
;;     [else (... 
;;             (first lasn)
;;             (lans-fn (rest lasn)))]))

;; lasn-number-of-strings: LASN -> Number
;; lans-number-of-strings : LANS -> Number
;; RETURNS: the number of strings in the given lasn or lans
;; EXAMPLE: (cons "bar" (cons 23 (cons "foo" (cons 11 empty)))) => 2
;; STRATEGY: Use template for LASN/LANS
(define (lasn-number-of-strings lasn)
  (cond
    [(empty? lasn) 0]
    [else (+ 1
            (lans-number-of-strings (rest lasn)))]))
(define (lans-number-of-strings lans)
  (cond
    [(empty? lans) 0]
    [else (lasn-number-of-strings (rest lans))]))

(begin-for-test
  (check-equal?
    (lasn-number-of-strings
      (cons "bar" (cons 23 (cons "foo" (cons 11 empty)))))
    2)
  (check-equal?
    (lans-number-of-strings 
      (cons 23 (cons "foo" (cons 11 empty))))
    1)
  (check-equal?
   (lasn-number-of-strings
    (cons "foo" (cons 11 (cons "bar" empty))))
   2)
  )
