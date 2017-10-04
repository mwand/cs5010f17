;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname 06-3-2-lasns) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(require rackunit)
(require "extras.rkt")

;; A different data definition that focuses on LASNs.  Here I'll call
;; them LASI's, so you won't get confused

;; DATA DEFINITIONS:

;; A LASI is one of:
;; -- empty
;; -- (cons String (cons Integer LASI))

;; Note that a LASI always has an even length.  This is not true for LASNs.

;; TEMPLATES:

#|
;; lasi-fn : LASI -> ??
;; HALTING MEASURE: length of l
(define (lasi-fn l)
  (cond
    [(empty? l) ...]
    [else (...
            (first lasi)
            (second lasi)
            (lasi-fn (rest (rest lasi))))]))
|#



;; right-lengths? : LASI -> Boolean
;; GIVEN: A Lasi
;; RETURNS: true iff the integer following each string is equal to the
;; length of the string.
;; STRATEGY: Use template for LASI on l
(define (right-lengths? l)
  (cond
    [(empty? l) true]
    [else (and
            (= (string-length (first l)) (second l))
            (right-lengths? (rest (rest l))))]))

(begin-for-test
  (check-equal?
    (right-lengths? empty)
    true)
  (check-equal?
    (right-lengths? (list "foo" 3 "april" 5 "george" 6))
    true)
  (check-equal?
    (right-lengths? (list "foo" 3 "april" 4 "george" 6))
    false)
  (check-error
    (right-lengths? (list 3 "april" 4 "george" 6))
    "this should raise an error, because the argument is not a lasi"))
