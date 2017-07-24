;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname 01-3-3-itemization-data) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; A Size is represented as one of the following integers:
;; -- 8, 12, 16, 20, 30
;; INTERP: a cup size, in fluid ounces

;; NOTE: it would be wrong to say "the cup", since there is no cup
;; here.  Look at the definition of CoffeeOrder below


;; NOTE: Constructor template is not necessary for itemization data


;; OBSERVER TEMPLATE:

;; size-fn : Size -> ?

(define (size-fn s)
  (cond
    [(= s 8)  ...]
    [(= s 12) ...]
    [(= s 16) ...]
    [(= s 20) ...]
    [(= s 30) ...]))