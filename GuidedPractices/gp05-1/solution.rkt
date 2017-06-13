;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname 05.1-pizza) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;; solutions to Guided Practice 5.1: pizza problem

;; Data Definitions:

;; A Topping is a String.

;; A Pizza is a ListOfTopping
;; interp: a pizza is a list of toppings, listed from top to bottom

;; pizza-fn : Pizza -> ??
; Given a Pizza, produce ....
;; (define (pizza-fn p)
;;   (cond
;;     [(empty? p) ...]
;;     [else (... (first p)
;;             (pizza-fn (rest p)))]))

;; Examples:
(define plain-pizza empty)
(define cheese-pizza (list "cheese"))
(define anchovies-cheese-pizza (list "anchovies" "cheese"))

;;; This is the original function definition, from the slides:

;; replace-all-anchovies-with-onions 
;;   : Pizza -> Pizza
;; RETURNS: a pizza like the given pizza, but with
;; anchovies in place of each layer of onions
;; STRATEGY: Use template for Pizza on p
(define (replace-all-anchovies-with-onions p)
  (cond
    [(empty? p) empty]
    [else (if (string=? (first p) "anchovies")
            (cons "onions"
              (replace-all-anchovies-with-onions (rest p)))
            (cons 
             (first p)
             (replace-all-anchovies-with-onions (rest p))))]))


;; replace-all-anchovies : Pizza Topping -> Pizza
;; RETURNS: a pizza like the given pizza, but with 
;; all anchovies replaced by the given topping.
;; STRATEGY: Use template for Pizza on p
(define (replace-all-anchovies p replacement)
  (cond
    [(empty? p) empty]
    [else (if (string=? (first p) "anchovies")
            (cons 
             replacement
             (replace-all-anchovies (rest p) replacement))
            (cons 
             (first p)
             (replace-all-anchovies (rest p) replacement)))]))


;; replace-topping : Pizza Topping Topping -> Pizza
;; RETURNS: a pizza like the given one, but with 
;; all instances of the first topping replaced by
;; the second one.
;; STRATEGY: Use template for Pizza on p
(define (replace-topping p topping replacement)
  (cond
    [(empty? p) empty]
    [else (if (string=? (first p) topping)
            (cons 
             replacement
             (replace-topping (rest p) topping replacement))
            (cons 
             (first p)
             (replace-topping (rest p) topping replacement)))]))




