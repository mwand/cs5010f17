;; solutions to Guided Practice 6.1: pizza problem

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; QUESTION 1
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; A Topping is represented as a String (any string will do)

;; A Pizza is represented as list of Topping, listed from top to bottom.

;; pizza-fn : Pizza -> ??
;; Given a Pizza, produce ....
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
;; STRATEGY: Use observer template for Pizza on p

(define (replace-all-anchovies-with-onions p)
  (cond
    [(empty? p) empty]
    [else (if (string=? (first p) "anchovies")
            (cons "onions"
                  (replace-all-anchovies-with-onions 
                   (rest p)))
            (cons (first p)
                  (replace-all-anchovies-with-onions 
                   (rest p))))]))
    


;; replace-all-anchovies : Pizza Topping -> Pizza
;; RETURNS: a pizza like the given pizza, but with 
;; all anchovies replaced by the given topping.
;; STRATEGY: Use template for Pizza on p

(define (replace-all-anchovies p replacement)
  (cond
    [(empty? p) empty]
    [else (if (string=? (first p) "anchovies")
            (cons replacement
              (replace-all-anchovies
                (rest p)
                replacement))
            (cons (first p)
              (replace-all-anchovies 
               (rest p)
               replacement)))]))


;; replace-topping : Pizza Topping Topping -> Pizza
;; RETURNS: a pizza like the given one, but with 
;; all instances of the first topping replaced by
;; the second one.
;; STRATEGY: Use template for Pizza on p

(define (replace-topping p topping replacement)
  (cond
    [(empty? p) empty]
    [else (if (string=? (first p) topping)
            (cons replacement
              (replace-topping 
               (rest p)
               topping
               replacement))
            (cons (first p)
              (replace-topping 
               (rest p)
               topping
               replacement)))]))

;; another solution (also Use template for Pizza on p):

(define (replace-topping p topping replacement)
  (cond
    [(empty? p) empty]
    [else (cons
            (if (string=? (first p) topping)
              replacement
              (first p))
            (replace-topping 
               (rest p)
               topping
               replacement))]))


