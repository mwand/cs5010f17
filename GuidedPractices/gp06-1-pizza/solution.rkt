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
    
;; replace-all-anchovies-with-onions 
;;   : Pizza -> Pizza
;; RETURNS: a pizza like the given pizza, but with
;; anchovies in place of each layer of onions
;; STRATEGY: Use template for Pizza on p

(define (replace-all-anchovies-with-onions p)
  (cond
    [(empty? p) empty]
    [else (if (string=? (topped-pizza-topping p) "anchovies")
            (cons "onions"
              (replace-all-anchovies-with-onions 
               (topped-pizza-base p)))
            (cons (topped-pizza-topping p)
              (replace-all-anchovies-with-onions 
               (topped-pizza-base p))))]))


;; replace-all-anchovies : Pizza Topping -> Pizza
;; RETURNS: a pizza like the given pizza, but with 
;; all anchovies replaced by the given topping.
;; STRATEGY: Use template for Pizza on p

(define (replace-all-anchovies p replacement)
  (cond
    [(empty? p) empty]
    [else (if (string=? (topped-pizza-topping p) "anchovies")
            (cons replacement
              (replace-all-anchovies
                (topped-pizza-base p)
                replacement))
            (cons (topped-pizza-topping p)
              (replace-all-anchovies 
               (topped-pizza-base p)
               replacement)))]))


;; replace-topping : Pizza Topping Topping -> Pizza
;; RETURNS: a pizza like the given one, but with 
;; all instances of the first topping replaced by
;; the second one.
;; STRATEGY: Use template for Pizza on p

(define (replace-topping p topping replacement)
  (cond
    [(empty? p) empty]
    [else (if (string=? (topped-pizza-topping p) topping)
            (cons replacement
              (replace-topping 
               (topped-pizza-base p)
               topping
               replacement))
            (cons (topped-pizza-topping p)
              (replace-topping 
               (topped-pizza-base p)
               topping
               replacement)))]))

;; another solution (also Use template for Pizza on p):

(define (replace-topping p topping replacement)
  (cond
    [(empty? p) empty]
    [else (cons
            (if (string=? (topped-pizza-topping p) topping)
              replacement
              (topped-pizza-topping p))
            (replace-topping 
               (topped-pizza-base p)
               topping
               replacement))]))


