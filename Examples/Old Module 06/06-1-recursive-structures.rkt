(define-struct plain-pizza ())
(define-struct topped-pizza (topping base))

;; A Topping is a String.
;; A Pizza is either
;; -- (make-plain-pizza)
;; -- (make-topped-pizza Topping Pizza)

;; Interp:
;; (make-plain-pizza)      represents a pizza with no toppings
;; (make-topped-pizza t p) represents the pizza p with topping t added on top.


#|
pizza-fn : Pizza -> ??
Halting Measure: number of toppings
(define (pizza-fn p)
  (cond
    [(plain-pizza? p) ...]
    [else (... (topped-pizza-topping p)
               (pizza-fn 
                 (topped-pizza-base p)))]))
|#
