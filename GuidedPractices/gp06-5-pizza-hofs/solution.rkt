;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; replace-topping : Pizza Topping Topping -> Pizza
;; RETURNS: a pizza like the given one, but with 
;; all instances of the first topping replaced by
;; the second one.
;; STRATEGY: Use HOF map on p
(define (replace-topping p topping replacement)
  (map
    (lambda (this-topping)
      (if (string=? this-topping topping)
        replacement
        this-topping))
    p))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

