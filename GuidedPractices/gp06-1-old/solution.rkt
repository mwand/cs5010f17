;; solutions to Guided Practice 6.1: pizza problem

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; QUESTION 1
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-struct plain-pizza ())
(define-struct topped-pizza (topping base))

;; A Topping is a String.

;; A Pizza is either
;; -- (make-plain-pizza)
;; -- (make-topped-pizza Topping Pizza)
;; INTERP:
;; (make-plain-pizza)      represents a pizza with no toppings
;; (make-topped-pizza t p) represents the pizza p with topping t added
;;                         on top.
    
;; replace-all-anchovies-with-onions 
;;   : Pizza -> Pizza
;; RETURNS: a pizza like the given pizza, but with
;; anchovies in place of each layer of onions
;; STRATEGY: Use template for Pizza on p

(define (replace-all-anchovies-with-onions p)
  (cond
    [(plain-pizza? p) empty]
    [else (if (string=? (topped-pizza-topping p) "anchovies")
            (make-topped-pizza "onions"
              (replace-all-anchovies-with-onions 
               (topped-pizza-base p)))
            (make-topped-pizza (topped-pizza-topping p)
              (replace-all-anchovies-with-onions 
               (topped-pizza-base p))))]))


;; replace-all-anchovies : Pizza Topping -> Pizza
;; RETURNS: a pizza like the given pizza, but with 
;; all anchovies replaced by the given topping.
;; STRATEGY: Use template for Pizza on p

(define (replace-all-anchovies p replacement)
  (cond
    [(plain-pizza? p) empty]
    [else (if (string=? (topped-pizza-topping p) "anchovies")
            (make-topped-pizza replacement
              (replace-all-anchovies
                (topped-pizza-base p)
                replacement))
            (make-topped-pizza (topped-pizza-topping p)
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
    [(plain-pizza? p) empty]
    [else (if (string=? (topped-pizza-topping p) topping)
            (make-topped-pizza replacement
              (replace-topping 
               (topped-pizza-base p)
               topping
               replacement))
            (make-topped-pizza (topped-pizza-topping p)
              (replace-topping 
               (topped-pizza-base p)
               topping
               replacement)))]))

;; another solution (also Use template for Pizza on p):

(define (replace-topping p topping replacement)
  (cond
    [(plain-pizza? p) empty]
    [else (make-topped-pizza
            (if (string=? (topped-pizza-topping p) topping)
              replacement
              (topped-pizza-topping p))
            (replace-topping 
               (topped-pizza-base p)
               topping
               replacement))]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; QUESTION 2
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Your boss says you need to represent pizzas using this struct:

(define-struct shazam (topping base))

;; what you DON'T do is to go through your codebase changing all instances of 
;; make-topped-pizza to make-shazam, etc, etc.

;; What you DO say is

(define (make-topped-pizza t p) (make-shazam p t))
(define (make-plain-pizza) "kaphlooey")
(define (plain-pizza? p) (equal? p "kaphlooey"))
(define (topped-pizza-topping p) (shazam-topping p))
(define (topped-pizza base p) (shazam-base p))

;; now all your code still works!

;; this is an example of "coding to an interface".  So long as all
;; your code manipulates pizza using these 4 functions, then you can
;; implement those functions any way you want, and your code will
;; still work!

;; Sometimes this is called the "adapter" or "facade" pattern.  A
;; fancier term, much beloved by professors, is "abstraction
;; boundary". 

;; We can use this technique to divide a system into layers.
;; Conventionally, we think of the "server" as the lower layer and the
;; "client" as the upper layer.  The client doesn't care how the
;; server is implemented: so long as the client manipulates the data
;; only through the server, it doesn't matter how the server does its
;; job. 

;; Here, your hundreds of functions that manipulate pizzas are the
;; client, and all you've done is change the implementation of the
;; server. 

;; Another term you'll see is "information-hiding": the details of the
;; server implementation are hidden from the client.

;; No matter what you call it, this technique is the programmer's most
;; important tool for controlling complexity, because it enables him
;; or her to ignore all kinds of details.   Large portions of the
;; development of programming languages has been devoted to developing
;; ways to enforce abstraction boundaries:  that is, to prevent the
;; programmer from writing programs that inadvertently depend on
;; details below the interface/boundary/whatever.


