#|

<li>You've been working at the pizza shop for a few months now, and
you've written hundreds of functions manipulating pizzas.  Your boss
has now come under the spell of an evil wizard, who has convinced him
that his business will be much better if he changes the data
definition for pizza to the following:

<pre>
;; A Topping is a String.
;; A Pizza is either
;; -- the string "kaphlooey"
;; -- (make-shazam Pizza Topping)

;; Interp:
;; "kaphlooey"        represents a pizza with no toppings
;; (make-shazam p t)  represents the pizza p with topping t added on top.
</pre>

<p>What do you do to satisfy your insane boss?</p></li>
|#


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


