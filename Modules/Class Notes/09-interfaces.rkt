;; What do we mean by "the implementation of <Whatever> is up to you?

;; Generally, this means that you can represent the data any way you
;; like, so long as you provide the deliverable functions that we
;; list.

;; Generally, we will ask that you provide the smallest possible set
;; of functions that will enable us to test your solution.  But you
;; may want your implementation to provide additional functions that
;; you will need to solve the problem.

;; Let's look back at ps04, where we wrote:

#|
Here is the beginning of the data definition for ListOfSlip:

(define-struct slip (color name1 name2))
A Slip is a (make-slip Color String String)
|#

;; Here we've specified that a Slip is represented as a particular
;; struct.

;; But what if we hadn't written down the define-struct?  Then we
;; could have implemented the slip in many ways.

;; So let's that we had written the following instead:

;; ================================================================

;; A Slip is a (make-slip Color String String)

;; interface:

;; make-slip : Color String String -> Slip

;; ================================================================

;; However you will probably want your implementation of slips to
;; provide the following additional functions:

;; slip-color : Slip -> Color
;; slip-name1 : Slip -> String
;; slip-name2 : Slip -> String
;; slip-equal? : Slip Slip -> Boolean

;;; ================================================================

;; What are some of the possible implementations you could have
;; written?

;; To make the distinction between the implementation and the rest of
;; your code ("the client") clearer, we'll imagine writing the
;; implementation in a separate file, which you'll 'require' into your
;; program. 

;;; ================================================================

;; implementation1.rkt:

;; implementation using structs (the original)

(define-struct slip (color name1 name2))

;; this could be either in the implementation or in the client code,
;; but we've specified that it be provided by the implementation.

(define (slip-equal? s1 s2)
  (and
   (equal? (slip-color s1) (slip-color s2))
   (or
    (and 
     (equal? (slip-name1 s1) (slip-name1 s2))
     (equal? (slip-name2 s1) (slip-name2 s2)))
    (and 
     (equal? (slip-name1 s1) (slip-name2 s2))
     (equal? (slip-name2 s1) (slip-name1 s2))))))

(provide make-slip slip-color slip-name1 slip-name2 slip-equal?)

;; you could also have written

(provide (struct-out slip) slip-equal?)

;;; ================================================================

;; implementation2.rkt.

;; Here a student was confused about slip vs. Slip.
;; This also illustrates that the field names are arbitrary.

;; This is a poorly designed implementation.  I would give it no more
;; than a B.


(define-struct Slip (field1 field2 field3))
;; are we confused about slip vs. Slip yet?  

(define (make-slip c n1 n2) (make-Slip c n1 n2))
;; or, more concisely,
(define make-slip make-Slip)

;; and similarly
(define slip-color Slip-field1)
(define slip-name1 Slip-field2)
(define slip-name2 Slip-field3)

;; better to use slip-color than to use Slip-field1.
;; code will be more readable that way.
;; also you might change your mind about this very poorly designed
;; implementation. 
(define (slip-equal? s1 s2) ...)

(provide make-slip slip-color slip-name1 slip-name2 slip-equal?)

;;; ================================================================

;; implementation3.rkt : representing slips as lists

;; This is an acceptable implementation.  But if you have anything
;; more complicated than this and just use lists, rather than structs,
;; you will find that your program is undebuggable because you'll have
;; great difficulty reading the data produced by check-equal? and
;; friends.  (I've learned this the hard way!)

;; internal representation:
;; A Slip is a (list Color String String)

(define (make-slip c n1 n2) (list c n1 n2))
(define slip-color first)
(define slip-name1 second)
(define slip-name2 third)

(define (slip-equal? s1 s2) ,,,)

(provide make-slip slip-color slip-name1 slip-name2 slip-equal?)

;;; ================================================================

;; implementation4.rkt : Using OO.

;; This is also probably a poor choice of design, since we are only
;; using this object as a struct.  (I give it a B also).

;; A Slip is an object of any class that implments the Slip<%> interface

(define Slip<%>
  (interface ()

    ;; -> Color
    get-color

    ;; -> String
    get-name1
    get-name2

    ))

;; Slip1% is one possible implementation of the Slip<%> interface

;; Constructor template for Slip1%:
;;  (new Slip1% [color Color][name1 String][name2 String]

(define Slip1%
  (class* object% (Slip<%>)

    (init-field color)  ; Color
    (init-field name1 name2) ; String

    ;; -> Color
    (define/public (get-color) color)

    ;; ..etc..
    ))

;; Color String String -> Slip
(define (make-slip c n1 n2)
  (new Slip1% [color c][name1 n1][name2 n2]))

;; Slip -> Color
(define (slip-color s)     ; the function named slip-color
  (send s get-color))

;; ...etc...

(provide make-slip slip-color slip-name1 slip-name2 slip-equal?)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




