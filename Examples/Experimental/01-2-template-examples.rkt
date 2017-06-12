;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname 01-2-template-examples) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require rackunit)
(require "extras.rkt")


;;; New-Style Data Definitions

;; In the new style, a Data Definition is a link between a piece of
;; information (something in the real world), and a piece of data
;; (something in the computer).

;; To emphasize this, we add the words "is represented as" in every
;; data definition.

;; In the new style, we will abandon both constructor templates and
;; observer templates.  Instead we will concentrate on a picture of
;; the data, as it might appear in the repl.

;; For structs, we put the field names in the representation and the
;; types in the interpretations.

;; We prefer that the types of the fields be given in terms of the
;; information they represent, rather than in terms of how that
;; information is represented.  However, this is a flexible criterion.

;; In the examples below, sections labeled "NOTE" are intended as
;; commentary, and not as something a student would be asked to write.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; A CoffeeOrder is represented as (coffee-order size type milk?)
;; with the following fields:
;; INTERP:
;;   size : Size           is the size of cup desired
;;   type : CoffeeType     is the kind of coffee order
;;   milk : MilkType       is the kind of milk ordered

(define-struct coffee-order (size type milk))

;; NOTE: As per Will's advice, we'll have constructor and observer
;; templates as deliverables, but we can drop them later on when
;; everybody gets it.

;; CONSTRUCTOR TEMPLATE

;; (make-coffee-order Size CoffeeType MilkType)

;; OBSERVER TEMPLATE

;; coffee-order-fn : CoffeeOrder -> ??
#;
(define (coffee-order-fn co)
  (...
   (coffee-order-size co)
   (coffee-order-type co)
   (coffee-order-milk co)))
   

;; NOTE: (coffee-order ...) in the first line is NOT a constructor.
;; It is a picture of the data as it might be printed out.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; A Size is represented as one of the following integers:
;; -- 8, 12, 16, 20, 30
;; INTERP: the size of the cup, in fluid ounces

;; NOTE: Constructor template is not necessary for itemization data

;; OBSERVER TEMPLATE:

;; size-fn : Size -> ?
#;
(define (size-fn s)
  (cond
    [(= s 8)  ...]
    [(= s 12) ...]
    [(= s 16) ...]
    [(= s 20) ...]
    [(= s 30) ...]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; A CoffeeType is represented as a string  (any string will do)

;; NOTE: In a more detailed representation, we might specify this further.

;; NOTE: Any time you write String, you MUST write "any string will
;; do".   This should eliminate definitions of the form "A Direction
;; is a String".  Possible similarly for Number (?).

;; A MilkType is one of
;; -- "black"
;; -- "skim"
;; -- "whole"
;; -- "soy"

;; CONSTRUCTOR TEMPLATE: Not needed for an itemization type

;; OBSERVER TEMPLATE:

;; milk-fn : MilkType -> ?
#;
(define (milk-fn m)
  (cond
    [(string=? m "black") ...]
    [(string=? m "skim")  ...]
    [(string=? m "whole") ...]
    [(string=? m "soy")   ...]))

;; NOTE: we don't need interpretations for CoffeeType or MilkType
;; because we believe the reader can figure this out.  If we were
;; doing this in some other language, or if we believe the reader
;; would not understand these terms well enough to write the code,
;; then we would have to have a more elaborate interpretation.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; A WineOrder is represented as (wine-order vineyard vintage)
;; with the following fields:
;; INTERP:
;; vineyard : Vineyard    the origin of the grapes
;; vintage  : Vintage     the vintage of the grapes

;; A Vineyard is represented as a String (any string will do)
;; A Vintage  is represented as a positive integer in [1800,2100]

;; NOTE: it would be OK to write either or both of the following:

;; vineyard : the origin of the grapes, represented as a string (any
;;            string will do)
;; vintage : PosInt[1800,2100]   the vintage of the grapes

(define-struct wine-order (vineyard vintage))

;; CONSTRUCTOR TEMPLATE: (make-wine-order Vineyard Vintage)

;; OBSERVER TEMPLATE:

;; wine-order-fn : WineOrder -> ??
#;
(define (wine-order-fn wo)
  (...
   (wine-order-vineyard wo)
   (wine-order-vintage  wo)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; A TeaOrder is represented as (tea-order size type)
;; INTERP:
;; size : Size     the size of cup desired
;; type : TeaType  the type of tea to be used

(define-struct tea-order (size type))

;; A TeaType is represented as a String (any string will do)

;; NOTE: similarly, it would be OK to write
;; type : String   the type of tea to be used, represented as a string
;;                 (any string will do)
;; It would NOT be OK to write
;; size : NonNegInt   the size of the cup desired, in fluid ounces
;; because the size is supposed to be one of the 5 possibilities
;; above.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; A BarOrder is one of
;; -- a CoffeeOrder
;; -- a WineOrder
;; -- a TeaOrder

;; bo-fn : BarOrder -> ??
;; STRATEGY: Cases on order : BarOrder
(define (bo-fn order)
  (cond
    [(coffee-order? order) ...]
    [(wine-order?   order) ...]
    [(tea-order?    order) ...]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Another example:

(define-struct book (author title on-hand price))

;; A Book is represented as a (book-record title on-hand price)
;; INTERP:
;; title   : Title      is the title of the book
;; author  : Author     is the author of the book
;; on-hand : NonNegInt  is the number of copies on hand
;; price   : Price      is the price of the book

;; A Price is a NonNegInt
;; INTERP: a price in USD*100
;; EXAMPLE: 795 represents the price $7.95

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Example template for itemization data

;; A Size is one of
;; -- "small"
;; -- "medium"
;; -- "large"

;; size-fn : Size -> ??
(define (size-fn s)
  (cond
    [(string=? s "small") ...]
    [(string=? s "medium") ...]
    [(string=? s "large") ...]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


