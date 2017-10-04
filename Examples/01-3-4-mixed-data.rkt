;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname 01-3-4-mixed-data) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; examples of mixed data

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Definition of Size (an itemization type)

;; A Size is represented as one of the following integers:
;; -- 8, 12, 16, 20, 30
;; INTERP: the size of the cup, in fluid ounces

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

;; NOTE: Any time you write String, you MUST write "any string will
;; do".

;; NOTE: In a more detailed representation, we might specify this further.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; A MilkType is one of
;; -- "black"
;; -- "skim"
;; -- "whole"
;; -- "soy"

;; CONSTRUCTOR TEMPLATE: Not needed.

;; OBSERVER TEMPLATE:
;; milk-fn : MilkType -> ?
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

;; Definition of CoffeeOrder:

;; REPRESENTATION:
;; A CoffeeOrder is represented as a struct
;;  (make-coffee-order size type milk)
;; with the following fields:
;; INTERP:
;;   size : Size           is the size of cup desired
;;   type : CoffeeType     is the kind of coffee order
;;   milk : MilkType       is the kind of milk ordered

;; IMPLEMENTATION:
(define-struct coffee-order (size type milk))

;; CONSTRUCTOR TEMPLATE
;; (make-coffee-order Size CoffeeType MilkType)

;; OBSERVER TEMPLATE
;; coffee-order-fn : CoffeeOrder -> ??
(define (coffee-order-fn co)
  (...
   (coffee-order-size co)
   (coffee-order-type co)
   (coffee-order-milk co)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; A WineOrder is represented as a struct
;;  (make-wine-order vineyard vintage)
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

;; IMPLEMENTATION:
(define-struct wine-order (vineyard vintage))

;; CONSTRUCTOR TEMPLATE:
;;  (make-wine-order Vineyard Vintage)

;; OBSERVER TEMPLATE:
;; wine-order-fn : WineOrder -> ??
(define (wine-order-fn wo)
  (...
   (wine-order-vineyard wo)
   (wine-order-vintage  wo)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; A TeaOrder is represented as a struct (make-tea-order size type)
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

;; Mixed Data:

;; REPRESENTATION:
;; A BarOrder is represented as one of
;; -- a CoffeeOrder
;; -- a WineOrder
;; -- a TeaOrder

;; CONSTRUCTOR TEMPLATE:
;; use the constructor templates for CoffeeOrder, WineOrder, or TeaOrder.

;; OBSERVER TEMPLATE:
;; bo-fn : BarOrder -> ??
;; STRATEGY: Cases on order : BarOrder
(define (bo-fn order)
  (cond
    [(coffee-order? order) ...]
    [(wine-order?   order) ...]
    [(tea-order?    order) ...]))


;; In the ... you can put a function on the order, or you can expand
;; the observer template for the compound data, eg:

#;
(define (bo-fn1 order)
  (cond
    [(coffee-order? order) (...
                             (coffee-order-size order)
                             (coffee-order-type order)
                             (coffee-order-milk order))]
    [(wine-order?   order) (wine-order-fn1 order)]
    [(tea-order?    order) ...]))


