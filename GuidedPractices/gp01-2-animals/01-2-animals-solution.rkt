;; Here is one solution:

(define-struct spider (nlegs volume))
(define-struct elephant (volume))
(define-struct boa (length girth volume))  
(define-struct armadillo (length volume appetite))

;; A ZooAnimal is represented by one of the following structs:
;; (make-spider nlegs volume) -- a spider 
;; (make-elephant volume) -- an elephant 
;; (make-boa l g volume)  -- a boa constrictor
;; (make-armadillo l volume app) -- an armadillo

;; INTERPRETATION OF FIELDS:
;; nlegs    : NonNegInt    number of legs of this animal
;; volume   : PosReal      volume of shipping container required for
;;                         this animal, in cm^3.
;; length   : PosReal      length of this animal in cm
;; girth    : PosReal      girth of this animal, in cm
;; appetite : PosReal      appetite of this animal (in cm^3 of ants
;;                         per day:)

; animal-fn: ZooAnimal -> ??
(define (animal-fn a)
  (cond
    [(spider? a)
     (...
       (spider-nlegs a)
       (spider-volume a))]
    [(elephant? a)
     (...
       (elephant-volume a))]
    [(boa? a)
     (...
       (boa-length a)
       (boa-girth a)
       (boa-volume a))]
    [(armadillo? a)
     (...
       (armadillo-length a)
       (armadillo-volume a)
       (armadillo-appetite a))]))
|#

(define spider1 (make-spider 7 10.0))
(define elephant1 (make-elephant 1.5e6))
(define boa1 (make-boa 105.6 8.2 2000))
(define armadillo1 (make-armadillo 56 3000 125))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; COMMENTARY: This is only one possible solution.  Any correct
;; solution will look much like this.  Here are some of the ways that
;; a correct solution might differ from this one:

;; It is ok to use different field names, since these were not
;; specified in the problem.

;; For the boa, I've included volume as a field, since the problem
;; said "whose attributes INCLUDE length and girth"

;; For the elephant, it would be incorrect to have any other fields,
;; because the problem said "whose ONLY attributes are the space they
;; need.."

;; Your interpretation MUST include units-- otherwise the
;; interpretation is incomplete.

;; I've been extra careful about writing NonNegInt, PosReal, etc.
;; If I'd written Number, that would allow for an elephant with an
;; imaginary volume :) 
