;; Here is one solution:

(define-struct spider (nlegs volume))
(define-struct elephant (volume))
(define-struct boa (length girth volume))  
(define-struct armadillo (length volume appetite))

;; A ZooAnimal is one of:
;; -- (make-spider NonNegInt PosReal)
;; -- (make-elephant PosReal)
;; -- (make-boa PosReal PosReal PosReal)
;; -- (make-armadillo PosReal PosReal PosReal)

;; INTERPRETATION
;; (make-spider n vol) -- a spider with n legs and volume vol 
;; (make-elephant vol) -- an elephant with volume vol
;; (make-boa l g vol)  -- a boa constrictor with:
;;                         length l cm
;;                         girth  g cm
;;                         volume vol cm^3
;; (make-armadillo l vol app) -- an armadillo with:
;;                                length l cm
;;                                volume vol cm^3
;;                                eats app cm^3/day of ants.
;; NOTE: volume refers to the volume of the container in which the
;; animal is to be shipped.

#|
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

;; I've surrounded the destructor template in #| ... |#, which turns
;; that region into a comment, just as if we'd put semicolons at the
;; front of each line.







