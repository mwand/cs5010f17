;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname car-order) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp")) #f)))
;; data definitions for the car order

(define-struct car (vin model int-col ext-col sunroof? radio-option))

;; a Car is a (make-car String String Color Color Boolean RadioOption)
;; vin    is the vehicle identification number
;; model  is the model of the car
;; int-col is the interior color of the car
;; ext-col is the exterior color of the car
;; sunroof?  tells whether the car has a sunroof
;; radio-option specifies the radio option that has been chosen for this car

;; car-fn : Car -> ??
(define (car-fn c)
  (... (car-vin c) (car-model c)
       (car-int-col c) (car-ext-col c)
       (car-sunroof? c) (car-radio-option c)))

;; A RadioOption is one of
;; -- (make-plain-radio)
;; -- (make-fancy-stereo PosInt)
;;    WHERE the PosInt is in [1,6]
;; -- (make-nav-system MapSupplier)


(define-struct plain-radio ())
(define-struct fancy-stereo (nspeakers))
;; interp: nspeakers is half of the number of speakers of the stereo
(define-struct nav-system (source))
;; interp: source is the supplier of the maps for the nav system

;; radio-option-fn : RadioOption -> ??
(define (radio-option-fn r)
  (cond
    [(plain-radio? r) ...]
    [(fancy-stereo? r) (... (fancy-stereo-nspeakers r))]
    [(nav-system? r)   (... (nav-system-source r))]))

;; if you used "plain-radio" as the representation, instead of a
;; struct, you'd say:
(define (plain-radio? r)
  (and (string? r) (string=? r "plain-radio")))

;; a MapSupplier is one of
;; -- "Used to do no evil"  interp: The Google Corporation, Mountain View CA
;; -- "Sellmorephones"      interp: The Apple Corporation

;; Note: this representation is a joke.  It is definitely *not*
;; beautiful, and is NOT to be emulated!

;; map-supplier-fn : MapSupplier -> ??
(define (map-supplier-fn s)
  (cond
    [(string=? s "Used to do no evil") ...]
    [(string=? s "Sellmorephones")     ...]))





















