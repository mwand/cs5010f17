;; non-empty lists

;; Let's say you have some information that you want to represent as a
;; non-empty list of X's.

;; Let's say we have a non-empty bunch of sardines, which we choose to represent
;; as a list

;; A Sardine is represented as ...

;; AtLeastOneSardine is represented as a non-empty list of Sardines.

;; CONSTRUCTORS:
;; (cons s empty)  where s is a Sardine
;; (cons s ss)
;;      where s is a Sardine
;;      and  ss is an AtLeastOneSardine

;; OBSERVER
;; atlos-fn : AtLeastOneSardine -> ??
(define (atlos-fn ss)
  (cond
    [(empty? (rest ss)) (... (first ss))]
    [else (...
           (first ss)
           (atlos-fn (rest ss)))]))

;; Note that (rest ss) is guaranteed to be an AtLeastOneSardine
