;; example for guided practice 7-2

;; f : Integer -> Integer
;; GIVEN/RETURNS: omitted
;; INVARIANT: to be filled in
(define (f n)
  (cond
    [(zero? n) 1]
    [else (* 2 (f (- n 3)))]))


