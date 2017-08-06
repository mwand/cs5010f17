;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; TREES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-struct leaf (datum))
(define-struct node (lson rson))

;; A Tree is either
;; -- (make-leaf Number)
;; -- (make-node Tree Tree) 

#|
tree-fn : Tree -> ???
(define (tree-fn t)
  (cond
    [(leaf? t) (... (leaf-datum t))]
    [else (...
            (tree-fn (node-lson t))
            (tree-fn (node-rson t)))]))
|#

;; tree-fold : (X X -> X) (Number -> X) Tree -> X
;; STRATEGY: Use template for Tree on t
(define (tree-fold combiner base t)
  (cond
    [(leaf? t) (base (leaf-datum t))]
    [else (combiner
            (tree-fold combiner base 
              (node-lson t))
            (tree-fold combiner base 
              (node-rson t)))]))

;; STRATEGY: Use HOF tree-fold on t
(define (tree-sum t) 
  (tree-fold + (lambda (n) n) t))

;; STRATEGY: Use HOF tree-fold on t
(define (tree-min t) 
  (tree-fold min (lambda (n) n) t))

;; STRATEGY: Use HOF tree-fold on t
(define (tree-max t) 
  (tree-fold max (lambda (n) n) t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ancestor trees
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-struct adam ())
(define-struct eve ()
(define-struct person (name father mother))

;; A Person is either
;; -- (make-adam)
;; -- (make-eve)
;; -- (make-person String Person Person)

#|
;; person-fn : Person -> ???
(define (person-fn p)
  (cond
    [(adam? p) ...]
    [(eve? p) ...]
    [else (...
           (person-name p)
           (person-fn (person-father p))
           (person-fn (person-mother p)))]))
|#

;; person-fold 
;;  : X X (String X X -> X) Person -> X
(define (person-fold adam-val eve-val combiner p)
  (cond
    [(adam? p) adam-val]
    [(eve? p) eve-val]
    [else (combiner
           (person-name p)
           (person-fold adam-val eve-val combiner
            (person-father p))
           (person-fold adam-val eve-val combiner
            (person-mother p)))]))


