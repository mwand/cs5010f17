;; 06-2-trees.rkt

(define-struct leaf (datum))
(define-struct node (lson rson))

;; A Tree is either
;; -- (make-leaf Number)
;; -- (make-node Tree Tree) 

#|
tree-fn : Tree -> ???
HALTING MEASURE: number of nodes in t
(define (tree-fn t)
  (cond
    [(leaf? t) (... (leaf-datum t))]
    [else (...
            (tree-fn (node-lson t))
            (tree-fn (node-rson t)))]))
|#

;; leaf-sum : Tree -> Number
;; STRATEGY: Use template for Tree on t
(define (leaf-sum t)
  (cond
    [(leaf? t) (leaf-datum t)]
    [else (+
            (leaf-sum (node-lson t))
            (leaf-sum (node-rson t)))]))

;; leaf-max : Tree -> Number
;; STRATEGY: Use template for Tree on t
(define (leaf-max t)
  (cond
    [(leaf? t) (leaf-datum t)]
    [else (max
            (leaf-max (node-lson t))
            (leaf-max (node-rson t)))]))

;; leaf-min : Tree -> Number
;; STRATEGY: Use template for Tree on t
(define (leaf-min t)
  (cond
    [(leaf? t) (leaf-datum t)]
    [else (min
            (leaf-min (node-lson t))
            (leaf-min (node-rson t)))]))
