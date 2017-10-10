;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; BINARY TREES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; A Binary Tree is represented as a BinTree, which is either:
;; (make-leaf datum)
;; (make-node lson rson)

;; INTERPRETATON:
;; datum      : Real       some real data
;; lson, rson : BinTree    the left and right sons of this node

;; IMPLEMENTATION:
(define-struct leaf (datum))
(define-struct node (lson rson))

;; CONSTRUCTOR TEMPLATES:
;; -- (make-leaf Number)
;; -- (make-node Tree Tree) 

;; OBSERVER TEMPLATE:
;; tree-fn : Tree -> ???
(define (tree-fn t)
  (cond
    [(leaf? t) (... (leaf-datum t))]
    [else (...
            (tree-fn (node-lson t))
            (tree-fn (node-rson t)))]))

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

;; A Person is represented by a struct which is one of 
;; -- (make-adam)
;; -- (make-eve)
;; -- (make-person name father mother)
;; INTERPRETATION:
;; (make-eve)             represents the first woman
;; (make-adam)            represents the first man
;;                        (these have no ancestors)
;; name : String          the person's name
;; father : Person        the person's father
;; mother : Person        the person's mother

;; IMPLEMENTATION:
(define-struct adam ())
(define-struct eve ()
(define-struct person (name father mother))

;; CONSTRUCTOR TEMPLATES
;; -- (make-adam)
;; -- (make-eve)
;; -- (make-person String Person Person)

;; OBSERVER TEMPLATE:
;; person-fn : Person -> ???
(define (person-fn p)
  (cond
    [(adam? p) ...]
    [(eve? p) ...]
    [else (...
           (person-name p)
           (person-fn (person-father p))
           (person-fn (person-mother p)))]))


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


