;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname solution) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require rackunit)
(require "extras.rkt")

;; A 23Tree is one of
;; -- (make-leaf-node data)
;; -- (make-binary-node lson rson)
;; -- (make-ternary-node lson mson rson)
;; INTERPRETATION:
;; data             : Real     -- the data at the leaf
;; lson, mson, rson : 23Tree   -- the subtrees of the node


;; IMPLEMENTATION
(define-struct leaf-node (data))
(define-struct binary-node (lson rson))
(define-struct ternary-node (lson mson rson))


;; OBSERVER TEMPLATE:
;; tree-fn : 23Tree -> ??
;(define (tree-fn t)
;  (cond
;    [(leaf-node? t) (... (leaf-node-data t))]
;    [(binary-node? t) (... (tree-fn (binary-node-lson t))
;                           (tree-fn (binary-node-rson t)))]
;    [(ternary-node? t) (... (tree-fn (ternary-node-lson t))
;                            (tree-fn (ternary-node-mson t))
;                            (tree-fn (ternary-node-rson t)))]))

;; 23-tree-fold : (Real -> X) (X X -> X) (X X X -> X) 23Tree -> X
(define (23-tree-fold leaf-fn binary-combiner ternary-combiner t)
  (cond
   [(leaf-node? t) (leaf-fn (leaf-node-data t))]
   [(binary-node? t) (binary-combiner
                      (23-tree-fold leaf-fn binary-combiner ternary-combiner (binary-node-lson t))
                      (23-tree-fold leaf-fn binary-combiner ternary-combiner (binary-node-rson t)))]
   [(ternary-node? t) (ternary-combiner
                       (23-tree-fold leaf-fn binary-combiner ternary-combiner (ternary-node-lson t))
                       (23-tree-fold leaf-fn binary-combiner ternary-combiner (ternary-node-mson t))
                       (23-tree-fold leaf-fn binary-combiner ternary-combiner (ternary-node-rson t)))]))



;; Example from practice:
(define sample-tree
  (make-binary-node
    (make-ternary-node
      (make-binary-node
        (make-leaf-node 26)
        (make-leaf-node 42))
      (make-leaf-node 13)
      (make-leaf-node 11))
    (make-binary-node 
      (make-leaf-node 35)
      (make-leaf-node 14))))
                             
(define sample-tree-ans
  (make-binary-node
    (make-ternary-node
      (make-binary-node
        (make-leaf-node 52)
        (make-leaf-node 84))
      (make-leaf-node 26)
      (make-leaf-node 22))
    (make-binary-node 
      (make-leaf-node 70)
      (make-leaf-node 28))))


;; leaf-max : 23Tree -> Number
;; RETURNS: the largest leaf in the given tree
;; STRATEGY: Use template for 23Tree on t

(define (leaf-max t)
  (23-tree-fold
   (lambda (x) x)
   (lambda (v1 v2) (max v1 v2))
   (lambda (v1 v2 v3) (max v1 v2 v3))
   t))

;; double-all : 23Tree -> 23Tree
;; RETURNS: a tree like the original, but with all the leaf nodes
;; doubled.
;; STRATEGY: Use template for 23Tree on t

(define (double-all t) 
  (23-tree-fold
   (lambda (x) (make-leaf-node (* 2 x)))
   (lambda (t1 t2) (make-binary-node t1 t2))
   (lambda (t1 t2 t3) (make-ternary-node t1 t2 t3))
   t))

;; OR:

(define (double-all2 t)
  (23-tree-fold
   (lambda (x) (make-leaf-node (* 2 x)))
   make-binary-node
   make-ternary-node
  t))


(begin-for-test
  (check-equal?
    (leaf-max sample-tree)
    42
    "max in sample tree should be 42")
  (check-equal?
    (double-all sample-tree)
    sample-tree-ans
    "wrong answer for double-all on example in probem")
  (check-equal?
    (double-all2 sample-tree)
    sample-tree-ans
    "wrong answer for double-all2 on example in probem")
  )



     
