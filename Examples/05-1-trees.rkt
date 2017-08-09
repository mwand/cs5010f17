;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname 05-1-trees) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require rackunit)
(require "extras.rkt")

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


;; leaf-sum : BinTree -> Number
;; RETURNS: the sum of the values in the leaves of the given tree
;; STRATEGY: Use template for BinTree on t

(define (leaf-sum t)
  (cond
    [(leaf? t) (leaf-datum t)]
    [else (+
            (leaf-sum (node-lson t))
            (leaf-sum (node-rson t)))]))


;; leaf-max : BinTree -> Number
;; RETURNS: the largest leaf value in the given tree
;; STRATEGY: Use template for Tree on t

(define (leaf-max t)
  (cond
    [(leaf? t) (leaf-datum t)]
    [else (max
            (leaf-max (node-lson t))
            (leaf-max (node-rson t)))]))


;; leaf-min : BinTree -> Number
;; RETURNS: the smallest leaf value in the given tree
;; STRATEGY: Use template for Tree on t
(define (leaf-min t)
  (cond
    [(leaf? t) (leaf-datum t)]
    [else (min
            (leaf-min (node-lson t))
            (leaf-min (node-rson t)))]))

;; tests:

(define leaf-13 (make-leaf 13))
(define leaf-2  (make-leaf  2))
(define leaf-8  (make-leaf  8))

(define tree1
  (make-node leaf-13 leaf-2))

(define tree2 (make-node leaf-8 tree1))

(define tree3 (make-node tree1 tree2))

(begin-for-test
  (check-equal? (leaf-sum tree1) 15)
  (check-equal? (leaf-max tree1) 13)
  (check-equal? (leaf-min tree1)  2)

  (check-equal? (leaf-sum tree3) 38)
  (check-equal? (leaf-max tree3) 13)
  (check-equal? (leaf-min tree3)  2))

    
          
