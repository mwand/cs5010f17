;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname 6-2-trees) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;; solution to practice 6.2

(require rackunit)
(require "extras.rkt")

(define-struct leaf-node (data))
(define-struct binary-node (lson rson))
(define-struct ternary-node (lson mson rson))

;; A 23Tree is one of
;; -- (make-leaf-node Number)
;; -- (make-binary-node 23Tree 23Tree)
;; -- (make-ternary-node 23Tree 23Tree 23Tree)

;; Template:
;; tree-fn : 23Tree -> ??
;; HALTING MEASURE: number of nodes in t
;(define (tree-fn t)
;  (cond
;    [(leaf-node? t) (... (leaf-node-data t))]
;    [(binary-node? t) (... (tree-fn (binary-node-lson t))
;                           (tree-fn (binary-node-rson t)))]
;    [(ternary-node? t) (... (tree-fn (ternary-node-lson t))
;                            (tree-fn (ternary-node-mson t))
;                            (tree-fn (ternary-node-rson t)))]))

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
  (cond
    [(leaf-node? t) (leaf-node-data t)]
    [(binary-node? t) (max (leaf-max (binary-node-lson t))
                           (leaf-max (binary-node-rson t)))]
    [(ternary-node? t) (max (leaf-max (ternary-node-lson t))
                            (leaf-max (ternary-node-mson t))
                            (leaf-max (ternary-node-rson t)))]))



;; double-all : 23Tree -> 23Tree
;; RETURNS: a tree like the original, but with all the leaf nodes doubled.
;; STRATEGY: Use template for 23Tree on t

(define (double-all t)
  (cond
    [(leaf-node? t) (make-leaf-node (* 2 (leaf-node-data t)))]
    [(binary-node? t) (make-binary-node
                        (double-all (binary-node-lson t))
                        (double-all (binary-node-rson t)))]
    [(ternary-node? t) (make-ternary-node
                         (double-all (ternary-node-lson t))
                         (double-all (ternary-node-mson t))
                         (double-all (ternary-node-rson t)))]))

(begin-for-test
  (check-equal?
    (leaf-max sample-tree)
    42
    "max in sample tree should be 42")
  (check-equal?
    (double-all sample-tree)
    sample-tree-ans
    "wrong answer for double-all on example in probem"))



     
