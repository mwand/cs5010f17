;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname 06-3-tree-fold) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;; solution to guided practice 6.3

(require rackunit)
(require "extras.rkt")

(define-struct leaf (datum))
(define-struct node (lson rson))

;; A Tree is either
;; -- (make-leaf Number)
;; -- (make-node Tree Tree) 

;; Template:
;; tree-fn : Tree -> ???
;; (define (tree-fn t)
;;   (cond
;;     [(leaf? t) (... (leaf-datum t))]
;;     [else (...
;;             (tree-fn (node-lson t))
;;             (tree-fn (node-rson t)))]))

;; tree-fold  
;;  : (X X -> X) (Number -> X) Tree -> X
;; Strategy: Use template for Tree on t
(define (tree-fold combiner base t)
  (cond
    [(leaf? t) (base (leaf-datum t))]
    [else (combiner
            (tree-fold combiner base 
              (node-lson t))
            (tree-fold combiner base 
              (node-rson t)))]))



;; number-of-nodes : Tree -> Number
;; RETURNS: the number of nodes in the given tree.
;; Strategy: Use HOF tree-fold on t
(define (number-of-nodes t)
  (tree-fold
    ;; Number Number -> Number
    (lambda (ans-for-lson ans-for-rson)
      (+ 1 ans-for-lson ans-for-rson))
    (lambda (num) 1)
    t))

;; increment-all : Tree -> Tree
;; RETURNS: a tree just like the original, but in which all of the
;; leaves have contents one more  than in the original.
;; STRATEGY: Use HOF tree-fold on t
(define (increment-all t)
  (tree-fold
    ;; Number Number -> Number
    (lambda (ans-for-lson ans-for-rson)
      (make-node ans-for-lson ans-for-rson))
    (lambda (num) (make-leaf (+ 1 num)))
    t))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Tests

(define tree1 
  (make-node
    (make-node 
      (make-leaf 3)
      (make-leaf 14))
    (make-node
      (make-leaf 15)
      (make-node 
        (make-leaf 9)
        (make-leaf 26)))))


(define tree1-ans 
  (make-node
    (make-node 
      (make-leaf 4)
      (make-leaf 15))
    (make-node
      (make-leaf 16)
      (make-node 
        (make-leaf 10)
        (make-leaf 27)))))

(begin-for-test
  (check-equal?
   (number-of-nodes (make-leaf 11))
   1)
  (check-equal?
    (number-of-nodes tree1)
    9)
  (check-equal?
    (number-of-nodes tree1-ans)
    9)
  (check-equal?
    (increment-all tree1)
    tree1-ans))

