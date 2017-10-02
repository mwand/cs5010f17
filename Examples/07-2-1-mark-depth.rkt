;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname 07-2-1-mark-depth) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;; bintree-depth

(require rackunit)
(require "extras.rkt")

(define-struct bintree (left data right))

;; A XBinTree is either
;; -- empty
;; -- (make-bintree XBinTree X XBinTree)

;; This is just data, so there's no interpretation

;; Template:
;; bintree-fn : BinTreeofX -> ??
;; (define (bintree-fn tree)
;;   (cond
;;     [(empty? tree) ...]
;;     [else (... 
;;            (bintree-fn (bintree-left tree))
;;            (bintree-data tree)
;;            (bintree-fn (bintree-right tree)))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Version 1: Using an invariant

;; mark-subtree : XBinTree NonNegInt-> BinTreeOfNumber
;; GIVEN: a subtree st of some tree, and a non-neg int n 
;; WHERE: the subtree occurs at depth n in the tree
;; RETURNS: a tree the same shape as st, but in which 
;; each node is marked with its distance from the top of the tree
;; STRATEGY: Use template for XBinTree on st

(define (mark-subtree st n)
  (cond
    [(empty? st) empty]
    [else (make-bintree
            (mark-subtree (bintree-left st) (+ n 1))
            n
            (mark-subtree (bintree-right st) (+ n 1)))]))

;; mark-depth : XBinTree -> BinTreeNumber
;; RETURNS: a bintree like the given one, except that each node is
;; replaced by its depth.
;; EXAMPLES: see below
;; STRATEGY: call a more general function

(define (mark-depth t)
  (mark-subtree t 0))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; tests

(begin-for-test
  (check-equal?
    (mark-depth
      (make-bintree 
        (make-bintree
          (make-bintree empty "foo" empty)
          "bar"
          empty)
        "baz"
        (make-bintree
          (make-bintree empty "quux" empty)
          "frob"
          empty)))

    (make-bintree 
      (make-bintree
        (make-bintree empty 2 empty)
        1
        empty)
      0
      (make-bintree
        (make-bintree empty 2 empty)
        1
        empty)))

  (check-equal?
    (mark-subtree 
      (make-bintree 
        (make-bintree
          (make-bintree empty "foo" empty)
          "bar"
          empty)
        "baz"
        (make-bintree
          (make-bintree empty "quux" empty)
          "frob"
          empty))
      10)

    (make-bintree 
      (make-bintree
        (make-bintree empty 12 empty)
        11
        empty)
      10
      (make-bintree
        (make-bintree empty 12 empty)
        11
        empty))))
