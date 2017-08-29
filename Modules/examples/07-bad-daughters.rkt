;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname 07-bad-daughters) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require "extras.rkt")
(require rackunit)

;; filtering bad nodes

(define-struct node (bad? data daughters))
;; A Node is a (make-node Badness Integer ListOfNode)

;; A Badness is a Boolean
(define GOOD false)
(define BAD  true)

;; Integer ListOfNode -> Node
(define (good-node n daughters)
  (make-node GOOD n daughters))

;; Integer ListOfNode -> Node
(define (bad-node n daughters)
  (make-node BAD n daughters))

;; Node -> Boolean
(define (is-bad? n)
  (equal? (node-bad? n) BAD))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; example.  Note that all the interesting part is what happens under a bad node.

;  ListOfNode
(define list1                           
  (list
    (good-node 1 (list (good-node 2 empty)))
    (bad-node 3 (list
                  (bad-node 4
                    (list
                      (good-node 5 empty)
                      (bad-node 6 empty)
                      (good-node 7 empty)))
                  (good-node 8
                    (list
                      (good-node 9 empty)
                      (bad-node 10
                        (list
                          (good-node 11 empty)
                          (bad-node 12 empty)))))))
    (good-node 13 empty)))


;; remove-bad-nodes : ListOfNode -> ListOfNode
;; removes all bad nodes in the list, along with all their daughters

;;  EXAMPLE: this removes 3 and its entire subtree
(begin-for-test
  (check-equal? (remove-bad-nodes list1)
    (list
      (good-node 1 (list (good-node 2 empty)))
      (good-node 13 empty))))
    
;; STRATEGY: Use template for ListOfNode

(define (remove-bad-nodes nodes)
  (cond
    [(empty? nodes) empty]
    [else (if (is-bad? (first nodes))
            (remove-bad-nodes (rest nodes))
            (cons (first nodes)
              (remove-bad-nodes (rest nodes))))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; remove-bad-nodes-promote-daughters
;; Like remove-bad-nodes, but daughters of bad nodes are promoted,
;; even if they are bad

;  EXAMPLE:
(begin-for-test
  (check-equal?
    (remove-bad-nodes-promote-daughters list1)
    ;; So bad node 3 is gone, but its daughters 4 and 8 are promoted
    ;; to the top level of the list
    (list
      (good-node 1 (list (good-node 2 empty)))
      ;; (bad-node 3 (list ..))
      (bad-node 4
        (list
          (good-node 5 empty)
          (bad-node 6 empty)
          (good-node 7 empty)))
      (good-node 8
        (list
          (good-node 9 empty)
          (bad-node 10
            (list
              (good-node 11 empty)
              (bad-node 12 empty)))))
      (good-node 13 empty))))



; STRATEGY: Use template for ListOfNodes, then cases on whether (first nodes) is bad.

#;(define (remove-bad-nodes-promote-daughters nodes)
 (cond
   [(empty? nodes) empty]
   [else (if (is-bad? (first nodes))
           (append (node-daughters (first nodes))
                   (remove-bad-nodes-promote-daughters (rest nodes)))
	   (cons (first nodes)
	         (remove-bad-nodes-promote-daughters (rest nodes))))]))

;; ListOfNodes -> ListOfNodes
;; STRATEGY: Use HOF foldr on nodes
(define (remove-bad-nodes-promote-daughters nodes)
  (foldr
    (lambda (first-node ans-for-rest)
      (if (is-bad? first-node)
        (append (node-daughters first-node)
	        ans-for-rest)
	(cons first-node ans-for-rest)))
    empty
    nodes))


;; But this leaves some bad nodes in the structure.  What if we wanted to
;; get rid of them (and promote their daughters)?

;; EXAMPLE:

(begin-for-test
  (check-equal?
    (list-of-nodes-clean-descendants list1)
    ;; so nodes 5 and  7 are promoted to their first non-bad
    ;; ancestor-- in this case the top level of the list.
    ;; node 11 is promoted to be a daughter of 8.
    ;; node
    (list
      (good-node 1 (list (good-node 2 empty)))
      (good-node 5 empty)
      (good-node 7 empty)
      (good-node 8
        (list
          (good-node 9 empty)
          (good-node 11 empty)))
      (good-node 13 empty))))

;; Ans: just recur on the daughters before you append them into the answer

;; ListOfNodes -> ListOfNodes
;; STRATEGY: Use HOF foldr on nodes
#;(define (list-of-nodes-clean-descendants nodes)
    (foldr
     (lambda (first-node ans-for-rest)
       (if (is-bad? first-node)
           (append (list-of-nodes-clean-descendants (node-daughters first-node))
                   ans-for-rest)
           (cons first-node ans-for-rest)))
     empty
     nodes))

;;; Ahh, but this isn't right!  We never look inside first-node.
;; So we really need the mutual recursion:

;; Node -> Node
;; RETURNS: a Node like the given one, but all descendants that are bad are removed.  Any good descendant of a
;; bad descendant is promoted to be a daughter of its
;; nearest good ancestor (or to be a daughter of the given node if there is none)
;; STRATEGY: Use template for Node (recur on daughters)
(define (node-clean-descendants n)
  (make-node
   (node-bad? n)
   (node-data n)
   (list-of-nodes-clean-descendants (node-daughters n))))

;; ListOfNode -> ListOfNode
;; RETURNS: a list of nodes like the given one, except that any nodes in the list or their descendants
;; that are bad are removed.  Any good descendant of a bad node is promoted to be a daughter of its
;; nearest good ancestor (or to the top level of the list if there is none)

;; STRATEGY: Use template for ListOfNode + cases on whether first node is bad.

(define (list-of-nodes-clean-descendants ns)
  (cond
    [(empty? ns) empty]
    [else (if (is-bad? (first ns))
              (append
               (list-of-nodes-clean-descendants (node-daughters (first ns)))
               (list-of-nodes-clean-descendants (rest ns)))
              (cons
               (node-clean-descendants (first ns))
               (list-of-nodes-clean-descendants (rest ns))))]))



           