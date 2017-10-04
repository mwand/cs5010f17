;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname 08-7-reachability) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require rackunit)
(require "extras.rkt")

(require "sets.rkt")

;; graph reachability 

;; data definitions using list of edges:

;; A Node is a Symbol
;; A Graph is a ListOfEdge with no repeats

(define-struct edge (from to))
;; An Edge is a (make-edge Node Node)

(define graph1
  (list
    (make-edge 'a 'c)
    (make-edge 'b 'a)
    (make-edge 'b 'c)))

(begin-for-test
  (check-true
   (reachable? 'a 'a graph1))
  (check-true
   (reachable? 'b 'b graph1)))

;; this is the graph from the slides
(define graph2
  (list
    (make-edge 'a 'b)
    (make-edge 'a 'c)
    (make-edge 'a 'd)
    (make-edge 'b 'c)
    (make-edge 'd 'c)
    (make-edge 'd 'f)
    (make-edge 'c 'e)
    (make-edge 'f 'g)
    (make-edge 'e 'g)
    (make-edge 'g 'b)))

(define (node=? n1 n2) (symbol=? n1 n2))

;; Node Graph -> ListofNode
(define (successors n1 loe)
  (map 
   edge-to  
   (filter
    (lambda (e) (node=? (edge-from e) n1))
    loe)))

(begin-for-test
  (check set-equal?
    (successors 'a (list (make-edge 'a 'b) (make-edge 'a 'c)))
    (list 'b 'c))
  (check set-equal?
    (successors 'a graph2)
    '(b c d)))

;; SetOfNode Graph -> SetOfNode
;; GIVEN: A set of nodes
;; RETURNS: the set of all their immediate successors
;; STRATEGY: use HOF foldr on nodes

(define (all-successors nodes graph)
  (foldr
    (lambda (node s)
      (set-union
        (successors node graph)
        s))
    empty
    nodes))

(begin-for-test
  (check set-equal?
    (all-successors empty graph2)
    empty)
  (check set-equal?
    (all-successors (list 'a 'c) graph2)
    (list 'b 'c 'd 'e))
  (check set-equal?
    (all-successors (list 'f 'g) graph2)
    (list 'g 'b)))

(begin-for-test
  (check-equal?
   (reachable? 'a 'a graph2) 
   true
   "there should be a path from a to a in graph2")

  (check-equal?
   (reachable? 'a 'g graph2) 
   true
   "there should be a path from a to g in graph2")

  (check-equal?
   (reachable? 'b 'd graph2)
   false
   "should find no path from b to d")

  (check-true
   (reachable? 'b 'b graph2))
  
  (check-equal?
   (reachable? 'd 'g graph2)
   true
   "should find a path from d to g")
  
  (check-equal?
   (reachable? 'e 'd graph2)
   false
   "should find no path from e to d")
  
  (check-equal? (reachable? 'a 'b graph1) false)

  (check-equal? (reachable? 'b 'c graph1) true))

  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; reachables : SetOfNode Graph -> SetOfNode
;; GIVEN: A set of nodes in a graph
;; RETURNS: the set of nodes reachable from 'nodes'

;; EXAMPLES/TESTS
(begin-for-test
  (check set-equal?
    (reachables (list 'b) graph2)
    (list 'b 'c 'e 'g))
  (check set-equal?
    (reachables (list 'a) graph2)
    (list 'a 'b 'c 'd 'e 'f 'g)))

;;; reachables: two versions

;; reachables.v1 : SetOfNode Graph -> SetOfNode
;; GIVEN: A set of nodes in a finite graph
;; WHERE: reached = the set of nodes reachable in graph g in at most
;; steps from a set of nodes S, for some n and some set of nodes S.
;; RETURNS: the set of nodes reachable from S.
;; STRATEGY: recur on reached + their immediate successors
;; HALTING MEASURE: the number of graph nodes NOT in reached

(define (reachables.v1 reached g)
  (local
    ((define candidates (all-successors reached g)))
    (cond
      [(subset? candidates reached) reached]
      [else (reachables.v1
              (set-union candidates reached)
              g)])))

;; CORRECTNESS REASONING: If 'reached' is the set of nodes reachable
;; from S in at most n steps, then 'candidates' is the set of nodes
;; reachable from S in at most n+1 steps.  If there are no more nodes
;; reachable in n+1 steps than there were in n steps, then we have
;; found all the nodes reachable from S.

;; TERMINATION REASONING: At the recursive call, 'candidates' contains at
;; least one element that is not in 'reached' (otherwise the subset? test
;; would have returned true).  Hence the result of the set-union is at
;; least one element bigger than 'reached'.  So the halting measure
;; decreases. 

;; This is called a CLOSURE ALGORITHM: we want to find the smallest
;; set containing nodes and which is closed under successors.

;; This version keeps looking at the successors of the original
;; nodes.  We only need to look at the successors of the most recently
;; added nodes. We'll do that in reachables.v2.

;;;;;;;;;;;;;;;; reachables.v2 ;;;;;;;;;;;;;;;;

;; reachables1: SetOfNode SetOfNode Graph -> SetOfNode
;; GIVEN: two sets of nodes and a finite graph g
;; WHERE:
;;  reached is the set of nodes reachable in graph g in fewer than n steps
;;        from a set of nodes S, for some S and n
;;  recent is the set of nodes reachable from S in n steps but
;;         not in n-1 steps.
;; RETURNS: the set of nodes reachable from S in g.
(define (reachables1 reached recent g)
  (cond
    [(empty? recent) reached]
    [else
     (local
         ((define next-reached (append recent reached))
          (define next-recent 
            (set-diff (all-successors recent g)
                      next-reached)))
       (reachables1 next-reached next-recent g))]))

;; CORRECTNESS REASONING:  If the invariant is true and recent is
;; empty, then there are no more nodes reachable in n steps than in
;; n-1 steps.  So 'reached' contains all the reachable nodes.

;; Otherwise, if the invariant is true, then 'next-reached' is the
;; set of nodes reachable from S in fewer than n+1 steps. 'next-recent' is the
;; set of nodes reachable from S in fewer than n+1 steps but not
;; in fewer than n steps.  

;; Since next and reached are disjoint, then (append next
;; reached) is a set (that is, no duplications), and is the set of nodes
;; reachable from S in fewer than n+1 steps.  So the recursive call to
;; reachables1 satisfies the invariant.

;; TERMINATION REASONING: If the invariant is true, then 'next' is
;; non-empty, so at the recursive call the number of nodes _not_ in
;; 'reached' is smaller.

;; reachables.v2 : SetOfNode Graph -> SetOfNode
;; GIVEN: A set of nodes in a finite graph
;; RETURNS: the set of nodes reachable from S.
;; STRATEGY: Call a more general function

(define (reachables.v2 nodes g)
  (reachables1 empty nodes g))

;; CORRECTNESS REASONING: There are no nodes reachable from 'nodes' in
;; fewer than 0 steps.  The set of nodes reachable from 'nodes' in
;; at most 0 steps is just 'nodes'.  So the call to reachables1
;; satisfies reachable1's invariant.

;; TERMINATION REASONING: No termination reasoning necessary because
;; this function relies on the termination of reachables1, which we've
;; already established.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; uncomment all but one of these lines
; (define reachables reachables.v1) "using reachables.v1"
(define reachables reachables.v2) "using reachables.v2"

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; reachable?, 2 versions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; reachable? : Graph Node Node -> Boolean
;; GIVEN: a graph and a source and a target node in the graph
;; RETURNS: true iff there is a path in g from src to tgt
;; EXAMPLES: See tests above

;;;;;;;;;;;;;;;; reachable?.v1 ;;;;;;;;;;;;;;;;

;; STRATEGY: call simpler function
;; [or more general function -- either would be OK
(define (reachable?.v1 src tgt g)
  (member tgt (reachables (list src) g)))
 
;; does this code depend on the representation of the graph?

;;;;;;;;;;;;;;;; reachable?.v2 ;;;;;;;;;;;;;;;;

;; better: instead of building the whole reachability set, 
;; just watch for tgt to show up:

;; reachable-from? : SetOfNodes SetOfNodes Node Graph -> Boolean
;; GIVEN: two sets of nodes, a node, and a graph
;; WHERE:
;;  reached is the set of nodes reachable in graph g in fewer than n steps
;;        from some starting node 'src', for some n
;;  recent is the set of nodes reachable from src in n steps but
;;         not in n-1 steps.
;; AND tgt is not in reached
;; RETURNS: true iff tgt is reachable from src in g.

(define (reachable-from? reached recent tgt g)
   (cond
      [(member tgt recent) true]
      [(empty? recent) false]
      [else
       (local
           ((define next-reached (append recent reached))
            (define next-recent 
              (set-diff (all-successors recent g)
                        next-reached)))
         (reachable-from? next-reached next-recent tgt g))]))


;; CORRECTNESS REASONING: If the invariant is true and tgt is in
;; recent, then tgt is reachable from src.  If the invariant is true
;; and recent is empty, then reached consists of all the nodes that
;; are reachable from src.  According to the invariant, tgt is not in
;; reached, so tgt is not reachable from src.

;; Otherwise, we need to check that the recursive call satisfies the
;; invariant.  Since next and reached are disjoint, then (append next
;; reached) is a set (that is, no duplications), and is the set of nodes
;; reachable from src in fewer than n+1 steps.  next-recent is exactly
;; the set of nodes reachable from src in n+1 steps but not in n steps
;; (because of the set-diff).  Last, tgt is not in reached or in
;; recent, so it is not in next-reached. So the recursive call to
;; reachables1 satisfies the invariant.

;; TERMINATION REASONING: If the invariant is true, then recent is
;; non-empty, so at the recursive call the number of nodes _not_ in
;; 'reached' is smaller.

;;;;;;;;;;;;;;;;

;; reachable?.v2 : Node Node Graph -> Boolean
;; GIVEN: Two nodes and a finite graph
;; RETURNS: true iff tgt is reachable from src
;; STRATEGY: Call a more general function

(define (reachable?.v2 src tgt g)
  (reachable-from? empty (list src) tgt g))

;; CORRECTNESS REASONING: There are no nodes reachable from src in
;; fewer than 0 steps.  The set of nodes reachable from src in
;; at most 0 steps is just (list src).  So the call to reachable-from?
;; satisfies reachable-from?'s invariant.

;; TERMINATION REASONING: No termination reasoning necessary because
;; this function relies on the termination of reachable-from?, which we've
;; already established.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; uncomment one of these lines:
; (define reachable? reachable?.v1) "using reachable?.v1"
(define reachable? reachable?.v2) "using reachable?.v2"

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; now provided by sets.rkt
#;(define (set-diff set1 set2)
  (filter
    (lambda (x) (not (my-member? x set2)))
    set1))
