;;; A Data Definition is a grammar


Data Definition:

A ListOfString is either
-- empty
-- (cons String ListOfString)

Context-Free Grammar:

ListOfString -> empty
ListOfString -> (cons String ListOfString)

Example:

ListOfString
-> (cons String ListOfString)
-> (cons "Marvin" ListOfString)
-> (cons "Marvin" (cons String ListOfString))
-> (cons "Marvin" (cons String empty))
-> (cons "Marvin" (cons "Rahul" empty))

5 Steps

A BinaryTree is either
-- (make-leaf Number)
-- (make-node BinaryTree BinaryTree)


BinaryTree
-> (make-node BinaryTree BinaryTree)
-> (make-node (make-node BinaryTree BinaryTree)
              BinaryTree)
-> (make-node (make-node (make-leaf Number) BinaryTree)
              BinaryTree)
-> (make-node (make-node (make-leaf 100) BinaryTree)
              BinaryTree)
-> (make-node (make-node (make-leaf 100) (make-leaf Number))
              BinaryTree)
-> (make-node (make-node (make-leaf 100) (make-leaf 200))
              BinaryTree)
-> (make-node (make-node (make-leaf 100) (make-leaf 200))
              (make-node Number))
-> (make-node (make-node (make-leaf 100) (make-leaf 200))
              (make-node 300))

8 steps

If it takes x steps to build t1 and y steps to build t2,
then it takes x+y+1 steps to build (make-node t1 t2) :

BinaryTree
-> (make-node BinaryTree BinaryTree)
-> ... {x steps}
-> (make-node t1 BinaryTree)
-> ... {y steps}
-> (make-node t1 t2)

Let steps(t) denote the number of steps it takes to build t.
Then steps(node-lson t) < steps(t)
and  steps(node-rson t) < steps(t)

So if we have a function following the template

(define (tree-fn t)
  (cond
    [(leaf? t) ...]
    [else (... (tree-fn (node-lson t))
               (tree-fn (node-rson t)))]))

steps(t) is a halting measure for tree-fn.

====================================================

The same idea works for SoS and LoSS:

Data Definition:

An S-expression of Strings (SoS) is either
-- a String
-- a List of SoS's (LoSS)

A List of SoS's (LoSS) is either
-- empty
-- (cons SoS LoSS)

Grammar:

SoS  -> String
SoS  -> LoSS
LoSS -> empty
LoSS -> (cons SoS LoSS)

{repeat example}

If it takes k steps to get from LoSS to lst, then it takes k+1 steps
to get from SoS to lst:

SoS -> LoSS -> ...{k steps} -> lst

If it takes k1 steps to get from SoS to s
and k2 steps to get from LoSS to lst
then it takes k1+k2+1 steps to get from LoSS to (cons s lst)

LoSS
-> (cons SoS LoSS)
-> {k1 steps}
-> (cons s LoSS)
-> {k2 steps}
-> (cons s lst)

Let's look at the template function:





