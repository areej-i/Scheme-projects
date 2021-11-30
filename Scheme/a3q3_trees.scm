;Areej Irfan
;101149642


;c. (3 marks) Write a function flatten-tree that takes a tree as an argument and returns a non-nested list of the terminal values of the tree in their original relative order.

(define (flatten-tree tree)
    (cond
       ((null? tree) '())
       ((pair? (car tree)) (append (flatten-tree (car tree))
                  (flatten-tree (cdr tree))))
       (else (cons (car tree) (flatten-tree (cdr tree))))))

;test
(flatten-tree '(1 (((2 3))) ((4 5 (7) (7)))(((8 (9)))))) ;→ (1 2 3 4 5 7 7 8 9)
(flatten-tree '((a (b)) (c) (d ((e))))) ;'(a b c d e))

;a. (3 marks) Write a function called tree-reduce for trees that is analogous to the reduce function for flat lists (see Section 4.2).
;This function should take an operator, an initial value, and a tree as arguments and return the result of combining all nodes of the tree using the given operator.
;For simplicity you may assume only commutative operators are supplied.


(define (tree-reduce operator initial tree)
  (define (treer operator initial tree)
    (if (null? tree) initial
         (operator (car tree) (tree-reduce operator initial (cdr tree))))) 
  (treer operator initial (flatten-tree tree)))
    
;test
(tree-reduce - 0 '(100 (9))) ;→ 91
(tree-reduce + 0 '((10) 20 30 (((40))))) ;→ 100
(tree-reduce * 1 '(((10)) (((20 30))))) ;→ 6000
(tree-reduce * 1 '(((10)) (((0 30))))) ;→ 0

;b. (3 marks) Write a function height that takes as argument an arbitrarily deeply nested list (ie a tree) and returns the maximum depth of any item in any of its sublists;
;the depth of an object in a list is the number of cars that can be applied to it, not necessarily in a row... (You may wish to use the built-in max function for this).

(define (len L)
  (if (null? L) 0
  (if (pair? L)
      (+ 1 (len (cdr L)))
      0)))

       
(define (height tree)
   (if (or (null? tree) (not (pair? tree))) 0
       (max ( + 1 (height (car tree))) (height (cdr tree)))))
 

(height 'a) ;→ 0
(height '(a)) ;→ 1
(height '(a (b) c)) ;→ 2
(height '(((((a(((b))))))))) ;→ 8

#|(6 marks) Write a function (tree-merge T1 T2) that takes two trees as arguments and merges them according to the following rules:
Merging two trees is done by recursing through their structure and merging their subtrees.
The root of T1 is merged with the root of T2
The first child of T1 is merged with the first child of T2
Second with second... etc, etc, etc, ...
Merging two leaf nodes is done by multiplying their values (you may assume they are numbers).
Merging a leaf node with a subtree is done by scaling the subtree by the value of the leaf.
Merging a subtree with an empty tree, is simply the non-empty subtree.
|#

(define (value T) (car T))
(define (right T) (caddr T))
(define (left T) (cadr T))


(define (tree-map f T)
    (cond ((null? T) '())
          ((list? (car T)) (cons (tree-map f (car T))
                                 (tree-map f (cdr T))))
          (else (cons (f (car T))
                      (tree-map f (cdr T))))))


(define (tree-merge T1 T2)
 (if (and (null? T1) (null? T2)) '()
 (if (null? T1) T2
 (if (null? T2) T1
 (* (car T1) (car T2))

 (left T1) = (tree-merge (left T1) (left T2))
 (right T1) = (tree-merge (right T1) (right T1) T1))))))


 
      
;(merge '(2 (2 3) (4 5 (5 6 7))) '((5 (4 3) (2 1)) 6 (7 8)))

 ;-> ((10 (8 6) (4 2)) (12 8) (28 40 (5 6 7)))




        