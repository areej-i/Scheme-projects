#|
[4 marks] Create a function called eval-prefix that takes a prefix expression tree as argument with the following form:
tree := number
tree := (operator tree tree)
The function should return the evaluated value of the given prefix expression tree. Your solution should traverse the given expression and
should not call the built-in eval or apply functions. You may assume that the only operations are: +, -, *, /.
|#

(define (eval-prefix T)
  (if (null? T) T
      (if (null? (cdr T)) T
          if (list? (car T)) (cons (caar T) (cdr T))
          (cons (car T) (eval-prefix (cdr T))))))
      
(eval-prefix '(+ (+ 1 2) 3)) ;→ 6
(eval-prefix '(* (+ 1 2) (* 1 (+ 1 (- 4 2))))) ;→ 9

#|
[11 marks] Create a function called parse-infix that takes an infix expression tree as argument and returns an equivalent prefix
expression tree. Infix expression trees will have the following form:
infix := number
infix := (infix operator infix)
infix := (infix operator infix operator infix)
infix := (infix operator infix operator infix ...)
Operators may be one of the set: {+, -, *, /}. Standard precedence and associativity rules should be followed.
|#


(define (parse-infix T)
  (if (null? T) T
      (if (not (list? T)) T
          (if (null? (cdr T)) (parse-infix (car T))
          (list (cadr T) (parse-infix (car T)) (parse-infix (cddr T)))))))
  
(parse-infix '(10 - 5))  ;→ (- 10 5)
(parse-infix '(1 + 2 - 3))  ;→ (- (+ 1 2) 3)
(parse-infix '(1 + 2 * 3 - 4))  ;→ (- (+ 1 (* 2 3)) 4)
(parse-infix '(1 + (2 + 2) * 3))  ;→ (+ 1 (* (+ 2 2) 3))