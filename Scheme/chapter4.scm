
;                     CHAPTER 4


(define x (cons 1 2))  ;  x = (1 . 2)
(car x) → 1
(cdr x) → 2
(pair? x) → #t

(define y (cons 3 4))	; y = (3 . 4)

(define z (cons x y))  ; z = ((1 . 2) . (3 . 4))
(car z) → (1 . 2)
(cdr z) → (3 . 4)
(car (car z)) → 1
(car (cdr z)) → 3

(caar z) → (car (car z)) → 1
(cadr z) → (car (cdr z)) → 3

;________________________________________________________

;= ← value comparison of numbers only.

(= 3 3)   ;#t
(= 3 3.0) ;#t

;eq? ← identity comparison. Compares references, may not work for primitives.

(define x (list 1 2 3 4))
(define y (list 1 2 3 4))
(eq? x y) ;#f

(eq? (+ 0.2 0.1) (+ 0.2 0.1)) ;#f

(define z x)
(eq? x z) ;#t

;eqv? ← like eq? for compound objects and = for primitives.

(eqv? (list 1 2)(list 1 2))   ;#f
(eqv? x z)   ;#t
(eqv? (+ 0.2 0.1)(+ 0.2 0.1))  ;#t

;equal? ← value comparison for compound objects.
(equal? x y)    ;#t
(equal? 3 3)    ;#t
(equal? 3 3.0)  ;#f

;_____________________________________________________________

;                             lists

(list 1 2 3 4)
(cons 1 (cons 2 (cons 3 (cons 4 '()))))
; → (1 2 3 4)

; Note, a proper list must terminate with the empty list '()
(cons a (cons b '()))
; → (a . (b . '()))
; → (a b) is an example of a proper list

(cons a b)
; → (a . b) is an example of an improper list.

 (define x (list 1 2 3 4))
 (car x) → 1
 (cdr x) → (2 3 4)
 (car (cdr x)) → (cadr x) 2
 (car (cdr (cdr x))) → (caddr x) 3
 (cadddr x) → 4

;Create a list of numbers between two end points a and b?
(define (range a b)
    (if (= a b) (list a)
        (cons a (range (+ a 1) b))))

(define nums (range 1 5))
nums → (1 2 3 4 5)


;Find ith element in a list?
(define (get-element items i)
    (if (= i 0)
        (car items)
        (get-element (cdr items) (- i 1))))

; Technique known as cdring down.
(define squares (list 1 4 9 16 25))
(get-element squares 3) → 16


;Append two lists together?
(define (append list1 list2)
    (if (null? list1)
        list2
        (cons (car list1)
              (append (cdr list1) list2))))

;Technique known as consing up.
(define odds (list 1 3 5 7 9))
(append squares odds) → (1 4 9 16 25 1 3 5 7 9)


; Scaling numbers?
(define (scale-list items factor)
    (if (null? items)
        '()
        (cons (* (car items) factor)
              (scale-list (cdr items) factor))))

; Technique known as mapping over lists.
(scale-list (list 1 2 3 4 5 ) 10) → ( 10 20 30 40 50)

(define (map procedure items)
    (if (null? items)
        '()
        (cons (procedure (car items))
              (map procedure (cdr items)))))

(map abs (list -10 2.5 -11.6 17)) → (10 2.5 11.6 17)
(map (lambda (x) (* x x)) (list 1 2 3 4)) → ( 1 4 9 16)


; Selecting numbers?
(define (filter predicate sequence)
    (cond ((null? sequence) '())
          ((predicate (car sequence)) 
		       (cons (car sequence) 
                     (filter predicate (cdr sequence))))
          (else (filter predicate (cdr sequence)))))

; Technique known as filtering over lists.
(filter odd? (list 1 2 3 4 5 6 7)) → ( 1 3 5 7)


; Summation of numbers?
(define (reduce operator initial sequence)
    (if (null? sequence)
        initial
        (operator (car sequence) 
                  (reduce operator initial (cdr sequence)))))

; Technique known as reducing lists (also known as accumulating).
(reduce + 0 (list 1 2 3 4 5)) → 15
(reduce * 1 (list 1 2 3 4 5)) → 120

; ________________________________________________________
;                     trees

;trees are lists of lists
(list (list 1 2) (list 3 4 5))

; Scaling all numbers in a tree? E.g., given some tree, multiply every number by 10:
(define aTree (list 1 
                    (list 2) 
                    3 
                    (list 4 
                          (list 5 6) 
                          7)
                    (list (list 8)
                          (list 9)
                          (list 10))))

aTree → (1 (2) 3 (4 (5 6) 7) ((8) (9) (10)))

(define (scale-tree T factor)
    (cond ((null? T) '())
          ((list? (car T)) (cons (scale-tree (car T) factor)
                                 (scale-tree (cdr T) factor)))
          (else (cons (* (car T) factor)
                      (scale-tree (cdr T) factor)))))

(scale-tree aTree 10) → (10 (20) 30 (40 (50 60) 70) ((80) (90) (100)))

(define (tree-map f T)
    (cond ((null? T) '())
          ((list? (car T)) (cons (tree-map f (car T))
                                 (tree-map f (cdr T))))
          (else (cons (f (car T))
                      (tree-map f (cdr T))))))

(tree-map (lambda (x) (* x x)) aTree)				
     → (1 (4) 9 (16 (25 36) 49) ((64) (81) (100)))

; ________________________________________________________
;                     quotes

#| syntax (quote <expr>) → <expr>
 (quote +) → +;
 '+ → +

 apply
 Applies a given procedure to a list of arguments
 Syntax: (apply <procedure> <args>)
 <procedure> is a procedure object
 <args> is a list of arguments
E.g.
|#
 (apply + '(1 2)) -> 3

#| eval
Evaluates a given expression within an environment
Syntax: (eval <expression> <environment>)
<expression>: any s-expression (list or atom) to be evaluated
<environment>: the user's global environment (define env (interaction-environment))

E.g.
|#
(define env (interaction-environment))
(eval '(+ 1 2) env) -> 3

; Note: eval relies on apply if the expression involves procedure application
