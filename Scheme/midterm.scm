
(+ (* 3 4)(- 5 2 1)(/ 8 2)) ;18

(and (> 18 0) (or (= -1 -1) (>= (* (/ 16 4)(+ 1 (* 3 2)(- 31 29)))(+ (* 3 4)(- 5 2 1)(* 8 2))))) ;true

(let ((l 3)(e (/ 16 16))(t (length '(5 7)))) (if (< l e) t 0))

((lambda (x y) (+ 3 x (* 2 y))) (+ 3 3)(* 2 2)) 
;((lambda (x y) (+ 3 6 (* 2 4))) 6 4)
;((lambda (x y) (+ 3 6 8)) 6 4)
;17

(let ((a (lambda (b c) (* b c))) (b 10) (c 5)) (+ (a 3 2) b c))
;(let ((a (lambda (3 2) (* 3 2))) (b 10) (c 5)) (+ (a 3 2) 10 5))
;(let ((a (lambda (3 2) 6)) (b 10) (c 5)) (+ (a 3 2) 10 5))
;(let (a 6) (b 10) (c 5) (+ 6 10 5))
; 21

(define (x y z)((lambda (y z)(- y z)) z y)) (x 3 5)

(define (foo y) ((lambda (x) y) ((lambda (y)(* y y)) y))) (foo 3)

(((lambda(x)(lambda(y)(+ x y))) 12) ((lambda(z)(* 3 z)) 3))
;(((lambda(x)(lambda(y)(+ 12 9))) 12) 9)
;21


((lambda (a b c)(list '(a b c) (list a b c) a 'b c)) 1 2 3)

(((lambda (a)(lambda (b) '(lambda (c) '(a b c)))) 1) 2)


(let ((a (lambda(x y)(list x y))) (b 2) (c 3)) (list (a b 'c) '(a b c)))
(let ((a (lambda(x y) (list x y))) (b 2) (c 3)) ((a 2 'c) '(a b c)))

