(define x 'hello)
(define y 'hello)
(eq? x y)

(define s1 "hello")
(define s2 (string-append "hel" "lo"))

(define L '(1 2 3))
(car L)
(cdr L)
(cdddr L)

(define a 1)
(define b 2)
(list a b)
'(a b)

(define t '((1 2)(3 4)5 (6 (7) 8)))

(define exp1 (let ((x 2)(y 5))
               (* 3 (+ x y)(- 10 x))))

(define exp2 '(let ((x 2)(y 5))
               (* 3 (+ x y)(- 10 x))))

(define env (interaction-environment))
(eval '(+ 1 2) env)

(apply + '(1 2))