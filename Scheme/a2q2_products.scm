; Areej Irfan
; 101149642

; a
(define (product a b term next)
    (if (> a b)
        1
        (* (term a)
           (product (next a) b term next))))

;test cases
(product 3 5 (lambda(x)x) (lambda(x)(+ x 1)))   ;= 60
(product 1 5 (lambda(x)x) (lambda(x)(+ x 1)))   ;=120


;b
(define (product-it a b term next)
  (define (prod a b next product)
    (if (> a (- b 1))
        product
        (prod (next a) b next (* product (next a)))))
  (prod a b next a))

;test cases
(product-it 1 5 (lambda(x)x) (lambda(x)(+ x 1))) ;=120
(product-it 3 5 (lambda(x)x) (lambda(x)(+ x 1))) ;=60

;c
(product 2 20 (lambda(x)(- (* x x x) (* x x))) (lambda(x)(+ x 1)))   ;720018831126277480427380151560144480842547200000000000
(product 1 15 (lambda(x)(* (+ (* 2 x) 1) (+ (* 2 x) 1))) (lambda(x)(+ x 1)))   ;36825143286290325050565453237890625
(product 1 5 (lambda(x)(* 3 x (* x x))) (lambda(x)(+ x 1))) ;419904000









