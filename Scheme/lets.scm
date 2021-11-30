;let -> lambda
;question
(let ((varname 2))
  (* varname 3))
;answer
((lambda (varname) (* varname 3)) 2)


;question
(let ((a (+ 3 5))
      (b (* 4 2))
      (c (- 12 8)))
  (+ (* 3 a)(* 2 b b)(/ c 2)))
;answer
((lambda (a b c)(+ (* 3 a)(* 2 b b)(/ c 2))) (+ 3 5)(* 4 2)(- 12 8))


;question
(let ((x (* 12 4))
      (y (+ 8 16)))
  (let ((z 4))
    (* z (+ (* 3 x) y))))
;answer
((lambda (x y)
  ((lambda (z) (* z (+ (* 3 x) y))) 4))
    (* 12 4)(+ 8 16))



;lambda -> let
;q
((lambda (x)(* x x)) 4)
;a
(let ((x 4)) (* x x))


;q
((lambda (i j k) (cons (+ (* 3 i) 2) (+ j (* 2 k))))  (caddr '(5 6 2 1 3)) (cadar '((4 12 3) 2 1 0)) (car '(1 2 3)))
;a
(let ((i (caddr '(5 6 2 1 3)))
    (j (cadar '((4 12 3) 2 1 0)))
    (k (car '(1 2 3))))
    (cons (+ (* 3 i) 2)(+ j (* 2 k))))


;q
((lambda (a b c) (/ (+ b c) a)) ((lambda(x)(* x x))(+ 3 2)) ((lambda (x y)(+ (* 3 y) x)) (* 2 4)(- 10 4))(+ 10 2))
;a			            
(let ((a (let ((x (+ 3 2)))(* x x)))
      (b (let ((x (* 2 4))(y (- 10 4))) (+ (* 3 y) x)))
      (c (+ 10 2))) 
   (/ (+ b c) a))