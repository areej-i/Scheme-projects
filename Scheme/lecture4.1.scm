(define (sqrt x)

  (define (square y)
    (* y y))
  
  (define (good-enough? guess)
    (< (abs (- (square guess) x)) 0.001))
  
  (define (average x y)
    (/ (+ x y) 2))
  
  (define (improve guess)
    (average guess (/ x guess)))
  
  (define (sqrt-iteration guess)
    (if (good-enough? guess)
        guess
        (sqrt-iteration (improve guess))))
  
  (sqrt-iteration 1.0 x))


(define (foo a)
  (define (bar b)
    (+ a b))
  (bar (* a 2)))
(foo 3)

;(foo 3)
;(define (bar b)(+ 3 b)) (bar (* 3 2))
;(bar (* 3 2))
;(bar 6)
;(+ 3 6)
;9

(define (g x)
  (define (f y)
    (+ x y))
  (+ (f 3)(f 4)))

(g 5)

;(g 5)
;(define (f y)(+ 5 y))(+ (f 3)(f 4))
;(+ (f 3)(f 4))
;(+ (+ 5 3)(+ 5 4))
;(+ 8 9)
;17

(define (outer x)
  (define (middle y)
    (define (inner x)
      (+ x y))
    (inner (+ x y)))
  (middle (+ x 1)))
(outer 3)
