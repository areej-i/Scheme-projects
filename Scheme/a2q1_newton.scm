; Areej Irfan
; 101149642

#|[5 marks] Newton's method for cube roots is based on the fact that if y is an approximation to the cube root of x,
then a better approximation is given by the value: (x/y2+2y)/3
Use this formula to implement a cube-root procedure analogous to the square-root procedure from the lecture notes.
Your code should use nested functions and free variables wherever possible.|#

(define (cubert x)
	(define (cube y)(* y y y))

	(define (good-enough? guess x)
		(< (abs (- (cube guess) x)) 0.001))

	(define (average x y)
		(/ (+ x (* 2 y)) 3))
	
	(define (improve guess x)
		(average guess (/ x ( * guess guess))))

	(define (cubert-iteration guess x)
		(if (good-enough? guess x)
			guess
			(cubert-iteration (improve guess x) x)))
	
	(cubert-iteration 1.0 x))

;test cases
(cubert 2) ; = 1.259...
(cubert 1.5) ; = 1.144... 

#|[2 marks] Consider the (new-if) procedure as shown below. Replace the standard if inside cbrt-iter with a call to new-if instead. Does the new version work?
Explain why or why not.|#


(define (cubert2 x)
	(define (cube y)(* y y y))

	(define (good-enough? guess x)
		(< (abs (- (cube guess) x)) 0.001))

	(define (average x y)
		(/ (+ x (* 2 y)) 3))
	
	(define (improve guess x)
		(average guess (/ x ( * guess guess))))

	(define (new-if predicate consequent alternate)
                  (cond (predicate consequent)
                        (else alternate)))
  
        (define (cubert-iteration guess x)
		(new-if (< x 0) (- x) x))
	
	(cubert-iteration 1.0 x))

#| This new version did not work, it only returned the same value as given because none of the other functions were called within this procedure
so no better estimates were made to get the precise answer|#

(cubert2 2)



