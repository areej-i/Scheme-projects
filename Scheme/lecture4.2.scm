(define (sqrt x)
	(define (square y)(* y y))

	;(define (good-enough? guess)
	;	(< (abs (- (square guess) x)) 0.001))

    (define (good-enough? guess)
      (let ((tolerance 0.001)
           (difference (abs (- (square guess) x))))
         (< difference tolerance)))

	(define (average x y)
		(/ (+ x y) 2))
	
	(define (improve guess)
		(average guess (/ x guess)))

	(define (sqrt-iteration guess)
		(if (good-enough? guess)
			guess
			(sqrt-iteration (improve guess))))
	
	(sqrt-iteration 1.0))

(let ((a 1)
      (b (+ 1 1)))
  (+ a b))

(define (letExample c)
  (let ((a 10)
        (b 7))
    (+ a b c)))

(letExample 13)


(define (letEx2 x)
  (+ (let ((x 3))           ;x = 3 inside let scope and then 5 outside
       (+ x (* x 10)))
     x))

(letEx2 5)

#|
(define (letEx3)
  (let ((x 3)
        (y (+ x 1)))   ;error, x is undefined
     (* x y)))
|#


(define (letEx3)
  (let ((x 3))
    (let ((y (+ x 1)))
     (* x y))))
(letEx3)

(define (letEx3b)
  (let* ((x 3)
        (y (+ x 1)))
    (* x y)))

(letEx3b)


#|(define (usingDef x)
  (if (> x 0)
      (begin (define y 7)
             (+ x y))))|#
;(usingDef 17)

(define (usingLet x)
  (if (> x 0)
      (let ((y 7))
        (+ x y))))
(usingLet 17)