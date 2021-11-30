; CHAPTER 4


(define ex1 (cons 1 (cons (cons 2 (cons 3 4)) 5)))

(car ex1) ;=> 1
(cdr ex1) ;=> ((2 . (3 . 4)) . 5)
(car (cdr ex1)) ; => (2 . (3 . 4))
;(car (car ex1)) ;=> error
(car (car (cdr ex1))) ;=> 2

(cdr (cdr ex1)) ; => 5

(cadr ex1) ;=> (car (cdr ex1)) => (2 . (3 . 4))
(caadr ex1) ;=> 2

;(define (make-rat num denom)
;  (let ((g (gcd num denom)))
;    (cons (/ num g) (/ denom g))))
;(define (numerator rat)
;  (car rat))
;(define (denominator rat)
;  (cdr rat))

(define (make-rat n d)
  (let ((g (gcd n d)))
    (lambda (x)(if (= x 0)(/ n g)(/ d g)))))
(define (numerator r)
  (r 0))
(define (denominator r)
  (r 1))

(define one-third (make-rat 1 3))
(numerator one-third)
(denominator one-third)

(define (print-rat x)
  (display (numerator x))
  (display "/")
  (display (denominator x))
  (newline))

(define (add-rat r1 r2)
  (make-rat (+ (* (numerator r1) (denominator r2))
               (* (numerator r2) (denominator r1)))
            (* (denominator r1)(denominator r2))))
            