(define (prime? n)
  (define (iter i)
    (cond ((>= i (/ n 2)) #t)
          ((= 0 (modulo n i)) #f)
          (else (iter (+ i 1)))))
  (iter 2))

(define (filter pred lis)  ;iterative version
  (define (iter L result)
    (cond ((null? L) result)
          ((pred (car L)) (iter (cdr L) (append result (list (car L)))))
          (else (iter (cdr L) result))))
  (iter lis '()))


(define (range a b)
  (define (iter i result)
    (cond ((< i a) result)
          (else (iter (- i 1) (cons i result)))))
  (iter b '()))

#|Lists:
   range: (10000 10001 10002 ... 999999 1000000); ~1 million steps, O(n)
   filter: (10007 10009 ... +~75k values); ~1million steps x prime? = O(n^2)
   cdr: (10009 ... +~75k values) ; 1 step, but still storing ~75k values
   car: 10009; 1 step
|#

#|Streams:
    range: (10000 . #<promise>); 1 step
    filter: (10007 . #<promise>) ;~8 steps, but O(n) b/c prime?
    cdr: (10009 . #<promise>) ;2 steps
    car: 10009                ;1 step
|#

;(define (stream-cons a b)   ;error! b/c applicative order
;  (cons a (delay b)))

;(stream-cons 1 (+ 1 1))
;(stream-cons 1 2)
;(cons 1 (delay 2))
;(1 . #<promise>)

(define-syntax stream-cons
  (syntax-rules ()
    ((stream-cons a b) (cons a (delay b)))))
(define (stream-car strm)
  (car strm))
(define (stream-cdr strm)
  (force (cdr strm)))

(define (stream-range a b)
  (if (> a b) '()
      (stream-cons a (stream-range (+ a 1) b))))

(define (range a b)
  (if (> a b) '()
      (cons a (range (+ a 1) b))))

;(stream-range 1 100))
;(stream-cons 1 (stream-range (+ 1 1) 100))
;(cons 1 (delay (stream-range (+ 1 1) 100)))
;(1 . [stream-range (+ 1 1) 100])

;(stream-cdr (stream-range 1 100))
;(stream-cdr (1 . [stream-range (+ 1 1) 100]))
;(force (cdr (1 . [stream-range (+ 1 1) 100])))
;(force [stream-range (+ 1 1) 100])
;(stream-range (+ 1 1) 100)
;(stream-range 2 100)
;(stream-cons 2 (stream-range (+ 2 1) 100))
;...


(define (stream-ref n strm)
  (cond ((eq? strm '()) #f)
        ((= n 0) (stream-car strm))
        (else (stream-ref (- n 1) (stream-cdr strm)))))

;(stream-ref 3 (stream-range 5 10))
;(stream-ref 3 (5 . [stream-range 6 10]))
;(stream-ref (- 3 1) (stream-cdr (5 . [stream-range 6 10])))
;(stream-ref 2 (force (cdr (5 . [stream-range 6 10]))))
;(stream-ref 2 (force [stream-range 6 10]))
;(stream-ref 2 (stream-range 6 10))
;(stream-ref 1 (stream-range 7 10))
;(stream-ref 0 (stream-range 8 10))
;(stream-car (8 . [stream-range 9 10]))
;8

(define (stream-filter pred strm)
  (cond ((null? strm) '())
        ((pred (stream-car strm))
         (stream-cons (stream-car strm)
                      (stream-filter pred (stream-cdr strm))))
        (else (stream-filter pred (stream-cdr strm)))))

;(stream-filter even? (stream-range 1 10))
;(stream-filter even? (1 . [stream-range 2 10]))
;(stream-filter even? (stream-cdr (1 . [stream-range 2 10])))
;(stream-filter even? (stream-range 2 10))
;(stream-filter even? (2 . [stream-range 3 10]))
;(stream-cons (stream-car (2 . [stream-range 3 10])) (stream-filter even? (stream-cdr (2 . [stream-range 3 10])))
;(cons (stream-car (2 . [stream-range 3 10])) (delay (stream-filter even? (stream-cdr (2 . [stream-range 3 10])))))
;(cons 2 [stream-filter even? (stream-cdr (2 . [stream-range 3 10]))])
;(2 . #<promise>)


(define (ints-starting-from i)
  (stream-cons i (ints-starting-from (+ i 1))))

(define sevens
  (stream-filter (lambda(x)(= (modulo x 7) 0)) (ints-starting-from 1)))

                                               
