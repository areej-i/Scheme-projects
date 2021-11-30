;Areej Irfan
;101149642

(define-syntax delay
    (syntax-rules ()
        ((delay exp)  (lambda() exp))))
		
(define-syntax force
    (syntax-rules ()
        ((force exp)  (exp))))

(define-syntax stream-cons
    (syntax-rules ()
        ((stream-cons a b)(cons a (delay b)))))

(define (stream-cdr stream)
    (force (cdr stream)))

(define (stream-car stream) 
    (car stream))
		
(define (stream-cdr stream)
    (force (cdr stream)))


;a. [2 marks] Create a procedure (list->stream lis) that makes a stream from a given list.

(define (list->stream lis)
  (if (null? lis) '()
  (stream-cons (car lis) (list->stream (cdr lis)))))

;test
(list->stream '(1 3 5 2)) ;--> (1 . #<procedure:...a3q4_streams.scm:23:18>)
(stream-cdr (list->stream '(1 3 5 2))) ;--> (3 . #<procedure:...a3q4_streams.scm:14:34>)

;b. [3 marks] Create a procedure (stream->list strm n) that makes a list from the first n items of the given stream.

(define (stream->list strm n)
  (define (s->l strm n c)
    (cond ((null? strm) '())
          ((= n c) '())
          ((= c 0) (cons (car strm) (s->l (stream-cdr strm) n (+ c 1))))
          (else (cons (car strm) (s->l (stream-cdr strm) n (+ c 1))))))
 (s->l strm n 0))

;test
(stream->list (list->stream '(1 3 5 2 6)) 2) ;--> (1 3)
(stream->list (list->stream '(a b c 1 2)) 4) ;--> (a b c 1)
(stream->list (list->stream '()) 4) ;--> ()

;c. [2 marks] Create a stream factorials that refers to the infinite stream of consecutive factorial values starting from 0!

(define (factorial n)
  (if (= n 1) 1
      (* n (factorial (- n 1)))))

(define (factorials)
  (define (facts c)
    (if (= c 0) (cons 1 (delay (facts (+ c 1))))
        (cons (factorial c) (delay (facts (+ c 1))))))
  (facts 0))

;test
(factorials) ;--> (1 . #<procedure:...a3q4_streams.scm:57:24>)
(stream->list (factorials) 5)  ;--> (1 1 2 6 24)
(stream->list (factorials) 0)  ;--> ()

;[4 marks] A pseudo-random number generator (prng) can be defined using the following equation:
(define m (expt 2 23))
(define a 22695477)
(define c 1.0)

(define (prng seed)
   (cons (/ (modulo (+ (* a seed) c) m) m) (delay (prng (+ seed 1)))))

;test
(stream->list (prng 4) 4) ;→ (0.8220468759536743 0.5275585651397705 0.2330702543258667 0.9385819435119629)

  
;[3 marks] Create a procedure (random-int r min max) that takes a stream of random numbers r in the range [0,1) and returns a stream of random integers in the range [min,max).


(define (random-int r min max)
  (if (< (* (car r) max) min)
      (cons (+ (car r) (+ min 1)) (delay (random-int (stream-cdr r) min max)))
  (cons (* (car r) max) (delay (random-int (stream-cdr r) min max)))))

;test
(stream->list (random-int (prng 3) 10 20) 5) ;→ (11.116535186767578 16.440937519073486 10.55117130279541 11.233070254325867 18.771638870239258)
(stream->list (random-int (prng 3) 5 10) 5) ;→ (6.116535186767578 8.220468759536743 5.275585651397705 6.233070254325867 9.385819435119629)
