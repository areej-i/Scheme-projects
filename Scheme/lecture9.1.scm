(define (range a b)
  (if (> a b) '()
     (cons a (range (+ a 1) b))))

;(range 1 5)
;(cons 1 (range (+ 1 1) 5))
;(cons 1 (range 2 5))
;(cons 1 (cons 2 (range 3 5)))
;(cons 1 (cons 2 (cons 3 (range 4 5))))
;(cons 1 (cons 2 (cons 3 (cons 4 (range 5 5)))))
;(cons 1 (cons 2 (cons 3 (cons 4 (cons 5 (range 6 5))))))
;(cons 1 (cons 2 (cons 3 (cons 4 (cons 5 '())))))
;(cons 1 (cons 2 (cons 3 (cons 4 (list 5)))))
;(1 2 3 4 5)

(define (range-it a b)
  (define (helper i L)
    (if (< i a) L
        (helper (- i 1)(cons i L))))
  (helper b '()))

;(range-it 1 4)
;(helper 4 '())
;(helper (- 4 1)(cons 4 '()))
;(helper 3 (list 4))
;(helper (- 3 1)(cons 3 (list 4)))
;(helper 2 (list 3 4))
;(helper 1 (list 2 3 4))
;(helper 0 (list 1 2 3 4))
;(1 2 3 4)

(define (get-element L i)
  (if (= i 0) (car L)
   (get-element (cdr L) (- i 1))))

;(get-element (list 1 2 3 4) 2)
;(get-element (cdr (list 1 2 3 4)) (- 2 1))
;(get-element (list  2 3 4) 1)
;(get-element (list 3 4) 0)
;(car (list 3 4))
;3

(define (length L)
  (if (null? L) 0
      (+ 1 (length (cdr L)))))


;(length (list 1 2 3))
;(+ 1 (length (cdr (list 1 2 3))))
;(+ 1 (length (list 2 3)))
;(+ 1 (+ 1 (length (cdr (list 2 3)))))
;(+ 1 (+ 1 (length (list 3))))
;(+ 1 (+ 1 (+ 1 (length '()))))
;(+ 1 (+ 1 (+ 1 0)))
;...
;3

(define L1 (list 1 2 3))
(define L2 (list 4 5 6))
(define L3 (cons L1 L2))

(define (append L1 L2)
  (if (null? L1) L2
      (cons (car L1)
            (append (cdr L1) L2))))

;(append (list 1 2)(list 3 4))
;(cons (car (list 1 2)) (append (cdr (list 1 2)) (list 3 4)))
;(cons 1 (append (list 2)(list 3 4)))
;(cons 1 (cons 2 (append '() (list 3 4))))
;(cons 1 (cons 2 (list 3 4)))
;...
;(list 1 2 3 4)

(define (scale-list L x)
  (if (null? L) '()
      (cons (* x (car L))
            (scale-list (cdr L) x))))

;(scale-list (list 1 2 3) 10)
;(cons (* 10 (car (list 1 2 3))) (scale-list (cdr (list 1 2 3)) 10))
;(cons (* 10 1) (scale-list (list 2 3) 10))
;(cons 10 (scale-list (list 2 3) 10))
;(cons 10 (cons 20 (scale-list (list 3) 10)))
;(cons 10 (cons 20 (cons 30 (scale-list '() 10))))
;(cons 10 (cons 20 (cons 30 '())))
;...
;(list 10 20 30)

(define (map f L)
  (if (null? L) '()
      (cons (f (car L))
            (map f (cdr L)))))

(define (filter predicate L)
  (cond ((null? L) '())
        ((predicate (car L)) (cons (car L)
                                   (filter predicate (cdr L))))
        (else (filter predicate (cdr L)))))

;(filter odd? (list 1 2 3 4 5))
;(odd? (car (list 1 2 3 4 5)))
;(cons (car (list 1 2 3 4 5)) (filter odd? (cdr (list 1 2 3 4 5))))
;(cons 1 (filter odd? (list 2 3 4 5)))
;(cons 1 (filter odd? (list 3 4 5)))
;(cons 1 (cons 3 (filter odd? (list 4 5))))
;(cons 1 (cons 3 (filter odd? (list 5))))
;(cons 1 (cons 3 (cons 5 (filter odd? '()))))
;(cons 1 (cons 3 (cons 5 '())))
;..
;(list 1 3 5)

(define (reduce operator initial lis)
  (if (null? lis) initial
      (operator (car lis)
                (reduce operator initial (cdr lis)))))

;(reduce + 0 (list 1 2 3))
;(+ 1 (reduce + 0 (list 2 3)))
;(+ 1 (+ 2 (reduce + 0 (list 3))))
;(+ 1 (+ 2 (+ 3 (reduce + 0 '()))))
;(+ 1 (+ 2 (+ 3 0)))
;..
;6
0
(define (reduce-it op total lis)
  (if (null? lis) total
      (reduce-it op (op total (car lis)) (cdr lis))))

;(reduce-it + 0 (list 1 2 3))
;(reduce-it + (+ 0 1) (cdr (list 1 2 3)))
;(reduce-it + 1 (list 2 3))
;(reduce-it + 3 (list 3))
;(reduce-it + 6 '())
;6



(reduce / 1 (list 10 2 2))   ;=> (10 / (2 / (2/1))) => 10   ;rFold
(reduce-it / 1 (list 10 2 2));=> ((1 / 10)/2)/2  => 1/40    ;lFold