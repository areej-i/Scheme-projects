(define (append list1 list2)
    (if (null? list1)
        list2
        (cons (car list1)
              (append (cdr list1) list2))))

(define (split k L)
  (define (split2 k L L2)
  (if (null? (cdr L)) L
      (if (eq? (cadr L) k) (split2 k (cddr L) (append (list (list (car L))) (list (cddr L)))) (split2 k (cdr L) L2))))
  (split2 k L L))


;(define (split k L)
;  (define (split2 k L L2)
;  (if (null? (cdr L)) L
  ;    (if (eq? (cadr L) k) (split2 k (list (cddr L))) (list (list (car L)))) (split2 k (cdr L) (append (list (append (car L) (cadr L))) L))))
;  (split2 k L L))

 ;(1 0 1 1 0 2 0 0 1)
 ;(0 1 1 0 2 0 0 1) ((1) (1 1 0 2 0 0 1))      
 ;(1 1 0 2 0 0 1) (1)
 ;(1 0 2 0 0 1) ((1)(1 1))

;(split 'c '(a b c d e)) ;→ ((a b)(d e))
;(split '0 '(1 0 1 1 0 2 0 0 1)) ;→ ((1)(1 1)(2)(1))
          
; Q2

(define (sliceHelper list e c)
  (if (= c e) '()
      (cons (car list) (sliceHelper (cdr list) e (+ c 1)))))

(define (slice L s e)
  (cond ((and (= s 0) (= e 0)) '())
        ((and (< s 0) (> e (length L))) L)
        ((< s 0) (slice L 0 e))
        ((> e (length L)) (slice L s (length L)))
        ((> s 0) (slice (cdr L) (- s 1) (- e 1)))
        ((> e 1) (sliceHelper L e 0))))

(slice '(0 1 2 3 4 5 6 7 8 9) 3 8) ;→ (3 4 5 6 7)
(slice '(a b c d e f g h i j) 5 25) ;→ (f g h i j)
(slice '(a b c d e f g h i j) -5 4) ;→ (a b c d)

; Q4
(define (slice2 L at)
  (define (iter n l i)
    (if (or (null? l) (= i at)) (cons (reverse n) l)
      (iter (cons (car l) n) (cdr l) (+ i 1))))
  (iter '() L 0))  

(define (fold_once2 L)
  (define (fold_twice L1 L2 L3)
  (if (null? L1) (reverse (append L2 L3))
  (if (null? L2) (reverse L3)
      (fold_twice (cdr L1) (cdr L2) (append (list (+ (car L1) (car L2))) L3)))))
  (fold_twice (car L) (reverse (cdr L)) '()))

(define (fold_once L)
  (fold_once2 (slice2 L (round (/ (length L) 2)))))

(fold_once '(6 2.5 9 1 5 7 8 4)) ;→ (10 10.5 16 6)
(fold_once '(1 2 3.5 4 5)) ;→ (6,6,3.5)
(fold_once '(1)) ;→ (1)
(fold_once '()) ;-> ()


; Q3
(define (add-into new p L)
  (cond ((null? L)
         (cons new L))
        ((= p 0)
         (cons new L))
        (else 
         (cons (car L) (add-into new (- p 1) (cdr L))))))


(define (removeElem L i)
  (if (null? L) '()
      (if (= i 0) (cdr L)
          (cons (car L) (removeElem (cdr L) (- i 1))))))


(define (infuse L1 a b L2)
    (if (or (or (> a (length L1)) (< a 0)) (> b (length L1))) L1
    (if (and (null? L2) (= b 0)) L1
    (if (> b 0) (infuse (removeElem L1 a) a (- b 1) L2) 
        (if (null? L2) L1
            (infuse (add-into (car L2) a L1) (+ a 1) b (cdr L2)))))))

(infuse '(0 1 2 3 4) 2 7 '()) ;→ '(0 1 2 3 4)
(infuse '(0 1 2 3 4) 7 2 '()) ;→ '(0 1 2 3 4)
(infuse '(0 1 2 3 4) 0 0 '()) ;→ '(0 1 2 3 4)
(infuse '(0 1 2 3 4) -1 0 '()) ;→ '(0 1 2 3 4)

(infuse '(a b c x f) 3 1 '(d e)) ;→ (a b c d e f)
(infuse '(a b c d e) 2 4 '(f g h)) ;→ (a b f g h)
(infuse '(a b c d e) 0 5 '(f g h i j)) ;→ (f g h i j)
(infuse '(0 1 2 3 4 5 6 7) 4 2 '()) ;→ (0 1 2 3 6 7)


;Q5
(define (pos_diffs L)
  (define (pos_diffs2 L R)
    (if (null? L) L
    (if (null? (cdr L)) R
      (if (> (car L) (cadr L)) (pos_diffs2 (cdr L) R) (pos_diffs2 (cdr L) (append R (list (- (cadr L) (car L)))))))))
(pos_diffs2 L '()))


(pos_diffs '(6 2 9 1 5 7 8)) ; → (7 4 2 1)
(pos_diffs '(1)) ;→ ()
(pos_diffs '()) ;→ ()

;Q7

;(define (all_sums2 k n)
;  (define (all_sums k n c1 c2)
;    (if (or (= n 0) (= k 0) '() #f)))
;(all_sums k n 0 0))

(define (countrec k n ans i)
  (if (or (= n 0) (= k 0)) 0
      (if (> i 10) ans
        (if (>= (- k i) 0) (+ ans (countrec (- k 1) (- n 1) 0 0)) (countrec k n ans (+ 1 i))))))
          
(define (all_sums k n)
  (define (all_sums2 k n ans i)
    (if (> i 10) ans
        (if (>= (- k i) 0) (+ ans (countrec (- k 1) (- n 1) 0 0)) (all_sums2 k n ans (+ 1 i)))))
(all_sums2 k n 0 1))
  
  
;(all_sums 2 7) ;→ (16 25 34 43 52 61 70)
;(all_sums 0 0) ;→ ()

;Q8

(define (andmap f X)
    (cond ((null? X) #t)
          ((f (car X))
            (andmap f (cdr X)))
          (else #f)))

(define (my-empty? L)
  (cond ((null? L) #t)
        ((list? L) (andmap my-empty? L))
        (else #f)))
        
(define (prune L)
  (if (null? L) '()
  (if (not (list? L)) L
  (if (my-empty? (car L)) (prune (cdr L))
  (cons (prune (car L)) (prune (cdr L)))))))


(prune '(1 (2 () (3 () 4))()()((())) 5)) ;-> (1 (2 (3 4)) 5)
(prune '(1 2 () 3 4)) ;→ (1 2 3 4)
(prune '((a ()) (b () (c () d)) e)) ;→ (a (b (c d)) e)
(prune '()) ;→ ()


;Q9
(define (case2 L L2 R C)
  (if (and (< (car L) 0) (< (car L) (car L2))) (append R (list C)) R))
      
(define (case L R C)
  (if (null? (cdr L)) (list (car L))
  (if (and (< (car L) 0) (< (car L) (cadr L))) (append R (list C)))))

(define (minima L)
  (define (minima2 L C L2 R)
    (if (null? L) R
    (if (= C 0) (minima2 (cdr L) (+ C 1) L2 (case L R C))
    (if (null? (cdr L)) (case2 L L2 R C)
    (if (and (< (car L) (car L2)) (< (car L) (cadr L))) (minima2 (cdr L) (+ C 1) (cdr L2) (append R (list C)))
        (minima2 (cdr L) (+ C 1) (cdr L2) R))))))
(minima2 L 0 L '()))
  
(minima '(-1 5 9 3 5 4 8)) ;→ (0 3 5)
(minima '(-42 25 -1)) ;→ (0 2)
(minima '())

