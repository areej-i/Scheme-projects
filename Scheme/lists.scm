(define (last L)
  (if (null? (cdr L)) (car L) (last (cdr L))))
(display "Last")(newline)
(display "(last '(1 2 3 4 5 6 7 8 9)): ")(last '(1 2 3 4 5 6 7 8 9))(newline)


(define (leading n L)
  (if (= n 0)'()
      (cons (car L)(leading (- n 1)(cdr L)))))
(display "Leading")(newline)
(display "(leading 3 '(a b c d e f g h)): ")(leading 3 '(a b c d e f g h))(newline)


(define (my-reverse L)
  (cond ((null? L) L)
        (else (append (my-reverse (cdr L))(list (car L))))))
(display "Reverse")(newline)
(display "(my-reverse '(1 2 3 4 5 6 7 8)): ")(display (my-reverse '(1 2 3 4 5 6 7 8)))(newline)


(define (my-reverse-it L)
  (define (iter forward backward)
    (if (null? forward) backward
        (iter (cdr forward) (cons (car forward) backward))))
  (iter L '()))
(display "Reverse")(newline)
(display "(my-reverse-it '(1 2 3 4 5 6 7 8)): ")(display (my-reverse-it '(1 2 3 4 5 6 7 8)))(newline)

;--recursive--
;(my-reverse '(a b c d e))
;(append (my-reverse '(b c d e))(list a))
;(append (append (my-reverse '(c d e))(list b))(list a))
;(append (append (append (my-reverse '(d e))(list c))(list b))(list a))
;(append (append (append (append (my-reverse '(e))(list d))(list c))(list b))(list a))
;(append (append (append (append (append (my-reverse '())(list e))(list d))(list c))(list b))(list a))
;(append (append (append (append (append '()'(e))(list d))(list c))(list b))(list a))
;(append (append (append (append '(e)'(d))(list c))(list b))(list a))
;(append (append (append '(e d)'(c))(list b))(list a))
;(append (append '(e d c)'(b))(list a))
;(append '(e d c b)'(a))
;'(e d c b a)

;--iterative--
;(my-reverse-it '(a b c d e))
;(iter '(a b c d e) '())
;(iter '(b c d e) '(a))
;(iter '(c d e) '(b a))
;(iter '(d e) '(c b a))
;(iter '(e) '(d c b a))
;(iter '() '(e d c b a))
;'(e d c b a)


;note: append operations considered atomic here... otherwise...
;(define (my-append L1 L2)
;  (cons ((null? L1) L2)
;        (else (cons (car L1) (my-append (cdr L1) L2)))))
;(my-reverse '(a b c d e))
;(append (my-reverse '(b c d e))(list a))
;(append (append (my-reverse '(c d e))(list b))(list a))
;(append (append (append (my-reverse '(d e))(list c))(list b))(list a))
;(append (append (append (append (my-reverse '(e))(list d))(list c))(list b))(list a))
;(append (append (append (append (append (my-reverse '())(list e))(list d))(list c))(list b))(list a))
;(append (append (append (append (append '()'(e))(list d))(list c))(list b))(list a))
;(append (append (append (append '(e)'(d))(list c))(list b))(list a))
;(append (append (append (cons (car '(e))(append '()'(d)))(list c))(list b))(list a))
;(append (append (append (cons e '(d))(list c))(list b))(list a))
;(append (append (append '(e d)'(c))(list b))(list a))
;(append (append (cons (car '(e d))(append (cdr '(e d)) '(c)))(list b))(list a))
;(append (append (cons e (append '(d) '(c)))(list b))(list a))
;(append (append (cons e (cons (car '(d))(append (cdr '(d))'(c))))(list b))(list a))
;(append (append (cons e (cons d (append '() '(c))))(list b))(list a))
;(append (append (cons e (cons d '(c)))(list b))(list a))
;(append (append (cons e '(d c))(list b))(list a))
;(append (append '(e d c)'(b))(list a))
;...and so on...


;; Create a procedure (remove-duplicates) that returns a copy of the given list with no duplicate values. You may assume that all values in the list are integers.
(define (remove-duplicates L)
  (define (remove x lis)
    (cond ((null? lis)lis)
          ((= x (car lis)) (remove x (cdr lis)))
          (else (cons (car lis)(remove x (cdr lis))))))
  (cond ((null? L) L)
        (else (cons (car L) (remove-duplicates (remove (car L)(cdr L)))))))
    
(display "Remove duplicates: ")(newline)
(display "(remove-duplicates '(1 1 2 2 3 4 3 4)): ")(display (remove-duplicates '(1 1 2 2 3 4 3 4)))(newline)
(display "(remove-duplicates '(1 1 1 1)): ")(display (remove-duplicates '(1 1 1 1)))(newline)
(display "(remove-duplicates '(1 2 3)): ")(display (remove-duplicates '(1 2 3)))(newline)
(display "(remove-duplicates '()): ")(display (remove-duplicates '()))(newline)

;alternate solution using filter
(define (rm-dupes L)
    (define (filter f L)
        (cond ((null? L) L)
            ((f (car L)) (cons (car L)(filter f (cdr L))))
            (else (filter f (cdr L)))))

  (cond ((null? L) L)
        (else (cons (car L) (rm-dupes (filter (lambda(x)(not(= x (car L)))) L))))))

(display "Remove duplicates (alternate solution): ")(newline)
(display "(rm-dupes '(1 1 2 2 3 4 3 4)): ") (display (rm-dupes '(1 1 2 2 3 4 3 4)))(newline)
(display "(rm-dupes '(1 1 1 1)): ")(display (rm-dupes '(1 1 1 1)))(newline)
(display "(rm-dupes '(1 2 3)): ")(display (rm-dupes '(1 2 3)))(newline)
(display "(rm-dupes '()): ")(display (rm-dupes '()))(newline)

(define (rm-dupes it))

(define (makeChange cost payment)
  (define (makeList amt denom)
    (if (null? denom) '()
        (cons (inexact->exact (floor (/ amt (car denom))))
              (makeList (- amt (* (car denom)(floor (/ amt (car denom))))) (cdr denom)))))
  
  (if (< payment cost) "Insufficient Funds"
      (makeList (- payment cost) '(1 0.25 0.1 0.05 0.01))))
      

(display "Cost: 10.25, Paid: 50, change: ")(makeChange 10.25 50)
(display "Cost: 12.35, Paid: 20, change: ")(makeChange 12.35 20)
(display "Cost: 5.00, Paid: 5, change: ")(makeChange 5.00 5.00)
(display "Cost: 1.13, Paid: 20, change: ")(makeChange 1.13 20)
(display "Cost: 15, Paid: 10, change: ")(makeChange 15 10)

(define (subsets x)
  (if (null? x) '(())
        (append  (subsets (cdr x))
                       (map (lambda (y) (append (list (car x)) y))
                            (subsets (cdr x))))))

(display "(subsets '(a b c)): ")(subsets '(a b c))



(define (treemap f t)
  (cond ((null? t) '())
        ((list? (car t))(cons (treemap f (car t))(treemap f (cdr t))))
        (else (cons (f (car t))(treemap f (cdr t))))))
		
(display "(treemap square '(2 (2 3)(4 5 (5 6 7)))) => ")
(treemap (lambda(x)(* x x)) '(2 (2 3)(4 5 (5 6 7))))
(display "(treemap double '(2 (2 3)(4 5 (5 6 7)))) => ")
(treemap (lambda(x)(+ x x)) '(2 (2 3)(4 5 (5 6 7))))

(define (tree-size t)
  (cond ((null? t) 0)
        ((list? (car t)) (+ (tree-size (car t))(tree-size (cdr t))))
        (else (+ 1 (tree-size (cdr t))))))
  

      