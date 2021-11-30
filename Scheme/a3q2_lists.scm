;Areej Irfan
;101149642



;[2 marks] Create a function (repeat x n) that returns a list that contains n copies of the value x. Where n is some non-negative integer, and x is any value.

(define (repeat x n)
  (define (rp x n c)
    (if (= c (- n 1)) (cons x '())
    (cons x (rp x n (+ c 1)))))
  (rp x n 0))

(repeat 'a 5) ;→ (a a a a a)

;test
(repeat 'apple 2) ;→ (apple apple)
(repeat 3 2) ;→ (3 3)

;[3 marks] Create a function (alternate list1 list2) that creates a list by alternating elements from the two given input lists.
;The result should contain all elements from both input lists in the provided order.


(define (alternate list1 list2)
     (if (null? list1)
        list2
        (cons (car list1) 
              (alternate list2 (cdr list1)))))

;test
(alternate '(a c e g) '(b d f)) ;→ (a b c d e f g)
(alternate '(1 3 5 7) '(2 4 6 8)) ;→ (1 2 3 4 5 6 7 8)


;[3 marks] Create a procedure (count x L) that returns the number of instances of the value x in the list L.

(define (count x L)
  (define (cnt x L c)
    (if (null? L) c
        (if (eqv? (car L) x) (cnt x (cdr L) (+ c 1))
            (cnt x (cdr L) c))))
  (cnt x L 0))

(count 3 '(1 4 3 6 2 3 3 1 4 3 5 7)) ;→ 4
(count 'b '(4 b a 3 2 c b 1 b 2 d a)) ;→ 3


;[4 marks] Create a procedure (mode L) that returns the most common value in the list L. In the case of a tie return the value closest to the front
; of the list (ie the first one encountered). You may not use the built-in max or min functions for this problem.


(define (mode L)
  (define (helper L largest cnt)
    (if (null? L) largest
    (if (> (count (car L) L) cnt) (helper (cdr L) (car L) (count (car L) L)) (helper (cdr L) largest cnt))))
  (helper L (car L) 0))


(mode '(a b a b b d d a b c a b)) ;→ b
(mode '(7 b 7 3 7 c b 1 b 7 d a)) ;→ 7


;[6 marks] Create a procedure (decreasing L) that takes a list of numbers as input and returns a list of all of the consecutive decreasing subsequences in the input list.
;Note: a decreasing subsequence is a sequence of two or more numbers (... ni ni+1 ...) such that ni > ni+1.

;(define (decreasing L)
  ;(if (and (< (car L) (cadr L)) (< (cadr L) (caadr L))) (decreasing (list (list (car L) (cadr L) (caadr L)) (list (cadr L) (caadr L)))

;


(define (helperf L lis) ;(9 7 4 8 6 3) (7 4 8 6 3)
  (if (null? (cdr lis)) '()
     (if (> (car lis) (cadr lis))
         (cons (car lis) (helperf L (cdr lis)))  ; (cons 7 (4 8 6 3))  
         (cons (car L) (decreasing (cdr L))))))  ; cons 9 (cons 7 (cons 4 (8 6 3)  -> 9 . 7 . 4 
         (cons (car L) (helping (cdr L)))
  
(define (decreasing L)
    (if (null? L) '()
    (if (null? (cdr L)) '()
    (if (> (car L) (cadr L)) (list (helper L (cdr L))) ;(9 7 4 8 6 3)
        (decreasing (cdr L))))))

  
;(decreasing '(3 6 8 9 7 4 8 6 3)) ;→ ((9 7 4) (7 4) (8 6 3) (6 3))
;(decreasing '(7 6 5 4 8 5 2 5 1 5 2 1)) → ((7 6 5 4) (6 5 4) (5 4) (8 5 2) (5 2) (5 1) (5 2 1) (2 1))
;(decreasing '(1 2 3 3 3 4 5)) → ()

