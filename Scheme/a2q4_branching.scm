; Areej Irfan
; 101149642

;f(n) = n, if n<3
; f(n) = 3f(n-1) + 2f(n-2) + f(n-3), otherwise

(define (f2 n)
     (if (< n 3)
      n
     (+ (* 3 (f2 (- n 1))) (* 2 (f2 (- n 2))) (f2 (- n 3)))))

(f2 5)

;(f2 5)
;(* 3 (f2 (- n (+ 1 c)))))


; iterative process
(define (f n)
  (define (func total n c)
      (if (= c n)
          total
          (if (< n 3)
               n
              (func (+ (* (- 3 c) (- n (+ c 1))) total) n (+ c 1)))))
  (func 0 n 0))


(f 5)
;(f 5)
;(func 0 5 0)
;(func 12 5 1)
;(func 18 5 2)
;(func 20 5 3)
;(func 20 5 4)
; 20