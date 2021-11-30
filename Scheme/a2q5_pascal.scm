; Areej Irfan
; 101149642

;a
(define (pascals r c)
  (if (<= c 0) 1
      (if (<= r 0) c
          (/ (* r (pascals (- r 1) (- c 1))) c))))

; test
(pascals 5 0)  ;1
(pascals 5 1)  ;5
(pascals 5 3)  ;10


(define (printTriangle n)
  (define (print r c str)
      (if (and (= r n) (= c n) str) 
       (number->string (pascals r c))))
  (print 0 0 "" n))

(define (printTriangle n)
  (if (and (= r n) (= c n) str)
      (pascals 0 0)))




