; Areej Irfan
; 101149642

(define (hyper x))


(define my-mult (hyper +))
(my-mult 3 4) ; 12
			
(define my-exp (hyper my-mult))
(my-exp 2 4) ; 16

(define my-tetra (hyper my-exp))
(my-tetra 2 4) ; 65536