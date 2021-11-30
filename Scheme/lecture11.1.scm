

;(define (delay exp)
;  (lambda () exp))   ;error b/c applicative order
;(define (force delayed-exp)
;  (delayed-exp))
 
(define-syntax delay
  (syntax-rules ()
    ((delay exp) (lambda() exp))))
(define-syntax force
  (syntax-rules ()
    ((force exp)(exp))))

(define (foo x y)
  (if (= x 0) x (force y)))
(foo (* 3 0) (delay (/ 3 0)))

