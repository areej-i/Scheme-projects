(define (repl)
  
  (define env (interaction-environment))
  
  (display "Read-Eval-Print-Loop (type stop to end): ")
  (newline)
  

  (define (loop)
    (let ((exp (read)))
      (newline)
      (display "input: ")
      (display exp)
      (newline)
      (if (eq? exp 'stop)
        (display "bye!")
        (begin
          (display "result: ")
          (display (eval exp))
          (newline)
          (loop)))))
  (loop))
