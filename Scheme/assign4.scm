; Areej Irfan
; 100149642


(define (make-graph)
  (let ((graph '()))

  ; add-node
  (define (append list1 list2)
    (if (null? list1)
        (list list2)
        (cons (car list1)
              (append (cdr list1) list2))))
    
  (define (check graph x)
    (if (null? graph) #t
          (if (eq? (caar graph) x) #f
              (check (cdr graph) x))))
    
  (define (add-node x)
    (if (null? graph) (begin (set! graph (list (list x))) #t)
          (if (eq? (check graph x) #t)
              (begin (set! graph (append graph (list x))) #t)
              #f)))

  ; add-edge
  (define (remove g x)
    (define (rm g g2 x)
      (if (null? g) #f
          (if (eq? (caar g) x) (begin (set! graph (cdr g2)) #t)
              (rm (cdr g) (append (cdr g2) (car g2)) x))))
    (rm g g x))
    
  (define (find-edge g y)
     (if (null? g) #t
     (if (null? (cdr g)) #t
        (if (eq? (caar (cdr g)) y) #f
            (find-edge (cdr (cdr g)) y)))))
    
  (define (find-node g x y)
     (if (null? g) #f 
        (if (not (eq? (caar g) x)) (find-node (cdr g) x y)
            (if (eq? (find-edge (car g) y) #f) #f 
                (if (null? (cdr (car g)))
                (remove (append graph (list x (append (cdr (car g)) y))) x)
                (remove (append graph (list x (append (cadr (car g)) y))) x))))))
    
  (define (add-edge x y)
    (if (null? graph) #f
        (if (eq? (check graph y) #f)
            (find-node graph x y)
            #f)))

  ;remove-node
  (define (remove-node x)
    (define (rm g g2 x)
      (if (null? g) #f
          (if (eq? (caar g) x) (begin (set! graph (cdr g2)) #t)
              (rm (cdr g) (append (cdr g2) (car g2)) x))))
    (rm graph graph x))

    
  ; remove-edge
  (define (rm-edge g g2 x y) ;(b a d)
     (cond ((not (eq? (car g) y)) (rm-edge (cdr g) (append g2 (car g)) x y))
           ((and (null? g2) (null? (cdr g))) (begin (set! graph (append graph (list x))) (remove-node x)))
           ((null? g2) (begin (set! graph (append graph (list x (cdr g)))) (remove-node x)))
           ((null? (cdr g)) (begin (set! graph (append graph (list x g2))) (remove-node x)))
           (else (begin (set! graph (append graph (list x (append g2 (cadr g))))) (remove-node x)))))
    
  (define (find-edge2 g y) ;((b a d))
     (if (null? g) #t
       (if (null? (car g)) #t
        (if (eq? (caar g) y) #f
            (find-edge2 (list (cdr (car g))) y)))))

  (define (remove-edge x y) 
    (define (re g x y)
    (if (null? g) #f
        (if (not (eq? (caar g) x)) (re (cdr g) x y)
            (if (eq? (find-edge2 (cdr (car g)) y) #t) #f
                (rm-edge (cadr (car g)) '() x y)))))
  (re graph x y))

  ; display
  (define (display)
    graph)

  (define (dispatch msg)
    (cond ((eq? msg 'add-node) add-node)
          ((eq? msg 'add-edge) add-edge)
          ((eq? msg 'remove-node) remove-node)
          ((eq? msg 'remove-edge) remove-edge)
          ((eq? msg 'display) display)
          (else #f)))
    dispatch))


;TESTING

(define G (make-graph))
((G 'add-node) 0)        ;=> #t
((G 'add-node) 'b)       ;=> #t
((G 'add-node) '3)       ;=> #t
((G 'add-node) 'd)       ;=> #t
((G 'add-node) 'd)       ;=> #f
((G 'display))           ;=>> 0:    ((0) (b) (3) (d))
;                             b: 
;                             3:
;                             d:
((G 'remove-node) 'c)    ;=> #f
((G 'remove-node) '0)    ;=> #t
((G 'remove-node) '3)    ;=> #t
((G 'display))           ;=>> b:    ((d) (b))
;                             d:

((G 'add-edge) 'a 'b)    ;=> #f
((G 'add-edge) 'b 'c)    ;=> #f
((G 'add-edge) 'b 'b)    ;=> #t

((G 'add-node) 'a)       ;=> #t
((G 'add-edge) 'a 'b)    ;=> #t
((G 'add-edge) 'a 'a)    ;=> #t
((G 'add-edge) 'a 'b)    ;=> #f
((G 'add-edge) 'a 'd)    ;=> #t
((G 'display))           ;=>> a: b a d
;                             b: b 
;                             d:

((G 'remove-edge) 'b 'c) ;=> #f
((G 'remove-edge) 'c 'b) ;=> #f
((G 'remove-edge) 'a 'd) ;=> #t
((G 'display))           ;=>> a: b a
;                             b: b
;                             d:
((G 'add-edge) 'a 'd)    ;=> #t
((G 'remove-edge) 'a 'a) ;=> #t
((G 'display))           ;=>> a: b d
;                             b: b
;                             d:
((G 'add-edge) 'a 'a)    ;=> #t
((G 'remove-edge) 'a 'b) ;=> #t
((G 'display))           ;=>> a: a d
;                             b: b
;                             d:
((G 'remove-edge) 'a '());=> #f
((G 'remove-edge) 'b 'b) ;=> #t
((G 'display))           ;=>> a: a d
;                             b: 
;                             d: