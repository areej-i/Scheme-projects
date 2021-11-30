; CHAPTER 5

(define (foo x y)
    (if (= x 0) x y))
	
; (foo (* 3 0) (/ 3 0))    will give error
(foo (* 3 0)(lambda() (/ 3 0)))

#|
  Recall: special forms are expressions that follow evaluation rules outside of Scheme's general rule of evaluation.
  Special forms are like functions, but their arguments are not evaluated using applicative order
  E.g.: lambda, cond, define, quote

(define-syntax <newkeyword>
   (syntax-rules (<other-needed-keywords>)
       (<pattern> <template>)
       ...	
       (<pattern> <template>)))

  newkeyword is the name of the special form to be created
  other-needed-keywords are auxiliary keywords for the special form (e.g. else for cond), typically left empty.
  pattern is the pattern to be matched in the input to be transformed
  template is the corresponding output structure
  Multiple patterns/template pairs are allowed, patterns must begin with the newkeyword
  Parameters in the pattern may appear in the template.
|#


;Using macros we can create special forms for delay and force
(define-syntax delay
    (syntax-rules ()
        ((delay exp)  (lambda() exp))))
		
(define-syntax force
    (syntax-rules ()
        ((force exp)  (exp))))

(define (foo x y)
  (if (= x 0) x (force y)))

(foo (* 3 0) (delay (/ 3 0)))

#|
  Streams rely on the special forms (delay <exp>) and (force delayed-object).
  Delay returns a delayed object – promise to evaluate <exp> in the future.
  Force unwraps the delayed object, performing the evaluation of the expression inside.
  delay and force are built-in to R5RS.

  A stream is a pair of the form: (cons a (delay b))
  Where a is an element of a sequence and b is some expression.
|#

(cons 1 (delay (+ 1 1)))  ;-> (1 . #<promise>)
(stream-cons 1 (stream-cons 2 (stream-cons 3 '()))) ;→ (1 . #<promise>)

; Special form stream-cons:
(define-syntax stream-cons
    (syntax-rules ()
        ((stream-cons a b)(cons a (delay b)))))

(stream-cons 1 (delay (/ 3 0)))  ;-> 1 . promise

(define (stream-car stream) 
    (car stream))
		
(define (stream-cdr stream)
    (force (cdr stream)))

;________________________________________________________

;Representing sequences

;Generate a stream of numbers
Stream version of enumerating an interval (creates a stream from low to high)
(define (stream-range low high)
    (if (> low high)
        the-empty-stream
        (stream-cons low (stream-range (+ low 1) high))))
So, (stream-range 1 100) → (1 . (delay (stream-range 2 100)))
Then, (stream-cdr (stream-range 1 100)) → (2 . (delay (stream-range 3 100)))

;Find element n of a stream
Cdr'ing down a stream should work the same as it does for lists
(define (stream-ref n stream)
    (if (= n 0)
        (stream-car stream)
        (stream-ref (- n 1) (stream-cdr stream))))

;Apply procedure to values in a stream
Even higher-level stream functions (like filter) should work the same as their list counterparts
(define (stream-filter predicate stream)
    (cond ((null? stream) '())
          ((predicate (stream-car stream))
              (stream-cons (stream-car stream)
                           (stream-filter predicate 
                                          (stream-cdr stream))))
          (else (stream-filter predicate (stream-cdr stream)))))

;Finally, we can compute the second prime between 10000 and 1000000
Using the same operations as lists:
(stream-car 
    (stream-cdr
        (stream-filter prime? 
            (stream-range 10000 1000000))))
