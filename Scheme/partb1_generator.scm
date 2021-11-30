#|

write a function called list-generator that takes two functions (a predicate and a mutator) as argument and returns a
list-making function. The returned list-making function should
take three integers (a start value, a stop value, and a step value) as arguments, and return a list of values based on
all of the provided information. The resulting lists should contain the result of applying the mutator function to all
of the integers in the given range for which the predicate function returns true. Any calls that would result in an
invalid or empty range should return an empty list.

|#

(define-syntax rec
  (syntax-rules ()
    ((rec (NAME . VARIABLES) . BODY)
     (letrec ( (NAME (lambda VARIABLES . BODY)) ) NAME))
    ((rec NAME EXPRESSION)
     (letrec ( (NAME EXPRESSION) ) NAME))))

(define (list-generator p m)
  (rec (list-gen x y z)
    (if (= z 0) '()
    (if (and (> x y) (> z 0)) '()
    (if (and (< x y) (< z 0)) '()   
    (if (p x) (cons (m x) (list-gen (+ x z) y z)) (list-gen (+ x z) y z)))))))


;testing

(define f (list-generator (lambda(x)#t) (lambda(x)x)))
   f         ;→ #<procedure:...>
  (f 0 10 3) ;→ (0 3 6 9)

((list-generator even? (lambda(x)(* x 2))) 1 30 3) ;→ (8 20 32 44 56)
((list-generator (lambda(x)#t) (lambda(x)x)) 20 0 -5) ;→ (20 15 10 5 0)












