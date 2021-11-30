; Areej Irfan
; 101149642

(define (palindrome? s)
    (define (same? f b)
        (if (string=? f b) #t #f)) ; t t

    (define (pali? front n s) ;1 6
       (if (not (same? (substring s front (+ front 1)) (substring s (- n 1) n))) #f ; 12  56
           (if (not (= (+ front 1) (string-length s))) ;6 7
               (pali? (+ front 1) (- n 1) s) #t))) ;6 1

    (pali? 0 (string-length s) s))


; test cases
(palindrome? "tasadasocat") ;#f
(palindrome? "racecar")     ;#t
(palindrome? "race car")    ;#f
(palindrome? "r")           ;#t



(define (k-palindrome? s num)
  
    (define (same? f b)
        (if (string=? f b) #t #f)) 
      
    (define (pali? front b s num) ;  
        (if (not (same? (substring s front (+ front 1)) (substring s (- b 1) b)))
            (if (> num 0) (pali? front (- b 1) (substring s (+ front 1) b) (- num 1)) #f) 
                (if (not (>= (+ front 1) (string-length s))) 
                    (pali? front (- b 2) (substring s (+ front 1) (- b 1)) num) #t))) 

    (pali? 0 (string-length s) s num))  ; 


(k-palindrome? "fracehcar" 2)  ;#t
(k-palindrome? "rafcehcar" 0)  ;#f
(k-palindrome? "r" 0)          ;#t
