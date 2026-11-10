

; f(n) recursively
(define (f n)
  (if (< n 3)
    n
    (+ (f (- n 1)) 
       (* 2 (f (- n 2))) 
       (* 3 (f (- n 3))))))
(f 5)

; f(n) iteratively
(define (f n)
  (define (iter a b c counter)
    (if (= counter 0)
      a
      (iter b c (+ c (* 2 b) (* 3 a)) (- counter 1))))
  (iter 0 1 2 n))
(f 5)

; Exercise 1.12
; Pascal's Triangle
;     1
;    1 1
;   1 2 1
;  1 3 3 1
; 1 4 6 4 1

(define (pascal i j)
  (if (or (= j 0) (= j i))
    1
    (+ (pascal (- i 1) (- j 1))
       (pascal (- i 1) j))))
(pascal 3 0)
(pascal 3 1)
(pascal 3 2)
(pascal 3 3)
