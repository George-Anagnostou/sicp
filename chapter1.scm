486
(+ 137 349)
(- 1000 334)
(* 5 99)
(/ 10 5)
(+ 2.7 10)
(+ 21 35 12 7)
(* 25 4 12)
(+ (* 3 5) (- 10 6))

(+ (* 3 (+ (* 2 4) (+ 3 5))) (+ (- 10 7) 6))

(+ (* 3
      (+ (* 2 4)
         (+ 3 5)))
   (+ (- 10 7)
      6))

(define size 2)
(* 5 size)

(define pi 3.14159)
(define radius 10)
(* pi (* radius radius))
(define circumference (* 2 pi radius))

(define (square x)
  (* x x))

(square 21)
(square (+ 2 5))
(square (square 3))

; x^2 + y^2
(define (sum-of-squares x y)
  (+ (square x) (square y)))

(sum-of-squares 3 4)

(define (f a)
  (sum-of-squares (+ a 1) (* a 2)))

(f 5)

; conditionals
(define (abs x)
  (cond ((> x 0) x)
        ((= x 0) 0)
        ((< x 0) (- x))))

; (cond (<p1> <e1>)
;       (<p2> <e2>)
;       (<pn> <en>))

; same meaning, different definition
(define (abs2 x)
  (cond ((< x 0) (- x))
        (else x)))

; third way of doing absolute value, demonstrating cond
(define (abs3 x)
  (if (< x 0)
    (- x)
    x))
; (if <predicate> <consequent> <alternative>)
; consequent and alternative must be single expressions
; cond expressions can be multiple expressions

; condition that x be in the range 5 < x < 10
(and (> x 5) (< x 10))

; test that one number is greater than another
(define (>= x y) (or (> x y) (= x y)))
; or
(define (>= x y) (not (< x y)))

; Newton's Method for roots
(define (sqrt-iter guess x)
  (if (good-enough? guess x)
    guess
    (sqrt-iter (improve guess x) x)))

(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))

(define (good-enough? guess x)
  (< (abs (- (square guess) x )) 0.001))

(define (sqrt x)
  (sqrt-iter 1.0 x))

(sqrt 9)
(sqrt (+ 100 37))
(sqrt (+ (sqrt 2) (sqrt 3)))
(square (sqrt 1000))
