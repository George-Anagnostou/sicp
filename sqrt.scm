(define (sqrt-iter guess x)
  (if (good-enough? guess x)
    guess
    (sqrt-iter (improve guess x)
               x)))

(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))

(define (square x)
  (* x x))

(define (sqrt x)
  (sqrt-iter 1.0 x))

(sqrt 9)
(sqrt (+ 100 37))
(sqrt (+ (sqrt 2) (sqrt 3)))
(square (sqrt 1000))

(sqrt 0)
(sqrt 1e-10)
(sqrt 1e100)


(square 5)
(improve 1.0 9)
(good-enough? 1.0 9)
(sqrt 0)

(define (absolute-diff g1 g2)
  (abs (- g1 g2)))

(define (relative-diff final initial)
  (/ (absolute-diff final initial) initial))

(define (good-enough2? guess x)
  (< (relative-diff guess x) 0.001))

(good-enough2? 1.0 4)
(good-enough2? 2.0 4)
(good-enough2? 4.0 4)

(define (sqrt-iter2 guess x)
  (if (good-enough2? guess x)
    guess
    (sqrt-iter2 (improve guess x)
                x)))

(define (sqrt2 x)
  (sqrt-iter2 1.0 x))

(sqrt2 0)
(sqrt2 4)
