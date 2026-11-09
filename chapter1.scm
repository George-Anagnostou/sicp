; Chapter 1 Notes and Exercises

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

; Consolidating the notes and procedures of the
; sqrt family of functions and procedures
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

; it may be better to define sqrt with it's related functions
; nested within it via lexical scoping to improve clarity, reduce
; potential namespace clashing, and group related functionality
(define (sqrt x)
  (define (good-enough? guess)
    (< (abs (- (square guess) x)) 0.001))
  (define (improve guess)
    (average guess (/ x guess)))
  (define (sqrt-iter guess)
    (if (good-enough? guess)
      guess
      (sqrt-iter (improve guess))))
  (sqrt-iter 1.0))

; Exercise 1.1
; My predictions are written as comments after the expression

10
; 10

(+ 5 3 4)
; 12

(- 9 1)
; 8

(/ 6 2)
; 3

(+ (* 2 4) (- 4 6))
; 6

(define a 3)
(define b (+ a 1))
(+ a b (* a b))
; 19

(= a b)
; false

(if (and (> b a) (< b (* a b)))
  b
  a)
; 4

(cond ((= a 4) 6)
      ((= b 4) (+ 6 7 a))
      (else 25))
; 16

(+ 2 (if (> b a) b a))
; 6

(* (cond ((> a b) a)
         ((< a b) b)
         (else -1))
   (+ A 1))
; 16

; Exercise 1.2
(/ (+ 5 4 
   (- 2 
      (- 3 
         (+ 6 
            (/ 4 5)))))
   (* 3 
      (- 6 2) (- 2 7)))

; Exercise 1.3
; define a procedure that takes 3 numbers as arguments
; and returns the sum of the squares of the two larger numbers
(define (square x)
  (* x x ))

(define (sum-of-squares x y)
  (+ (square x) (square y)))

(define (sum-squares-big2 a b c)
  (cond ((and (>= a b) (>= b c)) 
         (sum-of-squares a b))
        ((and (>= c b) (>= b a)) 
         (sum-of-squares c b))
        ((and (>= a b) (>= c b))
         (sum-of-squares a c))))
(sum-squares-big2 1 2 3)
(sum-squares-big2 3 2 1)

; Exercise 1.4
(define (a-plus-abs-b a b)
  ((if (> b 0) + -) a b))
(a-plus-abs-b 5 10)
(a-plus-abs-b 5 -10)
; The expression (if (> b 0) + -) evaluates to "+" or "-",
; and this expression becomes the operator, resulting in either
; (+ a b) or (- a b). So, the "+" and "-" act as operands in the if expression,
; and operators afterwards.

; Exercise 1.5
(define (p) (p))
(define (test x y)
  (if (= x 0) 0 y))
(test 0 (p))
; Applicative-Order Evaluation
; 1: (test 0 (p))
; 2: (if (= 0 0) 0 p)
; 3: infinite recursion or error trying to evaluate p?

; Normal-Order Evaluation
; 1: (test 0 (p))
; 2: (if (= 0 0) 0 p)
; 4: 0

; Exercise 1.6
; why is `if` a special form?
(define (new-if predicate then-clause else-clause)
  (cond (predicate then-clause)
        (else else-clause)))

(new-if (= 2 3) 0 5)
(new-if (= 1 1) 0 5)

; (define (sqrt-iter guess x)
;   (new-if (good-enough? guess x)
;           guess
;           (sqrt-iter (improve guess x) x)))
; (sqrt-iter 1.0 10)

; the issue would be an error, trying to infinitly recurse down the
; else branch in the applicative-order resolution

; Exercise 1.7
(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))

(define (good-enough? guess x)
  (< (abs (- (square guess) x )) 0.001))

(define (sqrt-iter guess x)
  (if (good-enough? guess x)
    guess
    (sqrt-iter (improve guess x) x)))

(define (sqrt x)
  (sqrt-iter 1.0 x))

(sqrt 9)
(sqrt (+ 100 37))

(sqrt 0)
(sqrt 1e-20)

; The problem with the current good-enough? test is that small numbers
; end up being less than the tolerance, so our good-enough? is too big,
; and with large numbers we have the opposite problem: good-enough? is too
; coarse. For example:
(sqrt 0.0001)
(sqrt 1000000000)
(sqrt 0)
(sqrt 1e-20)
(sqrt 1e14)
(sqrt 1e21)

(define (better-enough? g1 g2)
  (or (zero? g2)
      (< (/ (abs (- g2 g1)) g1)
         0.001)))

(define (sqrt-iter guess x)
  (if (better-enough? guess x)
      guess
      (sqrt-iter (improve guess x) x)))

(sqrt 0)
(sqrt 1e-20)
(sqrt 1e14)
(sqrt 1e21)

; 1.8 Newton's method for cube roots
(define (cube x) (* x x x))
(define (cbrt-iter guess x)
  (if (good-enough? guess x)
    guess
    (cbrt-iter (improve guess x) x)))
(define (improve guess x)
  (/ (+ (/ x (square guess)) 
        (* 2 guess)) 
     3))
(define (good-enough? guess x)
  (< (abs (- (cube guess) x)) 0.001))
(define (cbrt x)
  (cbrt-iter 1.0 x))

(cbrt 8)
