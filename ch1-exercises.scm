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
   (+ a 1))
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
