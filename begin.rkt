#lang sicp
(require racket/trace)

; compound procedure
(define (square x) (* x x))

(define (sum-of-squares x y)
  (+ (square x) (square y)))

(define (f a)
  (sum-of-squares (+ a 1) (* a 2)))

; (f 5)
; (sum-of-squares (6) (6))
;   (+ (square (6)) (square (6))
; -> applicative order (interpreter)
; oder
; (sum-of-squares (+ 5 1) (+ 5 1))
;   (+ (square (+ 5 1) (+ 5 1)))
; -> normal order

; case analysis
(define (abs x)
  (cond ((> x 0) x)
        ((= x 0) 0)
        ((< x 0) (- x))))

; alternative 1
(define (abs x)
  (cond ((< x 0) (- x))
        (else x)))

; alternative 2
(define (abs x)
  (if (< x 0) ; special case of cond
      (- x)
      x))

; exercise 1.2 DONE
(/ (+ 5 4 (- 2 (- 3 (+ 6 (/ 4 5))))) (* 3 (- 6 2) (- 2 7)))

; exercise 1.3 DONE
(define (opob a b c)
  (+
   (square
    (if (> a b) a b)
   )
   (square
    (if (> b c) b c)
   )
  )
)

; exercise 1.5 DONE
; test to find out evaluation strategy of interpreter
(define (p) (p))
(define (test x y)
  (if (= x 0) 0 y))
(test 0 (p))
; hypothesis: if the interpreter uses applicative-order evaluation, the program determines, because it tries to resolve (p), which cannot be resolved. If the function resolves to 0, it means the evaluation was resolved before the interpreter needed to evaluate (p) -> normal order evaluation.
; result: no determination, presumably because (p) cannot be resolved -> applicative-order evaluation
; solution: (p) is in fact defined to itself, so the interpreter gets stuck in a loop when trying to resolve

; exercise 1.6 DONE
; hypothesis: is else even a predicate?
; result: yes it is. hypothesis wrong.
; solution: conditional expressions are special forms with special evaluation rules.
; when defining a procedure with applicative order evaluation, both operands are evaluated
; in the special form for conditional expressions, the else is only evaluated, if the first expression is not true

; exercise 1.7 DONE
; square roots newtons method
(define (sqrt-iter guess x)
  (if (good-enough?-alternative2 guess x)
      guess
      (sqrt-iter (improve guess x) x)))

(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))

; does not work for big numbers, because the difference between the squared guess and x will not get below 0.001 -> algorithm gets stuck
; does not work for small numbers, because 0.001 might be too big of a comparison value for a very small number -> algorithm exits too early
(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))

; the solution to this problem is to instead compare the guess with the next guess to see if there is enough distance to qualify as an improvement.
(define (good-enough?-alternative1 guess x)
  (< (/ (abs (- (improve guess x) guess)) guess) 0.001))
; given that arithmetic operations are performed with limited precision, and the guess will not change after sufficient iterations, this is the better answer. We don't really need the threshold if the values to be compared are equal after reaching the boundary of precision.
(define (good-enough?-alternative2 guess x)
  (= guess (improve guess x)))

; exercise 1.8 DONE
(define (cube-iter guess x)
  (if (good-enough?-cube guess x)
      guess
      (cube-iter (improve2 guess x) x)))

(define (improve2 guess x)
  (/ (+ (/ x (* guess guess)) (* 2 guess)) 3))

(define (good-enough?-cube guess x)
  (= guess (improve2 guess x)))
