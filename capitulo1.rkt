#lang racket
(define (square-x a b)
  (+ (* a a) (* b b)))

(define (square-of-greatest a b c)
  (cond ((and (< a b) (< a c)) (square-x b c))
        ((and (< b a) (< b c)) (square-x a c))
        (else (square-x a b))))

(define (p) (p))
(define (text x y)
  (if (= x 0)
      0
      y))


(define (square x)
  (* x x))
(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x) x)))
(define (improve guess x)
  (average guess (/ x guess)))
(define (average x y)
  (/ (+ x y) 2))
(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))
(define (sqrt x)
  (sqrt-iter 1.0 x))


(define (fib n)
  (fib-iter 1 0 n))

(define (fib-iter a b count)
  (if (= count 0)
      b
      (fib-iter (+ a b) a (- count 1))))

(define (ex111 n)
  (if (< n 3)
      n
      (+ (ex111 (- n 1)) (* (ex111 (- n 2)) 2) (* (ex111 (- n 3)) 3))))
