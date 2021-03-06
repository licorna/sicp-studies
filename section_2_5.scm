;; -*- geiser-scheme-implementation: guile -*-

;; Section 2.5
;;
;; Rodrigo Valin <licorna@gmail.com>
;; Jan. 2016
;;

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (error "No method for these types: APPLY-GENERIC"
                 (list op type-tags))))))

(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))


;; Scheme number Package
(define (install-scheme-number-package)
  (define (tag x)
    (attach-tag 'scheme-number x))
  (put 'add '(scheme-number scheme-number)
       (lambda (x y) (tag (+ x y))))
  (put 'sub '(scheme-number scheme-number)
       (lambda (x y) (tag (- x y))))
  (put 'mul '(scheme-number scheme-number)
       (lambda (x y) (tag (* x y))))
  (put 'div '(scheme-number scheme-number)
       (lambda (x y) (tag (/ x y))))
  (put 'make 'scheme-number
       (lambda (x) (tag x)))
  'done)
(define (make-scheme-number n)
  ((get 'make 'scheme-number) n))


;; Rational package
(define (install-rational-package)
  (define (numer x) (car x))
  (define (denom x) (cdr x))
  (define (make-rat n d)
    (let ((g (gcd n d)))
      (cons (/ n g) (/ d g))))
  (define (add-rat x y)
    (make-rat (+ (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (sub-rat x y)
    (make-rat (- (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (mul-rat x y)
    (make-rat (* (numer x) (numer y))
              (* (denom x) (denom y))))
  (define (div-rat x y)
    (make-rat (* (numer x) (denom y))
              (* (denom x) (numer y))))
  (define (equ? x y)
    (and (= (type-tag x) 'rational)
         (= (type-tag y) 'rational)
         (= (numer x) (numer y))
         (= (denom x) (denom y))))
  (define (tag x) (attach-tag 'rational x))
  (define (rational-list? lst)
    "Will return true if the whole lst consists of rationals."
    (andmap (lambda (x) (rational? x) lst)))
  (put 'add '(rational rational)
       (lambda (x y) (tag add-rat x y)))
  (put 'sub '(rational rational)
       (lambda (x y) (tag sub-rat x y)))
  (put 'mul '(rational rational)
       (lambda (x y) (tag mul-rat x y)))
  (put 'div '(rational rational)
       (lambda (x y) (tag div-rat x y)))
  (put 'make 'rational
       (lambda (n d) (tag (make-rat n d))))
  (put 'equ? 'rational
       (lambda (x y) (equ? x y)))
  (put 'rational-list 'rational
       (lambda (lst) (rational-list? lst)))
  'done)

(define (make-rational n d)
  ((get 'make 'rational) n d))


;; Complex package
(define (install-complex-package)
  (define (make-from-real-imag x y)
    ((get 'make-from-real-imag
          'rectangular) x y))
  (define (make-from-mag-ang r a)
    ((get 'make-from-mag-ang
          'polar) r a))
  (define (add-complex z1 z2)
    (make-from-real-imag
     (+ (real-part z1) (real-part z2))
     (+ (imag-part z1) (imag-part z2))))
  (define (sub-complex z1 z2)
    (make-from-real-imag
     (- (real-part z1) (real-part z2))))
  (define (mul-complex z1 z2)
    (make-from-mag-ang
     (* (magnitude z1) (magnitude z2))
     (+ (angle z1) (angle z2))))
  (define (div-complex z1 z2)
    (make-from-mag-ang
     (/ (magnitude z1) (magnitude z2))
     (- (angle z1) (angle z2))))
  (define (equ? z1 z2)
    (and (= (type-tag z1) 'complex)
         (= (type-tag z2) 'complex)
         (= (real-part z1) (real-part z2))
         (= (imag-part z1) (imag-part z2))))
  (define (complex-list lst)
    (andmap (lambda (x) (complex? x) lst)))
  (define (tag z) (attach-tag 'complex z))
  (put 'add '(complex complex)
       (lambda (z1 z2)
         (tag (add-complex z1 z2))))
  (put 'sub '(complex complex)
       (lambda (z1 z2)
         (tag (sub-complex z1 z2))))
  (put 'mul '(complex complex)
       (lambda (z1 z2)
         (tag (mul-complex z1 z2))))
  (put 'div '(complex complex)
       (lambda (z1 z2)
         (tag (div-complex z1 z2))))
  (put 'make-from-real-imag 'complex
       (lambda (x y)
         (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'complex
       (lambda (r a)
         (tag (make-from-mag-ang r a))))
  (put 'equ? 'complex
       (lambda (z1 z2)
         (equ? z1 z2)))
  (put 'complex-list 'complex
       (lambda (lst) (complex-list lst)))
  (put 'real-part '(complex) real-part)
  (put 'imag-part '(complex) imag-part)
  (put 'magniitude '(complex) magnitude)
  (put 'angle '(complex) angle)
  'done)

;;
;; Exercise 2.78
;;

(define (type-tag datum)
  (cond [(number? datum) ('number)]
        [(pair? datum) (car datum)]
        [else (error "Bad tagged datum: TYPE-TAG" datum)]))

(define (contents datum)
  (cond [(number? datum) (datum)]
        [(pair? datum) (cdr datum)]
        [else (error "Bad tagged datum: CONTENTS" datum)]))

(define (attach-tag tag symbol)
  (cond [(number? symbol) (symbol)]
        [(symbol? symbol) (cons tag symbol)]
        [else (error "Should be number or symbol: ATTACH-TAG" tag symbol)]))

;;
;; Exercise 2.79
;; Exercise 2.80
;;

(define (install-generic-math-package)
  (define (equ? n1 n2)
    (cond [(and (number? n1) (number? n2)) (=number? n1 n2)]
          [(and (complex? n1) (complex? n2) ((get 'equ? 'complex) n1 n2))]
          [(and (rational? n1) (rational? n2) ((get 'equ? 'rational) n1 n2))]
          [else (error "Invalid type: EQU?" n1 n2)]))
  (define (zero? n1)
    (cond [(and (number? n1) (= n1 0))]
          [(and (complex? n1) (= (real-part n1) (imag-part n1) 0))]
          [(and (rational? n1) (= (numer n1) 0))]))
  'done)
