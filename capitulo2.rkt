#lang racket

;; necessary to use `nil` as in SICP.
(define nil '())

(define (sum-integers a b)
  (if (> a b)
      0
      (+ a (sum-integers (+ a 1) b))))

(define (cube x)
  (* x x x))

(define (sum-cubes a b)
  (if (> a b)
      0
      (+ (cube a)
         (sum-cubes (+ a 1) b))))

(define (pi-sum a b)
  (if (> a b)
      0
      (+ (/ 1.0 (* a (+ a 2)))
         (pi-sum (+ a 4) b))))

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

(define (inc n) (+ n 1))

(define (sum-cubes-high a b)
  (sum cube a inc b))

(define (identity n) (+ n 1))

(define (sum-integers-high a b)
  (sum identity a inc b))

(define (pi-sum-high a b)
  (define (pi-term x) (/ 1.0 (* x (+ x 2))))
  (define (pi-next x) (+ x 4))
  (sum pi-term a pi-next b))

(define (integral f a b dx)
  (define (add-dx x) (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b) dx))


(define (simpsons-rule f a b n)
  (define (h) (/ (- b a) n))
  (define (y k) (f (+ a (* (h) k))))
  f)

(define (square x)
  (* x x))

(define (f x y)
  (let ((a (+ 1 (* x y)))
        (b (- 1 y)))
    (+ (* x (square a))
       (* y b)
       (* a b))))

;;; exercise 1.34
(define (f134 g) (g 2))

;;; section 1.3.3
(define (close-enough? a b)
  (< (abs (- a b)) 0.001))

(define (average a b)
  (/ (+ a b) 2))

(define (half-interval-search f a b)
  (let ((midpoint (average a b)))
    (if (close-enough? a b)
        midpoint
        (let ((test-value (f midpoint)))
          (cond
            ((positive? test-value)
             (half-interval-search f a midpoint))
            ((negative? test-value)
             (half-interval-search f midpoint b))
            (else midpoint))))))

(define (half-interval-method f a b)
  (let ((a-value (f a))
        (b-value (f b)))
    (cond ((and (negative? a-value)
                (positive? b-value))
           (half-interval-search f a b))
          ((and (positive? a-value)
                (negative? b-value))
           (half-interval-search f b a))
          (else
           (error "Values are not of opposite sign" a b)))))

(define tolerance 0.00001)
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(define (sqrt3 x)
  (fixed-point
   (lambda (y) (average y (/ x y)))
   1.0))

(define (average-damp f)
  (lambda (x) (average x (f x))))

(define (sqrt4 x)
  (fixed-point
   (average-damp
    (lambda (y) (/ x y)))
   1.0))

(define balance 100)
(define (whitdraw amount)
  (if (>= balance amount)
      (begin (set! balance (- balance amount))
             balance)
      "Insufficient funds!"))

(define new-withdraw
  (let ((balance 100))
    (lambda (amount)
      (if (>= balance amount)
          (begin (set! balance (- balance amount))
                 balance)
          "Insufficient funds!"))))

(define (make-withdraw balance)
  (lambda (amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds")))

(define (make-account balance)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds!"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (dispatch m)
    (cond ((eq? m 'withdraw) withdraw)
          ((eq? m 'deposit) deposit)
          (else (error "Unknows request: MAKE-ACCOUNT" m))))
  dispatch)

(define (make-accumulator acc)
  (define (accumulate amount)
    (begin (set! acc (+ acc amount))
           acc))
  accumulate)



(define (make-interval a b) (cons a b))
(define (add-interval x y)
  (make-interval (+ (lower-bound x) 
                    (lower-bound y))
                 (+ (upper-bound x) 
                    (upper-bound y))))
(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) 
               (lower-bound y)))
        (p2 (* (lower-bound x) 
               (upper-bound y)))
        (p3 (* (upper-bound x) 
               (lower-bound y)))
        (p4 (* (upper-bound x) 
               (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))
(define (div-interval x y)
  (mul-interval x 
                (make-interval 
                 (/ 1.0 (upper-bound y)) 
                 (/ 1.0 (lower-bound y)))))

(define (upper-bound x) (cdr x))
(define (lower-bound x) (car x))

(define (my-length list1)
  (define (my-length-iter list1 count)
    (if (null? list1)
        count
        (my-length-iter (cdr list1) (+ 1 count))))
  (my-length-iter list1 0))

(define (my-append list1 list2)
  (if (null? list1)
      list2
      (cons (car list1)
            (my-append (cdr list1) list2))))

(define (last-pair list1)
  (cond ((= (my-length list1) 1) (car list1))
        ((> (my-length list1) 1) (last-pair (cdr list1)))
        (else null)))

(define (list-ref list1 ref)
  (cond ((< ref 0) null)
        ((> ref (my-length list1)) null)
        ((= ref 0) (car list1))
        (else (list-ref (cdr list1) (- ref 1))))) 

(define (my-reverse list1)
  (define (my-reverse-iter list1 list2)
    (if (null? list1)
        list2
        (my-reverse-iter (cdr list1) (cons (car list1) list2))))
  (my-reverse-iter list1 '()))


;; exercise 2.20
;; this exercise is super ugly, I have to find a way to turn
;; the (if (is-odd n)) from the `same-parity` procedure into something
;; more natural, and not so unbalanced.
(define (is-even n)
  (= (modulo n 2) 0))
(define (is-odd n)
  (not (is-even n)))

(define (filter list1 func)
  (define (filter-iter list1 list2)
    (if (null? list1)
        list2
        (if (func (car list1))
            (filter-iter (cdr list1) (cons (car list1) list2))
            (filter-iter (cdr list1) list2))))
  (my-reverse (filter-iter list1 '())))

(define (same-parity n . z)
  (if (is-odd n)
      (filter (cons n z) is-odd)
      (filter (cons n z) is-even)))
;; end ex 2.20


;; exercise 2.21
(define (square-list items)
  (if (null? items)
      nil
      (cons (* (car items) (car items))
            (square-list (cdr items)))))
(define (square-list-2 items)
  (map (lambda (x) (* x x)) items))
;; end ex 2.21

;; exercise 2.22
;; As I realized a few minutes ago, doing `(cons list number)` will
;; create a stupid data structure like this:
;; (cons (cons (cons 1 nil) 2) 3)
;; '(((1) . 2) . 3)
;;
;; Instead of: (cons 1 (cons 2 (cons 3 nil)))
;; '(1 2 3)
;;
;; end
;;

;; exercise 2.23
;; unable to get a good answer to this, as I don't know what to do
;; with the return value of the function, for some reason 
(define (for-each proc list1)
  (if (null? list1)
      2
      ((proc (car list1))
       (for-each proc (cdr list1)))))
;; end 2.23


(define (count-leaves x)
  (cond ((null? x) 0)
        ((not (pair? x)) 1)
        (else (+ (count-leaves (car x))
                 (count-leaves (cdr x))))))


;; exercise 2.27
;(define (my-reverse list1)
;  (define (my-reverse-iter list1 list2)
;    (if (null? list1)
;        list2
;        (my-reverse-iter (cdr list1) (cons (car list1) list2))))
;  (my-reverse-iter list1 '()))
(define (my-deep-reverse list1)
  (define empty-list '())
  (define (my-deep-reverse-iter list1 accum)
    (if (null? list1)
        accum
        (my-deep-reverse-iter (cdr list1)
                              (cons (car list1) accum))))
  (my-deep-reverse-iter list1 empty-list))
; (define x (list (list 1 2) (list 3 4)))
; (my-deep-reverse x)
;; TODO: need to fix this implementation.
;;

;; exercise 2.28
;;
(define (left tree)
  (car tree))
(define (right tree)
  (cdr tree))
(define (fringe tree)
  (cond ((null? tree) tree)
        ((not (pair? tree)) tree)
        (append (fringe (left tree)) (fringe (right tree)))))
          
;; exercise 2.28
;;

;; exercise 2.24
;;
