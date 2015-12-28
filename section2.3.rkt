;; -*- geiser-scheme-implementation: guile -*-

#lang racket

;; Exercise 2.54
;;

(define nil '())
;; removed this def as it collides with further examples.
;; (define (equal? list1 list2)
;;   (cond ((and (null? list1) (null? list2))
;;          #t)
;;         ((= (car list1) (car list2))
;;          (equal? (cdr list1) (cdr list2)))
;;         (else #f)))


;; 2.3.2 Example
;;
(define (variable? x) (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1)
       (variable? v2)
       (eq? v1 v2)))

(define (=number? exp num)
  (and (number? exp) (= exp num)))

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) 
         (+ a1 a2))
        (else (list '+ a1 a2))))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) 
             (=number? m2 0)) 
         0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) 
         (* m1 m2))
        (else (list '* m1 m2))))

(define (make-exponentiation b e)
  (list '** b e))

(define (exponentiation? x)
  (and (pair? x) (eq? (car x) '**)))

(define (base e) (cadr e))
(define (exponent e) (caddr e))

(define (sum? x)
  (and (pair? x) (eq? (car x) '+)))

(define (addend s) (cadr s))
(define (augend s) (caddr s))

(define (product? x)
  (and (pair? x) (eq? (car x) '*)))

(define (multiplier p) (cadr p))
(define (multiplicand p) (caddr p))

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
          (make-product 
           (multiplier exp)
           (deriv (multiplicand exp) var))
          (make-product 
           (deriv (multiplier exp) var)
           (multiplicand exp))))
        ((exponentiation? exp)
         (make-product
          (make-product
           (exponent exp)
           (make-exponentiation
            (base exp)
            (make-sum
             (exponent exp) -1)))
          (deriv (base exp) var)))

        (else (error "unknown expression 
                      type: DERIV" exp))))

;; section 2.3.3
;;

(define (element-of-set? x set)
  (cond ((null? set) #f)
        ((equal? x (car set)) #t)
        (else (element-of-set? x (cdr set)))))

(define (adjoin-set x set)
  (if (element-of-set? x set)
      set
      (cons x set)))

(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
        ((element-of-set? (car set1) set2)
         (cons (car set1) intersection-set (cdr set1) set2))
        (else intersection-set (cdr set1) set2)))

;; exercise 2.59
;;

(define (union-set set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        ((not (element-of-set? (car set1) set2))
         (cons (car set1) (union-set (cdr set1) set2)))
        (else (union-set (cdr set1) set2))))

;;

(define (oelement-of-set? x set)
  (cond ((= x (car set)) #t)
        ((< x (car set)) #f)
        (else (oelement-of-set? x (cdr set)))))

(define (ointersection-set set1 set2)
  (if (or (null? set1) (null? set2))
      '()
      (let ((x1 (car set1)) (x2 (car set2)))
        (cond ((= x1 x2)
               (cons x1 (ointersection-set (cdr set1)
                                           (cdr set2))))
              ((< x1 x2) (ointersection-set (cdr set1) set2))
              ((< x2 x2) (ointersection-set set1 (cdr set2)))))))


;; Sets as binary trees
;;

(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right)
  (list entry left right))

(define (element-of-tree? x set)
  (cond ((null? set) #f)
        ((= x (entry set)) #t)
        ((< x (entry set))
         (element-of-tree? x (left-branch set)))
        ((> x (entry set))
         (element-of-tree? x (right-branch set)))))

(define (adjoin-tree x tree)
  (cond ((null? tree) (make-tree x '() '()))
        ((= x (entry tree)) tree)
        ((< x (entry tree))
         (make-tree
          (entry tree)
          (adjoin-tree x (left-branch tree))
          (right-branch tree)))
        ((> x (entry tree))
         (make-tree
          (entry tree)
          (left-branch tree)
          (adjoin-tree x (right-branch tree))))))


(define (list->tree elements)
  (car (partial-tree
        elements (length elements))))

(define (partial-tree elts n)
  (if (= n 0)
      (cons '() elts)
      (let ((left-size
             (quotient (- n 1) 2)))
        (let ((left-result
               (partial-tree
                elts left-size)))
          (let ((left-tree
                 (car left-result))
                (non-left-elts
                 (cdr left-result))
                (right-size
                 (- n (+ left-size 1))))
            (let ((this-entry
                   (car non-left-elts))
                  (right-result
                   (partial-tree
                    (cdr non-left-elts)
                    right-size)))
              (let ((right-tree
                     (car right-result))
                    (remaining-elts
                     (cdr right-result)))
                (cons (make-tree this-entry
                                 left-tree
                                 right-tree)
                      remaining-elts))))))))


;; Sets and information retrival
;;

(define (lookup given-key set-of-records)
  (cond ((null? set-of-records) #f)
        ((equal? given-key
                 (key (car set-of-records)))
         (car set-of-records))
        (else
         (lookup given-key (cdr set-of-records)))))


;; exercise 2.66
;;

(define (make-entry key value)
  (list key value))
(define (key entry)
  (car entry))
(define (value entry)
  (cadr entry))


(define tree-as-list
  (list
   (make-entry 1 "One")
   (make-entry 2 "Two")
   (make-entry 3 "Three")
   (make-entry 4 "Four")
   (make-entry 5 "Five")
   (make-entry 6 "Six")))

(define tree (list->tree tree-as-list))
