#lang racket
;; Section 2.2
;;
;; Nov. 17th. 2015.
;;

;; global defines
(define nil '())
(define squares (list 1 4 9 16 25))
(define odds (list 1 3 5 7))

(define (list-ref items n)
  (if (= n 0)
      (car items)
      (list-ref (cdr items)
                (- n 1))))

(define (length items)
  (if (null? items)
      0
      (+ 1 (length (cdr items)))))

(define (length2 items)
  (define (length-iter a count)
    (if (null? a)
        count
        (length-iter (cdr a)
                     (+ 1 count))))
  (length-iter items 0))

(define (append list1 list2)
  (if (null? list1)
      list2
      (cons (car list1)
            (append (cdr list1)
                    list2))))


;; exercise 2.17
;;

(define (last-pair list1)
  (if (null? (cdr list1))
      (car list1)
      (last-pair (cdr list1))))


;; exercise 2.18
;;

(define (reverse list1)
  (if (null? list1)
      nil
      (append (reverse (cdr list1))
              (list (car list1)))))

;; exercise 2.19
;;
;; (staring with coin change from section 1.2.2)

(define (count-change amount)
  (cc-01 amount 5))

(define (cc-01 amount kinds-of-coins)
  (cond ((= amount 0) 1)
        ((or (< amount 0)
             (= kinds-of-coins 0))
         0)
        (else
         (+ (cc-01 amount (- kinds-of-coins 1))
            (cc-01 (- amount (first-denomination-01
                           kinds-of-coins))
                kinds-of-coins)))))

(define (first-denomination-01 kinds-of-coins)
  (cond ((= kinds-of-coins 1) 1)
        ((= kinds-of-coins 2) 5)
        ((= kinds-of-coins 3) 10)
        ((= kinds-of-coins 4) 25)
        ((= kinds-of-coins 5) 50)))

;; now exercise 2.19 for real

(define us-coins
  (list 50 25 10 5 1))
(define uk-coins
  (list 100 50 20 10 5 2 1 0.5))

(define (cc amount coin-values)
  (cond ((= amount 0)
         1)
        ((or (< amount 0)
             (no-more? coin-values))
         0)
        (else
         (+ (cc
             amount
             (except-first-denomination
              coin-values))
            (cc
             (- amount
                (first-denomination
                 coin-values))
             coin-values)))))

(define (no-more? list1)
  (null? list1))

(define (first-denomination list1)
  (car list1))

(define (except-first-denomination list1)
  (cdr list1))


;; exercise 2.20
;;

(define (odd-filter z)
  (define (odd-filter-accum z accum)
    (if (null? z)
        accum
        (odd-filter-accum (cdr z)
                          (if (odd? (car z))
                              (append accum (list (car z)))
                              accum))))
  (odd-filter-accum z '()))

(define (even-filter z)
  (define (even-filter-accum z accum)
    (if (null? z)
        accum
        (even-filter-accum (cdr z)
                           (if (even? (car z))
                               (append accum (list (car z)))
                               accum))))
  (even-filter-accum z '()))

(define (same-parity . z)
  (if (even? (car z))
      (even-filter z)
      (odd-filter z)))

;; exercise 2.21
;;

(define (square x)
  (expt x 2))

(define (square-list-01 items)
  (if (null? items)
      nil
      (cons (square (car items))
            (square-list-01 (cdr items)))))

(define (square-list-02 items)
  (map (lambda (x) (square x)) items))


