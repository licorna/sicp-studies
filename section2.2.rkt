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
