#lang racket

;; Exercise 2.54
;;

(define nil '())
(define (equal? list1 list2)
  (cond ((and (null? list1) (null? list2))
         true)
        ((= (car list1) (car list2))
         (equal? (cdr list1) (cdr list2)))
        (else false)))
