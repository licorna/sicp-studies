;; -*- geiser-scheme-implementation: guile -*-

;; Section 2.3.4
;; Huffman Encoding Trees
;;

(define (make-leaf symbol weight)
  (list 'leaf symbol weight))
(define (leaf? object)
  (eq? (car object) 'leaf))
(define (symbol-leaf x) (cadr x))
(define (weight-leaf x) (caddr x))

(define (make-code-tree left right)
  (list left
        right
        (append (symbols left)
                (symbols right))
        (+ (weight left) (weight right))))

(define (left-branch tree) (car tree))
(define (right-branch tree) (cadr tree))

(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))

(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))

(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits)
        '()
        (let ((next-branch
               (choose-branch
                (car bits)
                current-branch)))
          (if (leaf? next-branch)
              (cons
               (symbol-leaf next-branch)
               (decode-1 (cdr bits) tree))
              (decode-1 (cdr bits) next-branch)))))
  (decode-1 bits tree))

(define (choose-branch bit branch)
  (cond ((= bit 0) (left-branch branch))
        ((= bit 1) (right-branch branch))
        (else (error "bad bit: CHOOSE-BRANCH" bit))))

(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set)))
         (cons x set))
        (else
         (cons (car set)
               (adjoin-set x (cdr set))))))

(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
        (adjoin-set
         (make-leaf (car pair)
                    (cadr pair))
         (make-leaf-set (cdr pairs))))))

(define sample-tree
  (make-code-tree
   (make-leaf 'A 4)
   (make-code-tree
    (make-leaf 'B 2)
    (make-code-tree
     (make-leaf 'D 1)
     (make-leaf 'C 1)))))

(define sample-message
  '(0 1 1 0 0 1 0 1 0 1 1 1 0))

;;
;; Implementation
;;
;; For each character in the original message
;; Find the path to it in the encoding tree
;;
;; Exercise 2.68
;;

(define (choose-path symbol tree)
  (cond ((in-list? symbol (symbols (left-branch tree)))
         (left-branch tree))
        ((in-list? symbol (symbols (right-branch tree)))
         (right-branch tree))
        (else error "Symbol not in tree")))

(define (in-list? symbol list-1)
  (cond ((null? list-1) #f)
        ((eq? symbol (car list-1)) #t)
        (else (in-list? symbol (cdr list-1)))))

(define (encode-symbol symbol tree)
  (define (encode-symbol-1 symbol tree path)
    (cond ((null? tree) '())
          ((leaf? tree) path)
          ((in-list? symbol (symbols tree))
           (if (in-list? symbol (left-branch tree))
               (encode-symbol-1 symbol
                                (left-branch tree)
                                (append path '(0)))
               (encode-symbol-1 symbol
                                (right-branch tree)
                                (append path '(1)))))
          (else error "Error: symbol not in tree")))
  (encode-symbol-1 symbol tree '()))


(define (encode message tree)
  (if (null? message) '()
      (append
       (encode-symbol (car message) tree)
       (encode (cdr message) tree))))
               
