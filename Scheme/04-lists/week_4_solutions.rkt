#lang racket

;; Task 1

(define (len l)
  (if (null? l)
      0
      (+ 1 (len (cdr l)))))



;; Task 2

(define (any? p l)
  (if(null? l)
     #f
     (if (p (car l))
         #t
         (any? p (cdr l)))))



;; Task 3

(define (member? x l)
  (if (null? l)
      #f
      (if (equal? x (car l))
          l
          (member? x (cdr l)))))



;; Task 4

(define (at n l)
  (define (at-i n l curr)
    (cond ((null? l) #f)
          ((= n curr)(car l))
          (else (at-i n (cdr l) (+ curr 1)))))
  (at-i n l 0))



;; Task 5

(define (map-1 f l)
  (if (null? l)
      '()
      (cons (f (car l)) (map f (cdr l)))))



;; Task 6

(define (filter p l)
  (define (filter-i p l new-l)
    (if (null? l)
        new-l
        (filter-i p (cdr l) (if (p (car l))
                                (append new-l (list (car l)))
                                new-l))))
  (filter-i p l '()))



;; Task 7

(define (push x l)
  (append l (list x)))



;; Task 8

(define (reverse l)
  (if (null? l)
      '()
      (append (reverse (cdr l)) (list (car l)))))



;; Task 9

(define (insert x n l)
  (define (insert-i x n l curr new-l)
    (if (= n curr)
        (append new-l (list x) l)
        (insert-i x n (cdr l) (+ curr 1) (append new-l (list (car l))))))
  (if (> n (len l))
      (append l (list x))
      (insert-i x n l 0 '())))



;; Task 10

(define (range a b)
  (define (range-i a b new-l)
    (if (> a b)
        new-l
        (range-i (+ a 1) b (append new-l (list a)))))
  (range-i a b '()))



;; Task 11

(define (sum l)
  (define (sum-i l sum-container)
    (if (null? l)
        sum-container
        (sum-i (cdr l) (+ sum-container (car l)))))
  (sum-i l 0))



;; Task 12

(define (reduce op init l)
  (if (null? l)
      init
      (op (car l) (reduce op init (cdr l))))) 

;; Task 12 -> 5

(define (map-12 f l)
  (reduce (lambda (x y) (cons (f x) y)) '() l)) 

;; Task 12 -> 6

(define (filter-12 p l)
  (reduce (lambda (x y) (if (p x) (cons x y) y)) '() l))

;; Task 12 -> 11

(define (sum-12 l)
  (reduce + 0 l))