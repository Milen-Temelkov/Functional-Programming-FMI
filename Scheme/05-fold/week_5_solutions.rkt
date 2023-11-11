#lang racket

;; fold functions

(define (foldr op init l)
  (if (null? l)
      init
      (op (car l)
          (foldr op
                 init
                 (cdr l)))))


(define (foldl op init l)
  (if (null? l)
      init
      (foldl op
             (op init
                 (car l))
             (cdr l))))


(define (foldl-asoc op init l)
  (if (null? l)
      init
      (foldl-asoc op
                  (op (car l) init)
                  (cdr l))))


;; utilities

(define (id x) x)
(define (1+ x) (+ 1 x))
(define (snoc x r) (append r (list x)))



;; Task 1

(define (sum-r l)
  (foldr + 0 l))


(define (sum-l l)
  (foldl + 0 l))



;; Task 2

(define (map-r f l)
  (foldr (lambda (x r) (cons (f x) r)) '() l))


(define (map-l f l)
  (reverse (foldl (lambda (nv x) (cons (f x) nv)) '() l)))



;; Task 3

(define (filter-r p l)
  (foldr (lambda (x r) (if (p x) (cons x r) r)) '() l))


(define (filter-l p l)
  (reverse (foldl (lambda (nv x) (if (p x) (cons x nv) nv)) '() l)))



;; Task 4

(define (foldr1 op l)
  (if (null? (cdr l))
      (car l)
      (op (car l)
          (foldr1 op (cdr l)))))


(define (foldl1 op l)
  (foldl op (car l) (cdr l)))



;; Task 5

(define (len-r l)
  (foldr (lambda (x r) (+ 1 r)) 0 l))


(define (len-l l)
  (foldl (lambda (nv x) (+ 1 nv)) 0 l))



;; Task 6

;; any

(define (any?-r p l)
  (foldr (lambda (x r) (or (p x) r)) #f l))


(define (any?-l p l)
  (foldl (lambda (nv x) (or (p nv) (p x))) #f l))


;; all

(define (all?-r p l)
  (foldr (lambda (x r) (and (p x) r)) #t l))


(define (all?-l p l)
  (foldl (lambda (nv x) (and nv (p x))) #t l))



;; Task 7

(define (reverse-r l)
  (foldr snoc '() l))


(define (reverse-l l)
  (foldl (lambda (nv x) (cons x nv)) '() l))


(define (reverse-la l)
  (foldl-asoc cons '() l))



;; Task 8

;;take

(define (take n l)
  (define (helper n l new)
    (if (or (= n 0) (null? l))
        (reverse new)
        (helper (- n 1) (cdr l) (cons (car l) new))))
  (helper n l '()))


(define (take-r n l)
  (foldr (lambda (x r) (if (> n (len-r r)) (snoc x r) r)) '() (reverse l)))


(define (take-l n l)
  (reverse (foldl (lambda (nv x) (if (< (len-r nv) n) (cons x nv) nv)) '() l)))


(define (take-la n l)
  (reverse (foldl-asoc (lambda (x nv) (if (< (len-r nv) n) (cons x nv) nv)) '() l)))


;; drop

(define (drop n l)
  (if (= n 0)
      l
      (drop (- n 1) (cdr l))))


(define (drop-r n l)
  (let ([lenL (len-r l)])
    (foldr (lambda (x r) (if (>= n (- lenL (len-r r))) r (cons x r))) '() l)))


(define (drop-l n l)
  (let ([lenL (len-r l)])
    (foldl (lambda (nv x) (if (>= n (- lenL (len-r nv))) nv (cons x nv))) '() (reverse l))))


(define (drop-la n l)
  (let ([lenL (len-r l)])
    (foldl-asoc (lambda (x nv) (if (>= n (- lenL (len-r nv))) nv (cons x nv))) '() (reverse l))))



;; Task 9

;; takeWhile

(define (takeWhile p? l)
  (foldr (lambda (x r) (if (p? x) (cons x r) '())) '() l))


;; dropWhile

(define (dropWhile p? l)
  (reverse (foldl (lambda (nv x) (if (and (null? nv) (p? x)) nv (cons x nv))) '() l)))



;; task 10

(define (zipWith f l1 l2)
  (define (helper l1 l2 newl)
     (if (or (null? l1) (null? l2))
          (reverse newl)
          (helper (cdr l1) (cdr l2) (cons (f (car l1) (car l2)) newl))))
   (helper l1 l2 '()))


(define (zip l1 l2)
  (zipWith cons l1 l2))