#lang racket

;; Example graph
(define g (list '(1 2 3)
                '(2 3)
                '(3 4 5)
                '(4)
                '(5 2 4 6)
                '(6 2)))


;; Utilities

(define all? andmap)
(define any? ormap)

(define (max-el l)
  (define (max-helper l mah)
    (if (null? l)
        mah
        (if (< mah (car l))
            (max-helper (cdr l) (car l))
            (max-helper (cdr l) mah))))
  (max-helper l (car l)))


;; Task 1

(define (graph? g)
  (and (list? g)
       (all? list? g)
       (all? (lambda (pair) (list? (cdr pair))) g)))



;; Task 2

(define (out-deg v g)
  (length (cdr (assoc v g))))



;; Task 3

(define (in-deg v g)
  (count (lambda (pair) (member v (cdr pair))) g))



;; Task 4

(define (deg v g)
  (+ (out-deg v g) (in-deg v g)))



;; Task 5
  
(define (max-deg g)
  (max-el (map (lambda (v) (deg (car v) g)) g)))



;; Task 6

(define (edge? v u g)
  (member u (cdr (assoc v g))))



;; Task 7

(define (path v u g)
  (#t))
