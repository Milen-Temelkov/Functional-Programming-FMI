#lang racket

;; Utilities

(define (accumulate operator bottom left-bound right-bound calc-curr step)
  (if (> left-bound right-bound)
      bottom
      (operator (calc-curr left-bound)
          (accumulate operator bottom (step left-bound) right-bound calc-curr step))))



;; Higher order functions

;; Task 1

(define o
  (lambda (f g)
    (lambda (x)
      (f (g x)))))

(define 1+-then-sqr
  (let ([square (lambda (x) (* x x))]
        [1+ (lambda (x) (+ 1 x))])
    (o square 1+)))



;; Task 2

(define repeated
  (lambda (n f x)
    (if (= n 0)
        x
        (repeated (- n 1) f (f x)))))



;; Task 3
(define (1+ x) (+ 1 x))
(define (id x) x)

(define (repeat n f)
  (lambda (x) (repeated n f x)))

(define (repeat-ac n f)
  (accumulate o id 1 n (lambda (_) f) 1+))

(define (repeat-rec n f)
  (if (= 0 n)
      id
      (o f (repeat-rec  (- n 1) f))))


;; Accumulate

;; Task 1

;; Task 2

;; Task 3

;; Task 4

