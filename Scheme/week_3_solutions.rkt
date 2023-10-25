#lang racket

;; Utilities

(define (1+ x) (+ 1 x))
(define (id x) x)



;; Higher order functions

;; Task 1

(define o
  (lambda (f g)
    (lambda (x)
      (f (g x)))))



;; Task 2

(define (repeated n f x)
  (if (= n 0)
      x
      (repeated (- n 1) f (f x))))



;; Task 3

(define (repeat n f)
  (lambda (x) (repeated n f x)))

(define (repeat-rec n f)
  (if (= 0 n)
      id
      (o f (repeat-rec  (- n 1) f))))



;; Accumulate

;; Accumulate functions

(define (accumulate operator bottom left-bound right-bound calc-curr step)
  (if (> left-bound right-bound)
      bottom
      (operator (calc-curr left-bound)
                (accumulate operator
                            bottom
                            (step left-bound)
                            right-bound
                            calc-curr
                            step))))

(define (accumulate-i operator bottom left-bound right-bound calc-curr step)
  (if (> left-bound right-bound)
      bottom
      (accumulate-i operator
                    (operator bottom (calc-curr left-bound))
                    (step left-bound)
                    right-bound
                    calc-curr
                    step)))



;; Task 1

(define (5< x) (< x 5))
(define (5<= x) (<= x 5))

(define (count p a b)
  (let ([predicate (lambda (p x) (if (p x) 1 0))])
                                     
    (if (= (predicate p a) 0)
        0
        (+ (predicate p a) (count p (+ a 1) b)))))

(define (count-acc p a b)
  (accumulate-i + 0 a b (lambda (a) (if (p a) 1 0)) 1+))



;; Task 2

(define (any? p a b)
  (let ([predicate (lambda (x) (p x))])
                                     
    (cond ((predicate a) #t)
          ((> a b) #f)
          ((= a b) (predicate a))
          (else (any? p (+ a 1) b)))))

(define (any-acc? p a b)
  (< 0 (accumulate-i + 0 a b (lambda (a) (if (p a) 1 0)) 1+)))



;; Task 3

(define (all? p a b)
  (let ([predicate (lambda (x) (p x))])
                                     
    (cond ((not (predicate a)) #f)
          ((= a b) (predicate a))
          (else (all? p (+ a 1) b)))))

(define (all-acc? p a b)
  (= (- b (- a 1)) (accumulate-i + 0 a b (lambda (a) (if (p a) 1 0)) 1+)))



;; Task 4

(define (repeat-acc n f)
  (accumulate o id 1 n (lambda (_) f) 1+))

(define (repeated-acc n f x)
  ((repeat-acc n f) x))