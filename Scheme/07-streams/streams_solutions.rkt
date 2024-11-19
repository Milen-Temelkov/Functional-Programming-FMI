#lang racket

;; Utilities

(define-syntax delay
  (syntax-rules ()
    ((delay x) (lambda () x))))

(define force
  (lambda (x) (x)))


(define-syntax cons-stream
  (syntax-rules ()
    ((cons-stream h t)
     (cons h (delay t)))))


(define head car)

(define (tail s) (force (cdr s)))

;; prime predicate

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

           
(define (prime? n)
  (>= 2 (accumulate + 0 1 n (lambda (i) (if (= (remainder n i) 0) 1 0)) (lambda (i) (+ i 1)))))



;; Task 1

(define (nats-from n)
  (cons-stream
   n
   (nats-from (+ n 1))))

(define nats
  (nats-from 0))



;; Task 2

(define (take s n)
  (if (<= n 0)
      '()
      (cons (head s) (take (tail s) (- n 1)))))



;; Task 3

(define (map-stream f s)
  (cons-stream (f (head s)) (map-stream f (tail s))))



;; Task 4

(define (filter-stream p? s)
  (if (p? (head s))
      (cons-stream (head s) (filter-stream p? (tail s)))
      (filter-stream p? (tail s))))



;; Task 5

(define primes (filter-stream prime? nats))



;; Task 6

(define (map-stream1 f . streams)
  (cons-stream (apply f (map head streams))
               (apply map-stream1 f (map tail streams))))

(define ones (cons-stream 1 ones))

(define evens (cons-stream 0 (map-stream1 + ones odds)))

(define odds (cons-stream 1 (map-stream1 + ones (tail evens))))
              



;; Task 7

(define (iterate f x)
  (cons-stream x (iterate f (f x))))