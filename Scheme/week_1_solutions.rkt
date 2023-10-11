#lang racket

; task 0

(define (eq1)
  (* (+ 10 5.16 19 9.712361)
     (- 20
        (- 16 4))))

(define (eq2)
  (+ 1/4 2/5 3/8 (* 6
                    (- 5.1 1.6)
                    (- 9/3 7/4))))

(define (eq3)
  (+ (expt 3
           (/ 60 7))
     (/ (expt 2 10)
        179)))

(define (eq4)
  (expt 1-i 21))



;task 1

(define (add a b)
  (+ a b))



;task 2

(define (is-even? n)
  (= 0 (remainder n 2)))



;task 3

(define (signum n)
  (if (> 0 n)
      -1
      (if (< 0 n)
          1
          0)))



;task 4

(define (root? x)
  (= 0 (+ (* 3
             (expt x 2))
          (* 2 x)
          -1)))



;task 5

(define (triangle? a b c)
  (and (< a (+ b c))
       (< b (+ a c))
       (< c (+ a b))))



;task 6

(define (fib n)
  (if (= n 1)
      0
      (if (= n 2)
          1
          (+ (fib (- n 1))
             (fib (- n 2))))))



;task 7

(define (sum-interval a b)
  (if (> a b)
      0
      (+ a
         (sum-interval (+ a 1) b))))



; task 8 

(define (power base exponent)
  (cond
    ((< exponent 0) (/ 1 (power base (- exponent))))
    ((= exponent 0) 1)
    (else (* base (power base (- exponent 1))))))



;task 9

(define (count-digits n)
  (if (= n 0)
      n
      (+ 1
         (count-digits (quotient n 10)))))



;task 10

(define (reverse-digits n)
  ( if(= n 0)
      0
      (+ (* (remainder n 10)
            (expt 10
                  (- (count-digits n)
                     1)))
         (reverse-digits (quotient n 10)))))