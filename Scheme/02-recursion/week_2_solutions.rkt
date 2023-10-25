#lang racket

;; Utilities

(define // quotient)

(define % remainder)

(define (reverse-num n)
  (define (rev n rev-n)
    (if (= 0 n)
        rev-n
        (rev (// n 10) (+ (% n 10) (* rev-n 10)))))
  (rev n 0)
 )


;; Task 1

(define (sum-digits-iter n)
  (define (iter n sum)
    (if (= 0 n)
        sum
        (iter (// n 10) (+ sum (% n 10)))))
  (iter n 0))



;; Task 2

(define (count-devisors n)
  (define (iter n dev i)
    (if (= i n)
        (+ dev 1)
        (if (= (% n i) 0)
            (iter n (+ dev 1) (+ i 1))
            (iter n dev (+ i 1)))))
  (iter n 0 1))



;; Task 3

(define (prime? n)
  (if (= 2 (count-devisors n))
      #t
      #f))



;; Task 4

(define (increasing-digits n)
  (define (check n last)
    (if (= n 0)
        #t
        (if (< (% n 10) last)
            (check (// n 10) (% n 10))
            #f)))
    (check (// n 10) (% n 10)))



;; Task 5

(define (ends-with? n k)
  (define (comp-digits n k)
    (if (= k 0)
        #t
        (if (not (= (% n 10) (% k 10)))
            #f
            (comp-digits (// n 10) (// k 10)))))
  (comp-digits n k))
                   
(define (automorphic? n)
  (if (ends-with? (* n n) n)
      #t
      #f))



;; Task 6

(define (perfect? n)
  (define (sum-all-divisors n sum i)
    (if (= i n)
        (if (= sum n)
            #t
            #f)
        (if (= 0 (% n i))
            (sum-all-divisors n (+ sum i) (+ i 1))
            (sum-all-divisors n sum (+ i 1)))))
  (sum-all-divisors n 0 1))


;; Task 7

(define (binary-to-decimal bin-n)
  (define (create-dec-num bin-n dec-n i)
    (if (= bin-n 0)
        dec-n
        (create-dec-num (// bin-n 10) (+ dec-n (* (% bin-n 10) (expt 2 i))) (+ i 1))))
  (create-dec-num bin-n 0 0))



;; Task 8
 
(define (decimal-to-binary dec-n)
  (define (create-bin-num dec-n bin-n i)
    (if (= dec-n 0)
        (// (reverse-num bin-n) 10)
        (create-bin-num (// dec-n 2) (+ (* 10 bin-n) (% dec-n 2)) (+ i 1))))
  (create-bin-num dec-n 1 0))

  