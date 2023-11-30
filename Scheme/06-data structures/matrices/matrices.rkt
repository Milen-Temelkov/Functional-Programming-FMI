#lang racket

;; matrices

(define mat1 (list '(1 2 3) '(4 5 6) '(7 8 9)))
(define mat2 (list (list 1 2) (list 3 4)))
(define mat3 (list (list 3 4) (list 2 1)))
;(define  mat3 (list (list 3 4) (list 2 1)))

;; utilities

(define (1+ x) (+ 1 x))

(define(all? p? l)
  (foldr (lambda (x r) (and (p? x) r)) #t l))

(define(any? p? l)
  (foldr (lambda (x r) (or (p? x) r)) #f l))

(define (prime? n)
  (> 3 (accumulate +
                   0
                   1
                   n
                   (lambda (x) (if (= 0 (remainder n x)) 1 0))
                   1+)))

(define (at l i)
  (cond [(null? l) 'err]
        [(= i 0) (car l)]
        [else (at (cdr l) (- i 1))]))

(define (del l i)
  (cond [(null? l) 'err]
        [(= i 0) (cdr l)]
        [else (cons (car l) (del (cdr l) (- i 1)))]))

(define (accumulate op nv a b term step)
  (if (> a b)
      nv
      (accumulate op (op nv (term a)) (step a) b term step)))



;; Taks 1

(define (matrix-ref m i j)
  (at (at m i) j))



;; Task 2

(define (mat? m)
  (and (list? m)
       (not (null? m))
       (all? list? m)
       (let ([len (length (car m))])
         (and (not (= len 0))
              (all? (lambda (row) (= (length row) len)) (cdr m))))))



;; Task 3

(define (delete-column i m)
  (map (lambda (l) (del l i)) m))



;; Task 4

(define (transpose m)
  (if (null? (car m))
      '()
      (cons (map car m)
            (transpose (map cdr m)))))



;; Task 5

(define (map-matrix f m)
  (map (lambda (el) (map f el)) m))



;; Task 6

(define (main-diagonal m)
  (if (or (null? m) (null? (car m)))
      '()
      (cons (caar m) (main-diagonal (map cdr (cdr m))))))



;; Task 7

(define (dimensions m)
  (cons (length m) (length (car m))))



;; Task 8

(define (reverse-columns m)
  (map reverse m))



;; Task 9

(define (nth-column m n)
  (define (helper m n i)
    (if (= n i)
        (map car m)
        (helper (delete-column 0 m) n (1+ i))))
  (helper m n 1))



;; Task 10

(define (all-list? p? l)
  (foldr (lambda (x rec) (and (p? x) rec)) #t l))

(define (all-columns? p? m)
  (let ([mt (transpose m)])
    (all-list? (lambda (x) (eq? #t x)) (map (lambda (el) (all-list? p? el)) mt))))



;; Task 11

(define (has-prime? l)
  (foldr (lambda (x r) (or (prime? x) r)) #f l))
           
(define (prime-in-each-column? m)
  (foldr (lambda (x r) (and (has-prime? x) r)) #t (transpose m)))



;; Task 12

(define (scalar-product l1 l2)
  (foldl + 0 (map * l1 l2)))


;; умножава матрица по вектор ред и прави нов списък

(define (mat-row m r)
  (map (lambda (el) (scalar-product el r)) m))


;; multiply от илиянци

(define (multiply-not-gucci m1 m2)
  (let ([m3 (transpose m2)])
    (if (null? m1)
        '()
        (let ([curr-row (car m1)])
          (cons
           (mat-row m3 curr-row)
           (multiply (cdr m1) m2))))))


;; gucci multiply

(define (multiply m1 m2)
  (if (not (= (cdr (dimensions m1)) (car (dimensions m2))))
      'learn-algebra
      (let ([m3 (transpose m2)])
        (map (lambda (el) (mat-row m3 el)) m1))))



;; Task 13

(define (member? x l)
  (if (null? l)
      #f
      (if (equal? x (car l))
          #t
          (member? x (cdr l)))))

(define (subset? l1 l2)
  (all? (lambda (x) (member? x l2)) l1))

(define (subset-of-matrix? l m)
  (any? (lambda (x) (subset? l x)) m))

(define (find-columns m)
  (let* ([mt (transpose m)])
    (length (filter (lambda (col) (subset-of-matrix? col m)) mt))))