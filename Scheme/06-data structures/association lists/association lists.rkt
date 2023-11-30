#lang racket

;; Asociative lists

(define lis '(8 7 1 7 8 2 2 8 2 7 8 1))

(define ex-al '((8 . 1) (7 . 2) (2 . 4) (7 . 6) (3 . 2) (2 . 1)))

(define ex-al2 '((8 . 4) (7 . 3) (1 . 2) (2 . 3)))

(define comp1 '((1 . 2) (2 . 3) (3 . 4)))
(define comp2 '((2 . 20) (4 . 40) (6 . 60)))

;; Utilities

(define (put key val l)
  (if (null? l)
      (cons key val)
      (let* ([head (car l)]
             [k1 (caar l)]
             [tail (cdr l)])
        (if (equal? k1 key)
            (cons (cons k1 val) (cdr l))
            (cons head (put key val tail))))))

(define (zipWith f l1 l2)
  (define (helper l1 l2 newl)
    (if (or (null? l1) (null? l2))
        (reverse newl)
        (helper (cdr l1) (cdr l2) (cons (f (car l1) (car l2)) newl))))
  (helper l1 l2 '()))


;; Task 1

(define (index l)
  (define (index-helper l ind)
    (if (null? l)
        '()
        (cons (cons (car l) ind) (index-helper (cdr l) (+ 1 ind)))))
  (index-helper l 0))

(define (index-cool l)
  (map cons l (range 0 (length l))))



;; Task 2

(define (update-or-insert default f key al)
  (if (null? al)
      (list (cons key default))
      (if (= key (caar al))
          (cons (cons key (f (cdar al))) (cdr al))
          (cons (car al) (update-or-insert default f key (cdr al))))))



;; Task 3

(define (uniques l)
  (define (helper l new)
    (if (null? l)
        new
        (if (member (car l) new)
            (helper (cdr l) new)
            (helper (cdr l) (cons (car l) new)))))
  (reverse (helper l '())))


(define (amount el l)
  (foldr (lambda (x rec) (if (equal? el x)
                             (+ 1 rec)
                             rec))
         0
         l))


(define (histogram l)
  (let* ([uniq (uniques l)]
         [amnt (map (lambda (x) (amount x l)) uniq)])
    (map cons uniq amnt)))



;; Task 4

(define (clear-dups al)
  (define (helper al uniq)
    (if (null? al)
        '()
        (let* ([head-key (caar al)]
               [head (car al)]
               [tail (cdr al)])
          (if (member head-key uniq)
              (helper tail uniq)
              (cons head (helper tail (cons head-key uniq)))))))
  (helper al '()))



;; Task 5

(define (combine f al1 al2)
  (define (helper al1 al2 new)
    (if (or (null? al1) (null? al2))
        (reverse new)
        (let* ([head-key1 (caar al1)]
               [head-key2 (caar al2)]
               [head-val1 (cdar al1)]
               [head-val2 (cdar al2)]
               [head1 (car al1)]
               [head2 (car al2)]
               [tail1 (cdr al1)]
               [tail2 (cdr al2)])
          (if (equal? head-key1 head-key2)
              (helper tail1 tail2 (cons (cons head-key1 (f head-val1 head-val2)) new))
              (helper tail1 tail2 new)))))
  (helper al1 al2 '()))


;; Task 6

(define (compose al1 al2)
  (let* ([vals (map cdr al1)]
         [keys (map car al2)]
         [new1 (filter (lambda (x) (if (member (cdr x) keys) #t #f)) al1)]
         [new2 (filter (lambda (x) (if (member (car x) vals) #t #f)) al2)])
    (map (lambda (x) (cons (car x) (cdr (assoc (cdr x) new2)))) new1)))



;; Task 7

(define (group-by f l)
  (let* ([f-vals (uniques (map f l))]
         [eq-classes (map (lambda (x)
                            (filter (lambda (y) (= x (f y))) l))
                          f-vals)])
    (zipWith cons f-vals eq-classes)))
    


;; Task 8

(define != (lambda (x y) (not (= x y))))

(define (run-length-encode l)
  (define (helper l prev count)
    (cond [(null? l) (list (cons prev count))]
          [(!= prev (car l)) (cons (cons prev count) (helper (cdr l) (car l) 1))]
          [#t (helper (cdr l) (car l) (+ 1 count))]))
  (helper (cdr l) (car l) 1))



;; Task 9

(define (run-length-decode al)
  (apply append (map (lambda (pair)
                       (map (lambda (el)
                              (car pair)) (range 0 (cdr pair))))
                     al)))
