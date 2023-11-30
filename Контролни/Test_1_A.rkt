#lang racket

;; Helpers

(define (id x) x)
(define (1+ x) (+ x 1))
(define // quotient)
(define % remainder)

(define (filter p? l)
  (cond ((null? l) l)
        ((p? (car l)) (cons (car l) (filter p? (cdr l))))
        (else (filter p? (cdr l)))))


(define (foldr op nv l)
  (if (null? l) nv
      (op (car l) (foldr op nv (cdr l)))))


(define (foldl op nv l)
  (if (null? l) nv
      (foldl op (op nv (car l)) (cdr l))))


(define (foldr1 op l)
  (if (null? (cdr l)) (car l)
      (op (car l) (foldr1 op (cdr l)))))


(define (foldl1 op l)
  (foldl op (car l) (cdr l)))


(define (maxListEl l)
  (define (findMax l el)
    (if (null? l)
        el
        (if (> (car l) el)
            (findMax (cdr l) (car l))
            (findMax (cdr l) el))))
  (findMax l (car l)))


(define (minListEl l)
  (define (findMin l el)
    (if (null? l)
        el
        (if (< (car l) el)
            (findMin (cdr l) (car l))
            (findMin (cdr l) el))))
  (findMin l (car l)))

(define (numberToList n)
  (if (= 0 n)
      '()
      (cons (% n 10) (numberToList (// n 10)))))


;; Solutions

;; Task 1

;; a

(define (multiplyList l)
  (foldl * 1 (filter positive? l)))

(define (persistence n)
  (define (innerPers n count)
    (if (< n 10)
        count
        (innerPers (multiplyList (numberToList n)) (+ count 1))))
  (innerPers n 0))


;; b

(define (max-persistence-min-length a b)
  (let* ([persistancesList (map persistence (range a b))]
         [maxPersistance (maxListEl persistancesList)]
         [isMaxPersistance? (lambda (x) (= x maxPersistance))])
    
    (define (returnSearchedElement l index)
      (if (or (null? l) (isMaxPersistance? (car l)))
          (+ index a)
          (returnSearchedElement (cdr l) (+ 1 index))))
    
    (returnSearchedElement persistancesList 0)))



;; Task 2

(define q0 (cons #f (lambda (d) (if (odd? d) q0 q1))))
(define q1 (cons #t (lambda (d) (if (even? d) q0 q1))))

(define (admits q n)
  (let ([number (reverse (numberToList n))])
    (define (admits q l)
      (if (null? l)
          (car q)
          (admits ((cdr q) (car l)) (cdr l))))
    (admits q number)))



;; Task 3

(define (findAmp l)
  (if (null? l)
      0
      (- (maxListEl l) (minListEl l))))

(define (amplitudes l)
  (if (null? l)
      '()
      (cons (findAmp l) (amplitudes (cdr l)))))