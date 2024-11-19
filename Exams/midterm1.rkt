#lang racket

;; Helpers

(define (id x) x)

(define (1+ x) (+ x 1))

(define (compose f g) (lambda (x) (f (g x))))

(define (accumulate op nv a b term next)
  (if (> a b) nv
      (op (term a) (accumulate op nv (next a) b term next))))

(define (accumulate-i op nv a b term next)
  (if (> a b) nv
      (accumulate-i op (op nv (term a)) (next a) b term next)))

(define (collect a b next)
  (if (> a b) '()
      (cons a (collect (next a) b next))))

(define (from-to a b) (collect a b 1+))

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


(define // quotient)
(define % remainder)

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

(define (reverse l)
  (if (null? l)
      '()
      (append (reverse (cdr l)) (list (car l)))))

;; Solutions

;; Task 1

;; a

;; doesn't matter it is reversed
(define (numberToList n)
  (if (= 0 n)
      '()
      (cons (% n 10) (numberToList (// n 10)))))

(define (multiplyList l)
  (foldr * 1 (filter positive? l)))

(define (persistence n)
  (define (innerPers n count)
    (if (< n 10)
        count
        (innerPers (multiplyList (numberToList n)) (+ count 1))))
  (innerPers n 0))


;; b

(define (interval a b)
  (if (> a b)
      '()
      (cons a (interval (+ 1 a) b))))

(define (max-persistence-min-length a b)
  (let* ([persistancesList (map persistence (interval a b))]
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

(define (admits-left q n)
  (if (= n 0)
      (car q)
      (admits-left ((cdr q) (% n 10)) (// n 10))))
      
(define (admits-right q n)
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

;; bonus 1

;; На теория ако влезем в дълбочина до последния елемент за неговият подсписък той ще си е максимален
;; и минимален елемент. За всеки следващ стойностите на мин и макс ще се обновяват само ако новият елемент е по-голям/по-малък от макс/мин
;; на списъка преди него. Така линейно(рекурсивно) ще се направи нов списък съгласно условието.

;; Със обръщане линейно обхождане и обръщане отново става линейно :Д

;; ...


;(define (linearAmp l)
;  (define (helper l slightlySmallerL minEl maxEl)
;    (if (null? slightlySmallerL)
;       '(?)
;      (cons (helper (cdr l) maxEl minEl))))
;(helper l (cdr l) last last))




