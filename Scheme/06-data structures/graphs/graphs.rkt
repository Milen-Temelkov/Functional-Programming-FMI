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


;; {{{ my-delay

(define-syntax my-delay
  (syntax-rules ()
    ((_ nqkyv-kod)
     (lambda ()
       nqkyv-kod))))

;; }}}

;; {{{ my-force

(define my-force
  (lambda (promise)
    (promise)))

;; }}}

;; {{{ my-stream-cons

(define-syntax my-stream-cons
  (syntax-rules ()
    ((_ fst rst)
     (cons fst (my-delay rst)))))

;; }}}

;; {{{ (my-stream-first s)

(define (my-stream-first s)
  (car s))

;; }}}

;; {{{ (my-stream-rest s)

(define (my-stream-rest s)
  (my-force (cdr s)))

;; }}}

;; {{{ (nats-helper n)

(define (nats-from n)
  (my-stream-cons
   n
   (nats-from (+ n 1))))

;; }}}

;; {{{ nats

(define nats
  (nats-from 0))

;; Task 1

(define (stream-take s n)
  (if (<= n 0)
      '()
      (cons (my-stream-first s) (stream-take (my-stream-rest s) (- n 1)))))


(define (stream-map f s)
  (my-stream-cons (f (my-stream-first s)) (stream-map f (my-stream-rest s))))


(define (stream p? s)
  (my-stream-cons ( (my-stream-first s)) (stream-map f (my-stream-rest s))))