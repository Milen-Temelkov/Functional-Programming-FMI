#lang racket

;; Task 1 (a.k.a. class Tree)
  
(define empty-tree '())


(define (make-tree root left right)
  (list root left right))

(define mk make-tree)


(define (make-leaf root)
  (mk root empty-tree empty-tree))

(define mkl make-leaf)


(define t-root car)
(define t-left cadr)
(define t-right caddr)

(define (t-empty? t)
  (equal? t empty-tree))

(define (leaf? t)
  (and (not (t-empty? t))
       (t-empty? (t-left t))
       (t-empty? (t-right t))))

(define (tree? t)
  (or (t-empty? t)
      (and (= 3 (length t))
           (tree? (cadr t))
           (tree? (caddr t)))))



;; Task 2 (a.k.a. traversals)

(define (collect-pre-order t)
  (if (t-empty? t)
      '()
      (append (list (t-root t))
              (collect-pre-order (t-left t))
              (collect-pre-order (t-right t)))))

(define (collect-in-order t)
  (if (t-empty? t)
      '()
      (append (collect-in-order (t-left t))
              (list (t-root t))
              (collect-in-order (t-right t)))))

(define (collect-post-order t)
  (if (t-empty? t)
      '()
      (append (collect-post-order (t-left t))
              (collect-post-order (t-right t))
              (list (t-root t)))))

 

;; Task 3

(define (map-tree f t)
  (if (t-empty? t)
      '()
      (mk (f (t-root t)) (map-tree f (t-left t)) (map-tree f (t-right t)))))



;; Task 4

(define (height t)
  (define (helper curr-h t)
    (if (empty? t)
        curr-h
        (max (helper (+ 1 curr-h) (t-left t)) (helper (+ 1 curr-h) (t-right t)))))
  (helper 0 t))



;; Task 5

(define (level n t)
  (define (helper curr-lvl n t)
    (cond [(t-empty? t) '()]
          [(= curr-lvl n) (cons (t-root t) '())]
          [else (append (helper (+ curr-lvl 1) n (t-left t))
                        (helper (+ curr-lvl 1) n (t-right t)))]))
  (if (> (+ 1 n) (height t))
      '()
      (helper 0 n t)))



;; Task 6

(define (count-leaves t)
  (cond [(leaf? t) 1]
        [(t-empty? t) 0]
        [else (+ (count-leaves (t-left t)) (count-leaves (t-right t)))]))



;; Task 7

(define (remove-leaves t)
  (if (or (leaf? t) (t-empty? t))
      empty-tree
      (mk (t-root t) (remove-leaves (t-left t)) (remove-leaves (t-right t)))))



;; Task 8

(define (invert t)
  (if (t-empty? t)
      '()
      (mk (t-root t) (invert (t-right t)) (invert (t-left t)))))



;; Task 9

(define (bst? t)
  (if (or (t-empty? t) (leaf? t))
      #t
      (and (if (not (t-empty? (t-left t)))
               (> (t-root t) (t-root (t-left t)))
               #t)
           (if (not (t-empty? (t-right t)))
               (< (t-root t) (t-root (t-right t)))
               #t)
           (bst? (t-left t))
           (bst? (t-right t)))))



;; Task 10

(define (insert-bst x t)
  (if (empty? t)
      (mkl x)
      (cond [(< x (t-root t)) (mk (t-root t)
                                  (insert-bst x (t-left t))
                                  (t-right t))]
            [else (mk (t-root t)
                      (t-left t)
                      (insert-bst x (t-right t)))])))
      
(define (list->bst l)
  (define (helper l t)
    (if (null? l)
        t
        (helper (cdr l) (insert-bst (car l) t))))
  (helper l '()))

(define (sort-it l)
  (collect-in-order (list->bst l)))



;; Task 11

(define (balanced? t)
  (if (t-empty? t)
      #t
      (and (> 2 (abs (- (height (t-left t))
                        (height (t-right t)))))
           (balanced? (t-left t))
           (balanced? (t-right t)))))


;; Example trees

(define ext (mk 1
                (mkl 2)
                (mk 3
                    (mkl 4)
                    (mk 5
                        empty-tree
                        (mkl 6)))))

(define bst (mk 5
                (mk 3
                    (mkl 1)
                    (mkl 6))
                (mk 8
                    (mk 7
                        (mkl 6)
                        empty-tree)
                    (mkl 9))))



