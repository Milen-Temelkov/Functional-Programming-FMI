#lang racket

;; в индустрията я има идеята за т.нар. "partial application":
;; да дадем част от аргументите на функция и да получим нова такава,
;; която очаква остатъка от аргументите и пуска оригиналната функция с всички аргументи накуп

;; ((partial + 1 2) 3 4) == (+ 1 2 3 4)

(define (partial f . l1)
  (lambda l2 (apply f (append l1 l2))))


;; (define sum (partial foldr + 0))
;; вместо
;; (define sum (lambda (l) (foldr + 0 l))


;; имплементирайте vararg версия на map и zipWith (a.k.a. да се направи map като вградения)
;; (помислете какво биха правили ако им дадем множество списъци, върху които да работят)
;; на практика zipWith e като map 


;; прави списък от първите елементи на всеки списък (l е списък от списъци)

(define (getHeads l)
  (if (null? l)
      '()
      (cons (caar l) (getHeads (cdr l)))))


;; прави списък от последните елементи на всеки списък (l е списък от списъци)

(define (getTails l)
  (if (null? l)
      '()
      (cons (cdar l) (getTails (cdr l)))))

;; реално тези две функции са (map car/cdr l) но ще се ползват за имплементацията на мап
;; не може да ползваме map за да имплементираме map (if you know what i mean)

(define (my-map f . l)
  (define (map-helper f l)
    (if (null? (car l))
        '()
        (cons (apply f (getHeads l)) (map-helper f (getTails l)))))
  (map-helper f l))

(define zipWith my-map)

;; шано мап

(define transpose
  (partial apply map list))

(define (map-gucci f . l)
  (map (partial apply f) (transpose l)))

