# Условия

1. Дефинирайте функция `(len l)`, която намира дължината на списъкa `l`:

```scheme
(len '(1 2 3))     ;; => 3
(len (list))       ;; => 0
(len (cons 1 '())) ;; => 1
```

2. Дефинирайте функция `(any? p l)`, която проверява дали съществува елемент в `l`, за който
е изпълнен предикатът `p`:

```scheme
(any? odd? '(1 2 3 4 5)) ;; => #t
(any? odd? '(2 4 6))     ;; => #f
```

  - Дефинирайте аналогичния предикат `(all? p l)` чрез `any?`

3. Дефинирайте функция `(member? x l)`, която проверява дали елементът `x` се съдържа в списъка `l`:

```scheme
(member? "test" '(1 "test" 3 4)) ;; => '("test" 3 4)
(member? 5 '(1 "test"  3 4))     ;; => #f
```

4. Дефинирайте функция `(at n l)`, която връща елемента, намиращ се на позиция `n` (броим от 0)
в списъка `l`, или `#f`, ако позицията е извън списъка:

```scheme
(at 0 '(1 2 3)) ;; => 1
(at 3 '(1 2 3)) ;; => #f
```

5. Дефинирайте функция `(map f l)`, която прилага `f` върху всеки елемент на списъка `l`:

```scheme
(map (lambda (x) (+ x 1)) '(1 2 3)) ;; => '(2 3 4)
```

6. Дефинирайте функция `(filter p l)`, която съставя нов списък, съдържащ само елементите на `l`, за които е изпълнен предикатът `p`:

```scheme
(filter odd? '(1 2 3 4 5)) ;; => '(1 3 5)
```

7. Дефинирайте функция `(push x l)`, която добавя елемента `x` на края на списъка `l`:

```scheme
(push 5 '(1 2 3 4)) ;; => '(1 2 3 4 5)
(push #f '()) ;; => '(#f)
```

8. Дефинирайте функция `(reverse l)`, която връща списък с елементите на `l` в обратен ред:

```scheme
(reverse '(1 2 3)) ;; => '(3 2 1)
```

9. Дефинирайте функция `(insert x n l)`, която вкарва елемента `x` на позиция `n` в списъка `l` (ако `n` е след края на `l`, вкарваме `x` накрая):

```scheme
(insert 3 2 '(1 2 4 5)) ;; => '(1 2 3 4 5)
(insert 'ne6to 10000 '(ni6to)) ;; => '(ni6to ne6to)
```

10. Дефинирайте функция `(range a b)`, която генерира целочисления интервал `[a, b]`:

```scheme
(range 1 10) ;; => '(1 2 3 4 5 6 7 8 9 10)
(range 0 0) ;; => '(0)
(range 3 1) ;; => '()
```

11. Дефинирайте функция `(sum l)`, която намира сумата на елементите в списък

```scheme
(sum (range 0 100)) ;; => 5050
```

12. (**БОНУС**) Дефинирайте функция `(reduce op init l)`, която пресмята `(op l[0] (op l[1] (op l[2] ... (op l[n] init)...)))` (ако имаме подаден празен списък, резултатът е `init`).

  - Дефинирайте чрез `reduce` следните функции:
    - `map` от 5. задача;
    - `filter` от 6. задача;
    - `sum` от 11. задача.