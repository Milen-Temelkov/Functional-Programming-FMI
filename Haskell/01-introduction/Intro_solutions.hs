-- Task 1

fact :: Int -> Int
fact 0 = 1
fact n = n * fact (n - 1)


-- Task 2

fib :: Int -> Int
fib 1 = 0
fib 2 = 1
fib n = fib (n - 2) + fib (n - 1)

fibI :: Int -> Int -> Int -> Int
fibI prev curr 1 = curr
fibI prev curr n = fibI curr (prev + curr) (n - 1)

-- Task 3

myAbs :: Int -> Int
myAbs n
    | n < 0 = (-n)
    | otherwise = n


-- Task 4

add1 :: Int -> Int
add1 n = (n + 1)

mult2 :: Int -> Int
mult2 n = (n * 2)


composeInt :: (Int -> Int) -> (Int -> Int) -> (Int -> Int)
composeInt f g n = f (g n)


-- Task 5 

compose :: (a -> a) -> (a -> a) -> (a -> a)
compose f g n = f (g n)


-- Task 6

myConcat :: [a] -> [a] -> [a]
myConcat [] l2 = l2
myConcat l1 l2 = head l1 : myConcat (tail l1) l2


-- Task 7

isIntPrefix :: [Int] -> [Int] -> Bool
isIntPrefix = isPrefix


-- Task 8

isPrefix :: (Eq a) => [a] -> [a] -> Bool
isPrefix [] l = True
isPrefix (x : xt) (y : yt)
    | x == y = isPrefix xt yt
    | otherwise = False


-- Task 9

frepeat :: Int -> (a -> a) -> a -> a
frepeat 0 f x = x
frepeat n f x = frepeat (n - 1) f (f x)


-- Task 10

frepeated :: Int -> (a -> a) -> (a -> a)
frepeated 0 f = id
frepeated n f = (\ x -> (compose f (frepeated (n - 1) f) ) x)