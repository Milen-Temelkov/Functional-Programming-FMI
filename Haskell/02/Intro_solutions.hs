import Prelude hiding (length, foldr, foldl, reverse, product, map, filter, zip, zipWith, (!!))

-- Test data
list1 :: [Int]
list1 = [1, 2, 3, 4, 5]

list2 :: [String]
list2 = ["a", "b", "c"]


-- Task 1

length :: [a] -> Int
length [] = 0
length (x : xs) = 1 + length xs


-- Task 2

exists :: (a -> Bool) -> [a] -> Bool
exists p [] = False
exists p (x : xs) = p x || exists p xs


-- Task 3

forall :: (a -> Bool) -> [a] -> Bool
forall p [] = True
forall p (x : xs) = p x && forall p xs


-- Task 4

member :: (Eq a) => a -> [a] -> Bool
member x xs = exists (\y -> x == y) xs


-- Task 5

map :: (a -> a) -> [a] -> [a]
map f [] = []
map f (x : xs) = (f x : map f xs)


-- Task 6

filter :: (a -> Bool) -> [a] -> [a]
filter p [] = []
filter p (x : xs)
    | p x = (x : filter p xs)
    | otherwise = filter p xs


-- Task 7

push :: a -> [a] ->[a]
push x [] = [x]
push x (y : ys) = (y : push x ys)


-- Task 8

reverse :: [a] -> [a]
reverse = foldl (flip (:)) []


-- Task 9

insert :: a -> Int -> [a] -> [a]
insert y 0 xs = (y : xs)
insert y n [] = [y]
insert y n (x : xs) = (x : insert y (n - 1) xs)


-- Task 10

foldr :: (a -> b -> b) -> b -> [a] -> b
foldr op init [] = init
foldr op init (x : xs) = op x (foldr op init xs)


-- Task 11

foldl :: (b -> a -> b) -> b -> [a] -> b
foldl op init [] = init
foldl op init (x : xs) = foldl op (op init x) xs


-- Task 12

product :: [Int] -> Int
product xs = foldl (*) 1 xs


-- Task 13 

zip :: [a] -> [b] -> [(a, b)]
zip l1 l2 = zipWith (,) l1 l2


-- Task 14

zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith f xs [] = []
zipWith f [] ys = []
zipWith f (x : xs) (y : ys) = ((f x y) : zipWith f xs ys)


-- Task 15

nats :: [Int]
nats = [1..] -- sad Zero noises


-- Task 16

interleave :: [a] -> [a] -> [a]
interleave xs [] = xs
interleave [] ys = ys
interleave (x : xs) (y : ys) = (x : y : interleave xs ys)


-- Task 17

pythagoreanTriples :: [(Int, Int, Int)]
pythagoreanTriples = [(b, a, c) | a <- [1..], b <- [1..a], c <- [1..(a+b)], (a*a + b*b) == c*c]

simplePythagoreanTriples :: [(Int, Int, Int)]
simplePythagoreanTriples = [(b, a, c) | a <- [1..], b <- [1..a], c <- [1..(a+b)], (a*a + b*b) == c*c, (gcd (gcd a b) c) == 1]


-- Task 18

primes :: [Int]
primes = sieve (from 2) 
    where 
        sieve (x : xs) = (x : sieve (filter (\y -> (y `rem` x) /= 0) xs)) 
        from n = (n : from (n + 1))


-- Bonus task lecture

(!!) :: [a] -> Int -> a
[] !! _ = error "ne moje"
l@(x : xs) !! n
    | n < 0 = reverse l !! ((-n) - 1)
    | n == 0 = x
    | otherwise = xs !! (n - 1)