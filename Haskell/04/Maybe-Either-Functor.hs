-- utilities

safeDiv :: Int -> Int -> Maybe' Int
safeDiv _ 0 = Nothing'
safeDiv x y = Just' $ x `quot` y

data Maybe' a = Nothing' | Just' a deriving (Show, Eq)

data Either' e a = Left' e | Right' a

-- Task 1

safeDiv2 :: Int -> Maybe' Int
safeDiv2 n 
    | n `rem` 2 == 1 = Nothing'
    | otherwise = Just' $ n `quot` 2

safeDiv4 :: Int -> Maybe' Int
safeDiv4 n = 
    case safeDiv2 n of
        Nothing' -> Nothing'
        Just' m -> safeDiv2 m 

-- Task 2

listToMaybe :: [a] -> Maybe' a
listToMaybe [] = Nothing'
listToMaybe (x:_) = Just' x

maybeToList :: Maybe' a -> [a]
maybeToList Nothing' = []
maybeToList (Just' n) = [n] 

-- Task 3

getFirstAndLast :: (a -> Bool) -> [a] -> Maybe' (a, a)
getFirstAndLast p [] = Nothing'
getFirstAndLast p l = Just'(fst, lst)
    where   fst = head filtered 
            lst = last filtered
            filtered = filter p l


-- Task 4

untilJust :: Num b => (a -> Maybe' b) -> (a -> a) -> a -> b
untilJust fM f n = 
    case fM n of
        Nothing' -> untilJust fM f (f n)
        Just' m -> m


-- Task 5

filterMap :: (a -> Maybe' b) -> [a] -> [b]
filterMap _ [] = []
filterMap fM (x:xs) =
    case fM x of
        Nothing' -> filterMap fM xs
        Just' n -> n:filterMap fM xs


-- Task 6

hopHelper :: [Maybe' a] -> [a]
hopHelper [Just' x] = [x]
hopHelper ((Just' x):xs) = x : hopHelper xs


hop :: Eq (Maybe' a) => [Maybe' a] -> Maybe' [a]
hop [] = Nothing'
hop l
    | (Nothing') `elem` l = Nothing'
    | otherwise = Just' (hopHelper l)



{-
unjust :: Maybe' a -> a
unjust x = 
    case

azis :: [Maybe' a] -> Maybe' [a]
azis [] = []
azis ((Just' x) : xs) = (Just' (x : azis xs))
-}

azis :: [Maybe' a] -> Maybe' [a]
azis [] = Just' []  
azis (m:ms) = case (m, azis ms) of
    (Just' x, Just' xs) -> Just' (x : xs)  
    _                   -> Nothing' 