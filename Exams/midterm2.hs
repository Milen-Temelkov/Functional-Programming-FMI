-- Task 1

isPrefix :: [Int] -> [Int] -> Bool
isPrefix [] l = True
isPrefix p [] = False
isPrefix (p : ps)  (l : ls) = p == l && isPrefix ps ls

hasPrefixFrom :: [Int] -> [[Int]] -> Bool
hasPrefixFrom l [] = False
hasPrefixFrom l (p : ps) = isPrefix p l || hasPrefixFrom l ps

listsWithPrefix :: [[Int]] -> [[Int]] -> Int
listsWithPrefix [] p = 0
listsWithPrefix (l : ls) p 
    | (hasPrefixFrom l p) = 1 + listsWithPrefix ls p
    | otherwise = 0 + listsWithPrefix ls p

l1 :: [[Int]]
l1 = [[1,2,3], [2,3,4], [3,4,5]]

l2 :: [[Int]]
l2 = [[3], [2,3,4], [1,2]]

l3 :: [[Int]]
l3 = [[1], [5,6]]

l4 :: [[[Int]]]
l4 = [l2, l3]


minListsWithPrefixHelper :: [[Int]] -> [[[Int]]] -> [([[Int]], Int)]
minListsWithPrefixHelper l []= []
minListsWithPrefixHelper l (pl : pls) = (pl, listsWithPrefix l pl) : minListsWithPrefixHelper l pls

minListEl :: ([[Int]], Int) -> [([[Int]], Int)] -> [[Int]]
minListEl (resList, smallest) [] = resList
minListEl (resList, smallest) ( (list, amount) : ps) = minListEl (if (amount < smallest) then (list, amount) else (resList, smallest)) ps


minListsWithPrefix :: [[Int]] -> [[[Int]]] -> [[Int]]
minListsWithPrefix l pl = minListEl (head tuplesList) (tail tuplesList)
    where 
        tuplesList = minListsWithPrefixHelper l pl


-- Task 2

-- test data

poll :: [[String]]
poll = [["C", "A", "D"], ["B", "D", "C"], ["A", "B", "C"], ["D", "C"]]

pollToTuple :: [String] -> (String, [String])
pollToTuple (p : ps) = (p, ps)


countHisFriends :: [(String, [String])] -> (String, [String]) -> [String]
countHisFriends [] whose = (snd whose)
countHisFriends (fr : frs) whose
    | elem (fst whose) (snd fr) = (fst fr) : countHisFriends frs whose
    | otherwise = countHisFriends frs whose


countUniq :: [String] -> Int
countUniq [] = 0
countUniq (s : ss) 
    | elem s ss = 0 + countUniq ss
    | otherwise = 1 + countUniq ss


countFriendsHelper :: [(String, [String])] -> [(String, [String])] -> [(String, Int)]
countFriendsHelper all [] = []
countFriendsHelper all (x : xs) = ((fst x), ( countUniq (countHisFriends all x))) : countFriendsHelper all xs

countFriends :: [[String]] -> [(String, Int)]
countFriends poll = countFriendsHelper all all
    where 
        all = map pollToTuple poll

-- Task 3

nats :: [Int]
nats = [0..]

absoluteVal :: Int -> Int
absoluteVal n
    | n < 0 = (-n)
    | otherwise = n

squared :: Int -> Int
squared n = n * n

-- nz dali baca 

pointPairs :: [((Int, Int), (Int, Int))]
pointPairs = [((x1, y1), (x2, y2)) | x1 <- [0..], y1 <- [0..], x2 <- [x1..y1], y2 <- [0..(squared (x1 + y1 - x2))], ((squared (x1 - x2)) + (squared (y1 - y2))) < 9 && ((squared (x2 - x1)) + (squared (y2 - y1))) > 4, (x1 < x2) || (x1 == x2 && y1 < y2)]