-- Task 3

fst3 (x, y, z) = x
snd3 (x, y, z) = y
trd3 (x, y, z) = z

itemsList :: [(String, String, Int)]
itemsList = [("A1", "A", 1920), ("A2", "A", 1930), ("B1", "B", 1925), ("B2", "B", 1935), ("C2", "C", 1930), ("A1", "B", 1920), ("A1", "C", 1920)]

years :: [Int]
years = clearDubs (map trd3 itemsList) []

clearDubs :: Eq a => [a] -> [a] -> [a]
clearDubs [] res = res
clearDubs ( x : xs ) res
    | x `elem` res = clearDubs xs res
    | otherwise    = clearDubs xs (x : res)


countCategories :: [(String, String, Int)] -> [Int] -> [(Int, Int)] -> [(Int, Int)] 
countCategories _ [] res = res
countCategories items ( y : ys ) res = countCategories items ys (resPair : res)
    where 
        resPair = (y, uniqueCategoriesForTheYearCount)
        uniqueCategoriesForTheYearCount = length (clearDubs (map snd3 itemsFromYear) [])
        itemsFromYear        = filter (\item -> (trd3 item) == y) items

getMaxCategories :: [(Int, Int)] -> Int
getMaxCategories yearCatsCountList = maxCats
    where
        numberOfCats = map snd yearCatsCountList
        maxCats      = maximum numberOfCats


maxYear :: [(String, String, Int)] -> Int
maxYear items = head yearsWithMaxCats
    where
        years             = clearDubs (map trd3 items) []
        yearCatsCountList = countCategories items years []
        maxCats           = getMaxCategories yearCatsCountList
        yearsWithMaxCats  = map fst (filter (\yearCatsPair -> snd yearCatsPair == maxCats) yearCatsCountList)


-- b



-- Task 1

likes :: String -> [String]
likes "a" = ["x", "y"]
likes "b" = ["x", "z"]
likes "c" = ["z"]


kids = ["a", "b", "c"]
dogs = ["x", "y", "z"]

matchDogs :: [String] -> [String] -> (String -> [String]) -> [(String, String)]
matchDogs kids dogs likes = matchDogsHelper kids dogs likes []


-- съжалявам проверяващи :'(
matchDogsHelper :: [String] -> [String] -> (String -> [String]) -> [(String, String)] -> [(String, String)]
matchDogsHelper [] [] likes res     = res
matchDogsHelper kids dogs likes res = matchDogsHelper restOfKids restOfDogs likes newRes
    where 
        everyLikedDog                = map likes kids -- [[dogs]]
        everyLikedDogFiltered        = map (\dogsList -> filter (\dog -> dog `elem` dogs) dogsList) everyLikedDog -- [[dogs]]
        kidsWithTheirDogs            = zip kids everyLikedDogFiltered -- [(kid, [dogs])]
        kidsWithOneLikedDogAndTheDog = filter (\kidDogsPair -> length (snd kidDogsPair) == 1)  kidsWithTheirDogs-- [(kid, [dog])]
        kidDogPairOutput             = map (\kidDogPair -> ( fst kidDogPair, (head (snd kidDogPair)) )) kidsWithOneLikedDogAndTheDog -- [(kid, dog)]
        kidsWithOneDog               = map fst kidDogPairOutput -- [kids]
        dogsWithOneKid               = map snd kidDogPairOutput -- [dogs]
        restOfKids                   = filter (\kid -> not (kid `elem` kidsWithOneDog)) kids -- [kids]
        restOfDogs                   = filter (\dog -> not (dog `elem` dogsWithOneKid)) dogs -- [dogs]
        newRes                       = kidDogPairOutput ++ res -- [(kid, dog)]


tree :: [(Int, [Int])]
tree = [ (1, [2,5,6]), (2, [1,3,4]), (3, [2]), (4, [2]), (5, [1]), (6, [1, 7]), (7, [6])] -- после ще филтрирам кои са родителите 

nodesAt :: [(Int, [Int])] -> Int -> Int -> [Int]
nodesAt tree x dist = treeLevel newTree rootNode dist
    where 
        newTree = createTreeWithRoot tree childs root
        rootNode = head (filter (\(node, childs) -> node == x) tree)
        root = fst rootNode
        childs = snd rootNode

-- empty head pri dist = 3...
treeLevel :: [(Int, [Int])] -> (Int, [Int]) -> Int -> [Int]
treeLevel tree (node, childs) 0 = [node]
treeLevel tree (node, []) _ = []
treeLevel tree (node, (c:cs)) level = (treeLevel tree newNode (level - 1)) ++ (treeLevel tree (node, cs) level)
    where 
        newNode = head (filter (\(node, childs) -> node == c) tree)


createTreeWithRoot :: [(Int, [Int])] -> [Int] -> Int -> [(Int, [Int])]
createTreeWithRoot _ [] _ = []
createTreeWithRoot tree rootChilds@( c : cs ) currRootEl = rootNode ++ (createTreeWithRoot shrunkOldTree newRootChilds c) ++ (createTreeWithRoot shrunkOldTree cs currRootEl)
    where 
        rootNode        = filter (\(node, childs) -> node == currRootEl) tree
        filteredTree    = map (\(node, childs) -> (node, filter (\child -> child /= currRootEl) childs)) tree
        shrunkOldTree   = filter (\(node, childs) -> node /= currRootEl) filteredTree
        newRootChilds   = snd (head (filter (\(node, childs) -> node == c) shrunkOldTree))
