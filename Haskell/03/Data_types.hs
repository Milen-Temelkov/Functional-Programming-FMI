-- Utilities

data BinaryTree a = EmptyTree | Node { root :: a, left :: BinaryTree a, right :: BinaryTree a } deriving (Eq, Show, Read)

makeLeaf :: a -> BinaryTree a
makeLeaf x = Node x EmptyTree EmptyTree

mapBinaryTree :: (a -> b) -> BinaryTree a -> BinaryTree b
mapBinaryTree _ EmptyTree = EmptyTree
mapBinaryTree f (Node x l r) =
    Node (f x) (mapBinaryTree f l) (mapBinaryTree f r)

tree :: BinaryTree Int
tree =
  Node 1
    (Node 2
      (Node 3
        EmptyTree
        EmptyTree)
      EmptyTree)
    (Node 4
      EmptyTree
      EmptyTree)

btree :: BinaryTree Int
btree =
  Node 3
    (Node 1
      EmptyTree
      (Node 2
        EmptyTree
        EmptyTree))
    (Node 4
      EmptyTree
      (Node 5
        EmptyTree
        EmptyTree))


ntree :: BinaryTree Int
ntree =
  Node 3
    (Node 1
      EmptyTree
      (Node 4
        EmptyTree
        EmptyTree))
    (Node 4
      EmptyTree
      (Node 5
        EmptyTree
        EmptyTree))


ptree :: BinaryTree Int
ptree = 
  Node 3 
    EmptyTree
    (Node 7
      (Node 5
        EmptyTree
        (Node 6
          (makeLeaf 4)
          EmptyTree))
      EmptyTree)


-- Task 1

depth :: BinaryTree a -> Int
depth EmptyTree = 0
depth (Node root l r) = 1 + max (depth l) (depth r)

-- Task 2

countLeaves :: BinaryTree a -> Int
countLeaves EmptyTree = 0
countLeaves (Node x EmptyTree EmptyTree) = 1
countLeaves (Node _ l r) = (countLeaves l) + (countLeaves r)

-- Task 3

collectPreOrder :: BinaryTree a -> [a]
collectPreOrder EmptyTree = []
collectPreOrder (Node root l r) = [root] ++ (collectPreOrder l) ++ (collectPreOrder r)

collectInOrder :: BinaryTree a -> [a]
collectInOrder EmptyTree = []
collectInOrder (Node root l r) = (collectInOrder l) ++ [root] ++ (collectInOrder r)

collectPostOrder :: BinaryTree a -> [a]
collectPostOrder EmptyTree = []
collectPostOrder (Node root l r) = (collectPostOrder l) ++ (collectPostOrder r) ++ [root]

-- Task 4

level :: BinaryTree a -> Int -> [a]
level EmptyTree _ = []
level (Node root _ _) 0 = [root]
level (Node _ l r) n = level l (n - 1) ++ level r (n - 1)

-- Task 5

leaf :: BinaryTree a -> Bool
leaf (Node root EmptyTree EmptyTree) = True
leaf _ = False

prune :: BinaryTree a -> BinaryTree a
prune EmptyTree = EmptyTree
prune (Node root l r) 
  | leaf l && not (leaf r) = (Node root EmptyTree (prune r))
  | not (leaf l) && leaf r = (Node root (prune l) EmptyTree)
  | leaf l && leaf r = (Node root EmptyTree EmptyTree)
  | otherwise = (Node root (prune r) (prune l))

-- Task 6

invert :: BinaryTree a -> BinaryTree a
invert EmptyTree = EmptyTree
invert (Node root l r) = (Node root (invert r) (invert l))

-- Task 7

elemT :: Eq a => a -> BinaryTree a -> Bool
elemT _ EmptyTree = False
elemT n (Node root l r) = (n == root) || elemT n l || elemT n r

{-
path :: a -> BinaryTree a -> [a]
path _ EmptyTree = []
path n t@(Node root l r) 
  | not elemT n t = []
  | otherwise = undefined
-}
-- Task 8



-- Task 9

binHelper2 :: (Ord a, Num a) => BinaryTree a -> a -> a -> Bool
binHelper2 EmptyTree _ _ = True
binHelper2 (Node root l r) lower upper = root <= upper && root > lower && (binHelper2 l lower root) && (binHelper2 r root upper)

binarySearchTree :: (Ord a, Num a) => BinaryTree a -> Bool
binarySearchTree EmptyTree = True
binarySearchTree (Node root l r) = (binHelper2 l 0 root) && (binHelper2 r root 10000)


-- Task 10

binarySearchTreeInsert :: Ord a => BinaryTree a -> a -> BinaryTree a
binarySearchTreeInsert EmptyTree n = makeLeaf n
binarySearchTreeInsert (Node root l r) n
  | n <= root = (Node root (binarySearchTreeInsert l n) r)
  | otherwise = (Node root l (binarySearchTreeInsert r n))


-- Task 11

listToTree :: Ord a => [a] -> BinaryTree a -> BinaryTree a
listToTree [] tr = tr
listToTree (x:xs) tr = listToTree xs (binarySearchTreeInsert tr x)


treeSort :: Ord a =>[a] -> [a]
treeSort l = collectInOrder tr
 where tr = (listToTree l EmptyTree)