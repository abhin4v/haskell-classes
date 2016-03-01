module BST where

data BST = EmptyNode | Node BST Int BST deriving (Show, Eq)

inOrder :: BST -> [Int]
inOrder EmptyNode    = []
inOrder (Node l v r) = inOrder l ++ [v] ++ inOrder r

find :: BST -> Int -> Bool
find EmptyNode _ = False
find (Node l v r) x
  | x == v    = True
  | x > v     = find r x
  | otherwise = find l x

insert :: BST -> Int -> BST
insert EmptyNode x = Node EmptyNode x EmptyNode
insert node@(Node left value right) x
  | x < value = Node (insert left x) value right
  | x > value = Node left value (insert right x)
  | otherwise = node

fromList :: [Int] -> BST
fromList []       = EmptyNode
fromList (first : rest) = insert (fromList rest) first

isBST :: BST -> Bool
isBST EmptyNode = True
isBST (Node EmptyNode _ EmptyNode) = True
isBST (Node l@(Node _ lv _) v EmptyNode) = lv < v && isBST l
isBST (Node EmptyNode v r@(Node _ rv _)) = rv > v && isBST r
isBST (Node l@(Node _ lv _) v r@(Node _ rv _)) = lv < v && v > rv && isBST l && isBST r
