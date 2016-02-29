module DataTypes where

data User = NormalUser { userName :: String
                       , userAge  :: Int }
          | Admin { userName  :: String
                  , userAge   :: Int
                  , adminRole :: String }
          deriving (Show)

data FileAccessMode = Read | Write | Append

data Expr = IntLiteral Int
          | StringLiteral String
          | Addition Expr Expr

showUser :: User -> String
showUser (NormalUser name age) =
  ("Name: " ++ name ++ " Age: " ++ show age)
showUser (Admin name age role) =
  ("Name: " ++ name ++ " Age: " ++ show age ++ " Role: " ++ role)

data IntList = EmptyList | List Int IntList

iNull :: IntList -> Bool
iNull x = case x of { EmptyList -> True; _ -> False }

iLen :: IntList -> Int
iLen x = case x of { EmptyList -> 0; List i rest -> 1 + iLen rest }

iSum x = case x of { EmptyList -> 0; List i rest -> i + iSum rest }
iDouble x = case x of { EmptyList -> EmptyList;
                        List i rest -> iDouble (List (i*2) rest) }
iEven x = case x of { EmptyList -> EmptyList;
                      List a rest -> if even a then List a (iEven rest) else iEven rest }

data BST = EmptyNode | Node {left::BST, value::Int, right::BST} deriving (Show)

inOrder :: BST -> [Int]
inOrder EmptyNode    = []
inOrder (Node l v r) = inOrder l ++ [v] ++ inOrder r

find :: BST -> Int -> Bool
find EmptyNode _ = False
find n@(Node l v r) x
  | x == v    = True
  | x > v     = find r x
  | otherwise = find l x
