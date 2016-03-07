module TreeMap where

import Data.List (foldl, nub, sort)
import Data.Maybe (fromMaybe)

data TreeMap k v = EmptyNode | Node k v (TreeMap k v) (TreeMap k v) deriving (Show)

insert :: Ord k => TreeMap k v -> (k, v) -> TreeMap k v
insert EmptyNode (k, v) = Node k v EmptyNode EmptyNode
insert (Node nk nv l r) i@(k, v)
  | k == nk = Node k v l r
  | k <  nk = Node nk nv (insert l i) r
  | k >  nk = Node nk nv l (insert r i)

get :: Ord k => TreeMap k v -> k -> Maybe v
get EmptyNode _ = Nothing
get (Node nk nv l r) k
  | k == nk = Just nv
  | k <  nk = get l k
  | k >  nk = get r k

keys :: TreeMap k v -> [k]
keys EmptyNode = []
keys (Node nk nv l r) = keys l ++ [nk] ++ keys r

values :: TreeMap k v -> [v]
values EmptyNode = []
values (Node nk nv l r) = values l ++ [nv] ++ values r

toList :: TreeMap k v -> [(k, v)]
toList EmptyNode = []
toList (Node k v l r) = toList l ++ [(k, v)] ++ toList r

fromList :: Ord k => [(k, v)] -> TreeMap k v
fromList = foldl insert EmptyNode

prop_keysAreUnique :: Ord k => [(k, v)] -> Bool
prop_keysAreUnique l = (length . nub . keys $ t) == (length . keys $ t) where t = fromList l

prop_nonMemberGetIsNothing :: (Ord k, Eq v) => [(k, v)] -> k -> Bool
prop_nonMemberGetIsNothing l notk = (get t notk) == Nothing where t = fromList [(k, v) | (k, v) <- l, k /= notk]

prop_duplicateInsertGetsLastValue :: (Ord k, Eq v) => [(k, v)] -> k -> v -> v -> Bool
prop_duplicateInsertGetsLastValue l k v1 v2 = get (insert (insert t (k, v1)) (k, v2)) k == Just v2 where t = fromList l

prop_toListIsInorder :: (Ord k, Eq v) => [(k, v)] -> Bool
prop_toListIsInorder l = (map fst . toList . fromList) l ==  (nub . sort . map fst) l

prop_insertDoesNotRemoveKeys :: (Ord k) => [(k, v)] -> (k, v) -> Bool
prop_insertDoesNotRemoveKeys l i =
  lbefore == lafter || lbefore + 1 == lafter
  where
    lbefore = length . toList t
    lafter = length . toList (insert t i)
    t = fromList l
