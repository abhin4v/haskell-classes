module TextEditor where

import Data.Monoid ((<>), Sum(..))

data JoinList m a = Empty | Value m a | JoinList m (JoinList m a) (JoinList m a)
                    deriving (Show, Eq)

meta :: (Monoid m) => JoinList m a -> m
meta Empty = mempty
meta (Value m _) = m
meta (JoinList m _ _ ) = m

instance Monoid m => Monoid (JoinList m a) where
  mempty = Empty

  mappend Empty x = x
  mappend x Empty = x
  mappend left right = JoinList (meta left <> meta right) left right

instance Functor (JoinList m) where
  fmap _ Empty = Empty
  fmap f (Value m x) = Value m (f x)
  fmap f (JoinList m left right) = JoinList m (fmap f left) (fmap f right)

instance Foldable (JoinList m) where
  foldMap _ Empty = mempty
  foldMap f (Value _ x) = f x
  foldMap f (JoinList _ left right) = foldMap f left <> foldMap f right

data Meta = Meta { lineCount :: Sum Int
                 , letterCount :: Sum Int
                 , wordCount :: Sum Int
                 } deriving (Show)

instance Monoid Meta where
  mempty = Meta mempty mempty mempty
  mappend (Meta a b c) (Meta x y z) = Meta (a <> x) (b <> y) (c <> z)

mkMeta :: String -> Meta
mkMeta x = Meta (Sum 1) (Sum . length $ x) (Sum . length . words $ x)

type Text = JoinList Meta String

fromList :: [String] -> Text
fromList []  = Empty
fromList [x] = Value (mkMeta x) x
fromList xs  =
  let (left, right) = splitAt (length xs `div` 2) xs
  in fromList left <> fromList right

getAt :: Int -> Text -> Maybe String
getAt 1 (Value _ x)             = Just x
getAt n (JoinList _ left right) =
  let l = getSum . lineCount . meta $ left
  in if n <= l
    then getAt n left
    else getAt (n - l) right
getAt _ _                       = Nothing

transform :: Int -> String -> Text -> (Int -> String -> Text -> Maybe Text) -> Maybe Text
transform n s (JoinList _ left right) f =
  let l = getSum . lineCount . meta $ left
  in if n <= l
    then fmap (\e -> JoinList (meta e <> meta right) e right) $ f n s left
    else fmap (\e -> JoinList (meta left <> meta e) left e) $ f (n - l) s right
transform _ _ _ _ = Nothing

editAt :: Int -> String -> Text -> Maybe Text
editAt 1 s (Value _ _)   = Just (Value (mkMeta s) s)
editAt n s l@JoinList {} = transform n s l editAt
editAt _ _ _             = Nothing

insertAt :: Int -> String -> Text -> Maybe Text
insertAt 1 s Empty         = Just $ Value (mkMeta s) s
insertAt 1 s v@(Value _ _) =
  let left = Value (mkMeta s) s
  in Just (JoinList (meta left <> meta v) left v)
insertAt n s l@JoinList {} = transform n s l insertAt
insertAt _ _ _             = Nothing
