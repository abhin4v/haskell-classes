module Parser where

import qualified Data.Char as Char

newtype Parser i o = Parser (i -> Maybe (o, i))

instance Functor (Parser i) where
  fmap f (Parser p) = Parser $ \i -> fmap (\(o, i') -> (f o, i')) $ p i

instance Applicative (Parser i) where
  pure x = Parser $ \i -> Just (x, i)

  Parser ff <*> Parser fv = Parser $ \i -> case ff i of
    Just (f, i') -> case fv i' of
      Just (v, i'') -> Just (f v, i'')
      Nothing -> Nothing
    Nothing -> Nothing

charParser :: Char -> Parser String Char
charParser c = Parser $ \i -> case i of
  (x : xs) | x == c -> Just (c, xs)
  _                 -> Nothing

newtype Digit = Digit { getIntOfDigit :: Int } deriving (Show, Eq, Ord)

charToDigit :: Char -> Maybe Digit
charToDigit c =
  if Char.isDigit c then Just $ Digit (Char.digitToInt c) else Nothing

digitParser :: Parser String Digit
digitParser = Parser $ \i -> case i of
  (c : cs) -> fmap (\d -> (d, cs)) . charToDigit $ c
  _        -> Nothing

numberParser :: Parser String Int
numberParser = undefined

runParser :: i -> Parser i o -> Maybe o
runParser i (Parser p) = fmap fst (p i)
