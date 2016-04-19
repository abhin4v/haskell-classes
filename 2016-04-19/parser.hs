module Parser where

import qualified Data.Char as Char
import Control.Applicative

newtype Parser i o = Parser { runParser :: i -> Maybe (o, i) }

evalParser :: Parser a b -> a -> Maybe b
evalParser p i = fst <$> runParser p i

instance Functor (Parser i) where
  fmap f (Parser p) = Parser $ fmap (\(o, i') -> (f o, i')) . p

instance Applicative (Parser i) where
  pure x = Parser $ \i -> Just (x, i)

  Parser ff <*> Parser fv = Parser $ \i -> case ff i of
    Just (f, i') -> case fv i' of
      Just (v, i'') -> Just (f v, i'')
      Nothing       -> Nothing
    Nothing      -> Nothing

instance Alternative (Parser i) where
  empty = Parser $ const Nothing

  p1 <|> p2 = Parser $ \i -> case runParser p1 i of
    Nothing -> runParser p2 i
    success -> success

predParser :: Show a => (a -> Bool) -> Parser [a] a
predParser p = Parser $ \l -> case l of
  (x:xs) | p x -> Just (x, xs)
  _            -> Nothing

charParser :: Char -> Parser String Char
charParser c = predParser (== c)

newtype Digit = Digit { getIntOfDigit :: Int } deriving (Show, Eq, Ord)

charToDigit :: Char -> Maybe Digit
charToDigit c =
  if Char.isDigit c then Just $ Digit (Char.digitToInt c) else Nothing

digitParser :: Parser String Digit
digitParser = Parser $ \i -> case i of
  (c : cs) -> fmap (\d -> (d, cs)) . charToDigit $ c
  _        -> Nothing

digitsParser :: Parser String [Digit]
digitsParser = Parser $ \i -> case runParser digitParser i of
  Nothing      -> Nothing
  Just (d, i') ->  case runParser digitsParser i' of
    Nothing        -> Just ([d], i')
    Just (ds, i'') -> Just (d:ds, i'')

-- numberParser :: Parser String Int
-- numberParser = foldl (\acc -> (acc * 10 +) . getIntOfDigit) 0 <$> digitsParser

numberParser :: Parser String Int
numberParser = foldl (\acc -> (acc * 10 +) . getIntOfDigit) 0 <$> some digitParser

wordParser :: Parser String String
wordParser = some (predParser Char.isAlphaNum)

whitespaceParser :: Parser String String
whitespaceParser = many (predParser Char.isSpace)

data Person = Person String Int deriving (Show)

whitespacedParser :: Parser String a -> Parser String a
whitespacedParser p = whitespaceParser *> p <* whitespaceParser

personParser :: Parser String Person
personParser =
  whitespacedParser (charParser '(') *> rawPersonParser <* whitespacedParser (charParser ')')
  where
    nameParser      = unwords <$> some (whitespacedParser wordParser)
    ageParser       = whitespacedParser numberParser
    rawPersonParser = pure Person <*> (nameParser <* charParser ',') <*> ageParser
