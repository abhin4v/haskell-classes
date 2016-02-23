module FunctionSyntax where

sumTill :: Int -> Int
sumTill 1 = 1
sumTill n = n + sumTill (n - 1)

sumTill' :: Int -> Int
sumTill' n
 | n == 1 = 1
 | otherwise = n + sumTill' (n - 1)

sumTill'' :: Int -> Int
sumTill'' x = case x of
 1 | x > 0 -> 1
 _ -> x + sumTill'' (x - 1)

rot13Char :: Char -> Char
rot13Char c
 | isUpper = undefined
 | isLower = undefined
 | otherwise = error "ERROR"
 where
   isUpper = 'A' <= c && c <= 'Z'
   isLower = 'a' <= c && c <= 'z'

addOneSquare :: Int -> Int
addOneSquare x =
  let y = x + 1
      z = y + x
  in y * y + z
