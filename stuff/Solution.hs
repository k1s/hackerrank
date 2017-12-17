module Solution where

import qualified Data.List.Split as Split
import Debug.Trace
import qualified Data.Char as Char


debug = flip trace


evenOrOdd :: Integral a => a -> [Char]
evenOrOdd n 
  | even n = "Even"
  | otherwise = "Odd"


nbYear :: Int -> Double -> Int -> Int -> Int
nbYear p0 percent aug p 
  | p0 >= p = 0
  | otherwise = 1 + nbYear next percent aug p
    where next = (p0 + (floor $ percent / 100 * fromIntegral p0) + aug)


rowSumOddNumbers :: Integer -> Integer
rowSumOddNumbers = (^ 3)


sumFibs :: Int -> Integer
sumFibs n = sum . filter even . take (n + 1) $ fibs
  where fibs = 0 : 1 : zipWith (+) fibs (tail fibs)


validParentheses :: String -> Bool
validParentheses s = parentsHelper 0 s where
  parentsHelper acc [] = acc == 0
  parentsHelper acc (h:t)
      | acc < 0   = False
      | h == '('  = parentsHelper (acc + 1) t
      | h == ')'  = parentsHelper (acc - 1) t
      | otherwise = parentsHelper acc t


validParentheses1 :: String -> [Integer]
validParentheses1 s = takeWhile (< 0) . scanl check 0 $ s
  where check acc '(' = acc + 1
        check acc ')' = acc - 1
        check acc _ = acc


firstUpper :: String -> String
firstUpper [] = []
firstUpper (h:t) = (Char.toUpper h):t

toCamelCase :: String -> String
toCamelCase s = concat $ first:(map firstUpper rest)
  where first:rest = Split.splitWhen (\x -> x == '_' || x == '-') $ s



