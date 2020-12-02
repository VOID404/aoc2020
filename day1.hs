module Main where

import           Data.List
import           System.Environment

subsets :: Int -> [a] -> [[a]]
subsets 0 _      = [[]]
subsets _ []     = []
subsets n (x:xs) = map (x:) (subsets (n-1) xs) ++ subsets n xs

type R n = Maybe n
findWithSum :: (Num n, Eq n) => n -> [[n]] -> R [n]
findWithSum s l = find ((==s) . sum) l

day1 :: Int -> [Int] -> R Int
day1 n l = product <$> (findWithSum 2020 . subsets n $ l)

fromFile :: Int -> FilePath -> IO (R Int)
fromFile n p = do
  f <- readFile p
  return . day1 n . map read $ lines f

main :: IO ()
main = do
  p <- getArgs
  print . fromFile $ p !! 1
