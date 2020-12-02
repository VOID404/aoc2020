module Main where

import           Data.Char
import           System.Environment

data Rule = Rule Int Int Char deriving (Show)

isValid :: (Rule, String) -> Bool
isValid ((Rule mn mx c), p) = n >= mn && n <= mx
  where n = length . filter (==c) $ p

isValidS :: (Rule, String) -> Bool
isValidS ((Rule a b ch), pass) =
  ((pass !! (a-1)) == ch) /= ((pass !! (b-1)) == ch)


parseLine :: String -> (Rule, String)
parseLine l = (Rule min max ch, pass)
  where w = words l
        pass = last w
        ch = (!! 0) . (!! 1) $ w
        min = read . takeWhile (isNumber) . head $ w :: Int
        max = read . drop 1 . dropWhile (isNumber) . head $ w :: Int

main :: IO ()
main = do
  p <- getArgs
  l <- fromFile (p !! 0)
  print . length $ l

fromFile :: FilePath -> IO [String]
fromFile p = do
  f <- readFile p
  return . filter (isValidS . parseLine) . lines $ f
