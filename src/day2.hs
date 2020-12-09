module Main where
import System.Environment
import Data.List.Split (splitOn)

count :: [String] -> Int
count = foldl (\b a -> if solveNew a then b + 1 else b) 0

solve :: String -> Bool
solve cont =
  let
    [num1,rest1] = splitOn "-" cont
    (num2:chcol:str:_) = splitOn " " rest1
    a = read num1 :: Int
    b = read num2 :: Int
    ch = head chcol
    f = freq ch str
  in
    if f >= a && f <= b
    then True
    else False

solveNew :: String -> Bool
solveNew cont =
  let
    [num1,rest1] = splitOn "-" cont
    (num2:chcol:str:_) = splitOn " " rest1
    a = read num1 :: Int
    b = read num2 :: Int
    ch = head chcol
    f = freq ch str
  in
    if str!!(a-1) == ch && str!!(b-1) /= ch || str !! (a-1) /= ch && str!!(b-1) == ch
    then True
    else False

freq :: Char -> String -> Int
freq ch = foldl (\b a -> if a == ch then b + 1 else b) 0

main :: IO ()
main = do
  [f] <- getArgs
  inp <- readFile f
  let cont = lines $ inp
  let lb = count cont
  putStrLn $ show lb

