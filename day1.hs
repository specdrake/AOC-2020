module Main where

import System.IO
import Control.Monad
import Data.List
import Data.Foldable

solve2 :: [Int] -> Int
solve2 xs =
  let
    srt = sort xs
    lcp = [(x,y) | x <- xs, y <- xs, x /= y, x+y == 2020]
    prd = if not (null lcp)
             then
             fst (head lcp) * snd (head lcp)
             else
             0
  in
    prd

solve3 :: [Int] -> Int
solve3 xs =
  let
    srt = sort xs
    lcp = [(x,y,z) | x <- xs, y <- xs, z <- xs, x /= y,y /= z, x /= z,  x+y+z == 2020]
    prd (a,b,c) = a * b * c
  in
    if not (null lcp) then prd (head lcp) else 0



main :: IO ()
main = do
  handle <- openFile "inp1.txt" ReadMode
  contents <- hGetContents handle
  let lns = lines contents
  traverse_ putStrLn lns
  -- pure ()
  -- a <- return $ (return putStrLn) <*> lns
  -- a <- traverse putStrLn lns
  -- foldl (\b a -> putStrLn a) (return ()) lns
  -- foldl (\b a -> b <> putStrLn a) (return ()) lns
  let li = map read lns :: [Int]
  putStrLn . show $ solve2 li
  putStrLn . show $ solve3 li
  -- concat a
  -- a
