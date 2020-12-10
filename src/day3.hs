module Main where
import System.Environment
import Data.List.Split (splitOn)

-- solve :: Int -> Int -> [String] -> Int
-- solve w r cont = go w cont (0,0)
--   where
--     go :: Int -> [String] -> (Int, Int) -> Int
--     go _ [] (c,ans) = ans
--     go w (x:xs) (c,ans) = if x!!(c `rem` w) == '#'
--                           then go w xs (c+3, ans+1)
--                           else go w xs (c+3, ans)

solve :: Int -> Int -> [String] -> (Int, Int) -> Int
solve w r cont (right, down) = go w cont (0,0)
  where
    go :: Int -> [String] -> (Int, Int) -> Int
    go _ [] (c,ans) = ans
    go w (x:xs) (c,ans) =
      if x!!(c `rem` w) == '#'
      then go w (drop (down-1) xs) (c+right, ans+1)
      else go w (drop (down-1) xs) (c+right, ans)



solveNew :: Int -> Int -> [String] -> [(Int, Int)] -> Int
solveNew w r cont lslp = product $ solve w r cont <$> lslp

main :: IO ()
main = do
  [f] <- getArgs
  inp <- readFile f
  let cont = lines $ inp
  let r = length cont
  let w = length . head $ cont
  let ans = solve w r cont (3,1)
  let lslp = [(1,1),(3,1),(5,1),(7,1),(1,2)]
  let ansNew = solveNew w r cont lslp
  -- let lb = count cont
  putStrLn $ "width = " ++ show w ++ " rows = " ++ show r ++ " ans = " ++ show ans
              ++ " ansNew = " ++ show ansNew

