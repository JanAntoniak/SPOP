module Test where

main :: IO ()

a :: [Int]
a = foldl (++) [] [[1,2], [5,6]]

main = do
  putStrLn (show a)
  return ()