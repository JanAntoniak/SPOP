module Test where

main :: IO ()

a' :: Int -> Int
a' x = x*x

a :: Int -> Int -> Int
a x y = x+y

main = do
  putStrLn (show (a' 4))
  putStrLn (show (a 4 5))
  return ()