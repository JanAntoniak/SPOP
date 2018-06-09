module Minimax where

import Board
import Data.List
import Utils (createBoardFrom', (|>))

type Value = Maybe Int

data Tree = Node (GameTree, Value, [Tree]) | NoTree deriving Show

--                  Wolf       Sheep
type GameTree = ((Int, Int), [(Int, Int)])

-------------------------------------- Creating and expanding game tree-------------------------------------------------

emptyGameTree = ((9,9), [])

createGameTree :: Board -> GameTree
createGameTree board = createGameTree' board 1 1 emptyGameTree

createGameTree' :: Board -> Int -> Int -> GameTree -> GameTree
createGameTree' [] x y gameTree = gameTree
createGameTree' (row:rows) x y gameTree = createGameTree' rows x (y+1) (findInRow row x y gameTree)

findInRow :: [Field] -> Int -> Int -> GameTree -> GameTree
findInRow [] _ _ gameTree = gameTree
findInRow ((White(Just (Sheep _))):rest) x y gameTree = findInRow rest (x+1) y (updateGameTree gameTree x y (Sheep One)) -- no matter which Sheep
findInRow ((White(Just Wolf)):rest) x y gameTree = findInRow rest (x+1) y (updateGameTree gameTree x y Wolf)
findInRow (el:elems) x y gameTree = findInRow elems (x+1) y gameTree

updateGameTree (_, sheep) x y Wolf = ((y,x), sheep)
updateGameTree (w, sheep) x y (Sheep _) = (w, (y,x):sheep)

createTreeRoot :: GameTree -> Tree
createTreeRoot gameTree = Node (gameTree, Nothing, [NoTree])

--Wolf always minimizes, Sheep always maximizes
expandTree :: Tree -> Int -> Animal -> Tree
expandTree tree 0 _ = tree
expandTree NoTree deep animal = NoTree
expandTree (Node ((w, s), v, t)) deep Wolf =
  Node((w,s), v, (map (\x -> (expandTree (Node (x, Nothing, [])) (deep-1) (Sheep One))) (expandMoves [w] s)))
expandTree (Node ((w, s), v, t)) deep (Sheep _) =
  Node((w,s), v, (map (\x -> (expandTree (Node (x, Nothing, [])) (deep-1) (Wolf))) (expandMoves s [w])))

expandMoves :: [(Int,Int)] -> [(Int,Int)] -> [GameTree]
expandMoves [(x,y)] rest = map (\x -> (x, rest)) (expandMovesWolf (x,y) rest)
expandMoves lst wolf = expandMovesSheep lst (head(wolf))

expandMovesWolf :: (Int, Int) -> [(Int, Int)] -> [(Int, Int)]
expandMovesWolf (x,y) rest =
  filter filterBoundaries ([(x+1,y+1), (x-1,y-1), (x-1, y+1), (x+1, y-1)] \\ rest)

expandMovesSheep positions wolfPos = getNeighbours positions wolfPos

getNeighbours :: [(Int,Int)] -> (Int, Int) -> [GameTree]
getNeighbours positions wolfPos =
  foldl (++) [] (map (\x -> (map (\nP -> (wolfPos, nP:(positions \\ [x]))) (newPos x positions wolfPos))) positions)

newPos :: (Int, Int) -> [(Int, Int)] -> (Int, Int) -> [(Int, Int)]
newPos oldPos positions wolfPos =
  filter filterBoundaries [(newPair oldPos a b) | a <- [1], b <- [-1,1], (not (elem (newPair oldPos a b) (wolfPos:positions)))]
newPair p a b = (fst(p)+a, snd(p)+b)

replace' :: Eq b => b -> b -> [b] -> [b]
replace' a b = map (\x -> if (a == x) then b else x)

filterBoundaries = (\x -> ((elem (fst(x)) [1..8]) && (elem (snd(x)) [1..8])))

------------------------------------------ Filling tree with heuristic value -------------------------------------------

fillTree :: Tree -> Tree
fillTree tree = fillTree' tree (Sheep One)

fillTree' :: Tree -> Animal -> Tree
fillTree' (Node((w,s), _, [])) _  = Node ((w,s), (Just (heuristic ((w,s)))), [])
fillTree' (Node((w,s), _, children)) (Sheep _) =
  Node (
    (w,s),
    Just (maximum (map (\x -> case x of (Node ((w,s), (Just val), _)) -> val
                                        _                             -> 0) mappedChildren)),
    mappedChildren) where
    mappedChildren = (map (\x -> (fillTree' x Wolf)) children)
fillTree' (Node((w,s), _, children)) Wolf =
  Node (
    (w,s),
    Just (minimum (map (\x -> case x of (Node ((w,s), (Just val), _)) -> val
                                        _                             -> 0) mappedChildren)),
    mappedChildren) where
    mappedChildren = (map (\x -> (fillTree' x (Sheep One))) children)

heuristic :: GameTree -> Int
heuristic ((w1, w2), sheep) = (heuristic' ((w1, w2), sheep) []) + w1

heuristic' :: GameTree -> [(Int, Int)] -> Int
heuristic' ((w1, w2), sheep) tabu =
  if (expandMovesWolf (w1,w2) sheep) == []
    then 10000
    else if w1 == 1
      then 0
      else if (elem (w1,w2) tabu)
        then 100
        else 1+(min
          (if (canGoRightUp ((w1,w2), sheep))
            then (heuristic' ((w1-1, w2+1), sheep) (((w1,w2)):tabu))
            else if (canGoRightBack ((w1,w2), sheep))
              then (heuristic' ((w1+1, w2+1), sheep) (((w1,w2)):tabu))
              else 100)
          (if (canGoLeftUp ((w1,w2), sheep))
            then (heuristic' ((w1-1, w2-1), sheep) (((w1,w2)):tabu))
            else if (canGoLeftBack ((w1,w2), sheep))
              then (heuristic' ((w1+1, w2-1), sheep) (((w1,w2)):tabu))
              else 100))

canGo :: GameTree -> Bool
canGo ((w1,w2), sheep) = (elem w1 [1..8]) && (elem w2 [1..8]) && (not (elem (w1,w2) sheep))
canGoRightUp :: GameTree -> Bool
canGoRightUp ((w1,w2), sheep) = canGo ((w1-1,w2+1), sheep)
canGoRightBack ((w1,w2), sheep) = canGo ((w1+1,w2+1), sheep)
canGoRightBack :: GameTree -> Bool
canGoLeftUp ((w1,w2), sheep) = canGo ((w1-1,w2-1), sheep)
canGoLeftUp :: GameTree -> Bool
canGoLeftBack :: GameTree -> Bool
canGoLeftBack ((w1,w2), sheep) = canGo ((w1+1,w2-1), sheep)

------------------------------------------------ chooseMove ------------------------------------------------------------

chooseMove :: Tree -> GameTree
chooseMove (Node (_, value, children)) =
  head(
    map (\y -> case y of (Node (gT, _, _)) -> gT
                         _                 -> ((997,997), [])) -- error
    (filter (\x -> case x of (Node (gT, val, _)) -> val == value
                             _                   -> False) children))

------------------------------------------------ updateBoard -----------------------------------------------------------

updateBoardWith :: GameTree -> IO Board
updateBoardWith ((w1,w2), [(s11,s12), (s21,s22), (s31,s32), (s41,s42)]) =
  createBoardFrom' ((w1,w2), (s11,s12), (s21,s22), (s31,s32), (s41,s42))

------------------------------------------------ minimax main algorithm ------------------------------------------------

minimax :: Board -> Int -> IO Board
minimax b d = do
  res <- minimax' b d
  putStrLn("Counting next move...")
  return res

minimax' :: Board -> Int -> IO Board
minimax' board deep =
  (expandTree (createTreeRoot (createGameTree board)) deep (Sheep One)) |> fillTree |> chooseMove |> updateBoardWith