module Minimax where

--    1  2  3  ...  8
--  1 B  S  B  ...  S
--  2 W  B  W  ...  B
--  3
--  .
--  .
--  .
--  8
import Board
import Data.List

type Value = Maybe Int

data Tree = Node (GameTree, Value, [Tree]) | NoTree deriving Show

--                  Wolf       Sheep
type GameTree = ((Int, Int), [(Int, Int)])

emptyGameTree = ((9,9), [])

data Action = Min | Max

-- evaluateTree :: Tree -> Animal -> Tree
--
-- evaluate :: GameTree -> [GameTree]
-- evaluate = ???
--
-- score :: GameTree -> Int
-- score = ???


startingBoard2 = [
                [EmptyBlack, White(Just Sheep), EmptyBlack, White(Just Sheep), EmptyBlack, White(Just Sheep), EmptyBlack, White(Just Sheep)],
                [White Nothing, EmptyBlack, White Nothing, EmptyBlack, White Nothing, EmptyBlack, White Nothing, EmptyBlack],
                [EmptyBlack, White Nothing, EmptyBlack, White Nothing, EmptyBlack, White Nothing, EmptyBlack, White Nothing],
                [White Nothing, EmptyBlack, White Nothing, EmptyBlack, White Nothing, EmptyBlack, White Nothing, EmptyBlack],
                [EmptyBlack, White Nothing, EmptyBlack, White Nothing, EmptyBlack, White Nothing, EmptyBlack, White Nothing],
                [White Nothing, EmptyBlack, White Nothing, EmptyBlack, White Nothing, EmptyBlack, White Nothing, EmptyBlack],
                [EmptyBlack, White Nothing, EmptyBlack, White Nothing, EmptyBlack, White Nothing, EmptyBlack, White Nothing],
                [NewPositionForWolf, EmptyBlack, White(Just Wolf), EmptyBlack, NewPositionForWolf, EmptyBlack, NewPositionForWolf, EmptyBlack]
               ]





-- createGameTree :: Board -> GameTree
createGameTree board = createGameTree' board 1 1 emptyGameTree

-- createGameTree' :: Board -> Int -> Int -> GameTree
createGameTree' [] x y gameTree = gameTree
createGameTree' (row:rows) x y gameTree = createGameTree' rows x (y+1) (findInRow row x y gameTree)

-- findInRow :: [Field] -> Int -> Int -> GameTree -> GameTree
findInRow [] _ _ gameTree = gameTree
findInRow ((White(Just Sheep)):rest) x y gameTree = findInRow rest (x+1) y (updateGameTree gameTree x y Sheep)
findInRow ((White(Just Wolf)):rest) x y gameTree = findInRow rest (x+1) y (updateGameTree gameTree x y Wolf)
findInRow (el:elems) x y gameTree = findInRow elems (x+1) y gameTree

updateGameTree (_, sheep) x y Wolf = ((y,x), sheep)
updateGameTree (w, sheep) x y Sheep = (w, (y,x):sheep)

-- createTreeRoot :: GameTree -> Tree
createTreeRoot gameTree = Node (gameTree, Nothing, [NoTree])

-- Wolf always minimizes, Sheep always maximizes
-- expandedTree :: Tree -> Int -> Animal ->  Tree
expandedTree tree 0 _ = tree
expandedTree NoTree deep animal = NoTree
expandedTree (Node ((w, s), v, t)) deep Wolf = Node((w,s), v, (map foo (expandMoves [w] s)))
expandedTree (Node ((w, s), v, t)) deep Sheep = map foo (expandMoves s [w])
-- expandedTree tree deep animal action =

-- expandMoves :: [(Int,Int)] -> [GameTree]
expandMoves [(x,y)] rest= map (\x -> (x, rest)) [expandMovesWolf (x,y) rest]
expandMoves lst wolf = expandMovesSheep lst

-- both like (Int, Int) -> [(Int, Int)]
expandMovesWolf (x,y) rest =
  filter (\x -> ((elem fst(x) $ range (0,8)) && (elem snd(x) $ range (0,8 )))) ([(x+1,y+1), (x-1,y-1), (x-1, y+1), (x+1, y-1)] \\ rest)
expandMovesSheep (x,y) = rozwinac ruchy owiec i usunac za plansza i te na wilku -> zwrócić listę gameTreesów

-- :: [(Int, Int)] -> (Int, Int) -> [(Int, Int)]
removeWolf sheepPositions wolfPosition = ???

-- minimax :: Board -> Int -> Animal -> Action -> Tree
-- minimax board deep animal action = do
--   expandedTree <- expandTree (createTreeRoot board) deep animal action
--   filledTree <- fillTree expandedTree
--   move <- chooseMove filledTree



main = do
  putStrLn(show (createGameTree startingBoard2))
  putStrLn(show (createTreeRoot (createGameTree startingBoard2)))
  return ()
