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
                [EmptyBlack, White(Just (Sheep One)), EmptyBlack, White(Just (Sheep Two)), EmptyBlack, White(Just (Sheep Three)), EmptyBlack, White(Just (Sheep Four))],
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
findInRow ((White(Just (Sheep _))):rest) x y gameTree = findInRow rest (x+1) y (updateGameTree gameTree x y (Sheep One)) -- no matter which Sheep
findInRow ((White(Just Wolf)):rest) x y gameTree = findInRow rest (x+1) y (updateGameTree gameTree x y Wolf)
findInRow (el:elems) x y gameTree = findInRow elems (x+1) y gameTree

updateGameTree (_, sheep) x y Wolf = ((y,x), sheep)
updateGameTree (w, sheep) x y (Sheep _) = (w, (y,x):sheep)

-- createTreeRoot :: GameTree -> Tree
createTreeRoot gameTree = Node (gameTree, Nothing, [NoTree])

--Wolf always minimizes, Sheep always maximizes
--expandTree :: tree -> deep -> animal -> tree
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
-- expandMovesSheep positions wolfPos = (map (\x -> (wolfPos, (getNeighbours x wolfPos))) positions) - delete this

-- to powinno zwracać listę z drzewami czyli liste z tuplami wolf i nowe pozycje owiec
getNeighbours :: [(Int,Int)] -> (Int, Int) -> [GameTree]
getNeighbours positions wolfPos =
  foldl (++) [] (map (\x -> (map (\nP -> (wolfPos, nP:(positions \\ [x]))) (newPos x positions wolfPos))) positions)

newPos :: (Int, Int) -> [(Int, Int)] -> (Int, Int) -> [(Int, Int)]
newPos oldPos positions wolfPos =
  filter filterBoundaries [(newPair oldPos a b) | a <- [-1,1], b <- [-1,1], (not (elem (newPair oldPos a b) (wolfPos:positions)))]
newPair p a b = (fst(p)+a, snd(p)+b)

replace' :: Eq b => b -> b -> [b] -> [b]
replace' a b = map (\x -> if (a == x) then b else x)

filterBoundaries = (\x -> ((elem (fst(x)) [1..8]) && (elem (snd(x)) [1..8])))

-- minimax :: Board -> Int -> Animal -> Action -> Tree
-- minimax board deep animal action = do
--   expandedTree <- expandTree (createTreeRoot board) deep animal action --without action probably
--   filledTree <- fillTree expandedTree
--   move <- chooseMove filledTree



main = do
--   putStrLn(show (createGameTree startingBoard2))
--   putStrLn(show (createTreeRoot (createGameTree startingBoard2)))
-- -- X V | Y -> swapped coords
--   putStrLn(show (expandMoves [(1,8),(1,6),(1,4),(1,2)] [(8,1)]))

  putStrLn(show (expandTree (createTreeRoot (createGameTree startingBoard2)) 2 Wolf))
  return ()
