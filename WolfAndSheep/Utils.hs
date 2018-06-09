module Utils where

import Config
import Board

(|>) = flip ($)

replaceNth :: Int -> t1 -> [t1] -> [t1]
replaceNth n newVal (x:xs)
  | n == 0 = newVal:xs
  | otherwise = x:replaceNth (n-1) newVal xs

putElemIn :: Field -> Board -> Coo -> IO Board
putElemIn elem board (Coords (x1, y1, x2, y2)) = do
  return (replaceNth x1 (replaceNth x2 elem (board!!x1)) board)

putSheepIn :: Board -> SheepNumber -> Coo -> IO Board
putSheepIn board number coo = putElemIn (White (Just (Sheep number))) board coo

putWolfIn :: Board -> Coo -> IO Board
putWolfIn board coo = putElemIn (White (Just Wolf)) board coo

createBoardFrom :: PlayersPositions -> IO Board
createBoardFrom ((x1, y1), (x2, y2), (x3, y3), (x4, y4), (x5, y5)) = do
  boardWithWolf <- putWolfIn emptyBoard (Coords (x1, x1+1, y1, y1+1))
  board <- putSheepIn boardWithWolf One (Coords (x2, x2+1, y2, y2+1))
  board <- putSheepIn board Two (Coords (x3, x3+1, y3, y3+1))
  board <- putSheepIn board Three (Coords (x4, x4+1, y4, y4+1))
  board <- putSheepIn board Four (Coords (x5, x5+1, y5, y5+1))
  return board

createBoardFrom' :: PlayersPositions -> IO Board
createBoardFrom' ((x1, y1), (x2, y2), (x3, y3), (x4, y4), (x5, y5)) =
  createBoardFrom ((x1-1, y1-1), (x2-1, y2-1), (x3-1, y3-1), (x4-1, y4-1), (x5-1, y5-1))