module Main where

import Board
import Config
import FileManagement
import Graphics.UI.Gtk

main :: IO ()
main = do
  initGUI
  window <- windowNew
  set window [windowTitle := "Wolf and Sheep", containerBorderWidth := 0,
                windowDefaultWidth := width, windowDefaultHeight := height]
  createTable window startingBoard
  onDestroy window mainQuit
  mainGUI

createTable :: Window -> Board -> IO ()
createTable window board = do
  table <- createBoard 8 12 window board
  widgetShowAll table
  containerAdd window table
  widgetShowAll window

updateTable :: Window -> Board -> IO ()
updateTable window board = do
  table <- createBoard 8 12 window board
  widgetShowAll table
  children <- containerGetChildren window
  containerRemove window (head children)
  containerAdd window table
  widgetShowAll table
  widgetShowAll window

createBoard :: Int -> Int -> Window -> Board -> IO Table
createBoard heightT widthT window board = do
  table <- tableNew heightT widthT True
  table <- createPlayingBoardFromField window board table (Coords (0, 1, 0, 1))
  createNewGameButton table window
  createSaveButton table window board
  createLoadButton table window
  createCloseButton table window
  widgetShowAll table
  return table

createPlayingBoardFromField :: Window -> Board -> Table -> Coo -> IO Table
createPlayingBoardFromField window board table None = return table
createPlayingBoardFromField window board table (Coords (x1, x2, y1, y2)) = do
  fieldButton window (board!!x1!!y1) table board (Coords (x1, x2, y1, y2))
  createPlayingBoardFromField window board table (next (Coords (x1, x2, y1, y2)))

next :: Coo -> Coo
next None = None
next (Coords (0, 0, 0, 0)) = Coords (0, 1, 0, 1)
next (Coords (x1, x2, y1, y2)) =
  case (x2, y2) of (8, 8) -> None
                   (8, y) -> Coords (0, 1, y, y+1)
                   (x, 8) -> Coords (x, x+1, 7, 8)
                   (x, y) -> Coords (x, x+1, y1, y)

createNewGameButton :: Table -> Window -> IO (ConnectId Button)
createNewGameButton table window = do
  newGameButton <- myNewButton table "" "images/NewGame.png" 9 11 0 1
  onClicked newGameButton $ (updateTable window startingBoard)

createSaveButton :: Table -> Window -> Board -> IO (ConnectId Button)
createSaveButton table window board = do
  saveButton <- myNewButton table "" "images/Save.png" 9 11 4 5
  onClicked saveButton $ saveGame $ positionsToSave board

createLoadButton :: Table -> Window -> IO (ConnectId Button)
createLoadButton table window = do
  loadButton <- myNewButton table "" "images/Load.png" 9 11 2 3
  infoPopup <- messageDialogNew Nothing [] MessageInfo ButtonsNone "Please wait, loading game..."
  onClicked loadButton $ loadGameFrom (loadGame infoPopup) window infoPopup

createCloseButton :: Table -> Window -> IO (ConnectId Button)
createCloseButton table window = do
  closeButton <- myNewButton table "" "images/Exit.png" 9 11 6 7
  onClicked closeButton (do
    onDestroy window mainQuit
    widgetDestroy window)

--loadGameFrom :: IO (Maybe PlayersPositions) -> Window -> MessageDialog -> IO ()
loadGameFrom ioMaybePos window infoPopup = do
  maybePos <- ioMaybePos
  case maybePos of
    Left _ -> do
      return ()
    (Right Nothing) -> do
      infoOnFailureLoadingPopup <- messageDialogNew Nothing [] MessageInfo ButtonsNone "Error occured when loading game."
      widgetShow infoOnFailureLoadingPopup
    Right (Just lstOfPos) -> do
      board <- createBoardFrom lstOfPos
      updateTable window board
  widgetDestroy infoPopup

createBoardFrom :: PlayersPositions -> IO Board
createBoardFrom ((x1, y1), (x2, y2), (x3, y3), (x4, y4), (x5, y5)) = do
  boardWithWolf <- putWolfIn emptyBoard (Coords (x1, x1+1, y1, y1+1))
  board <- putSheepIn boardWithWolf One (Coords (x2, x2+1, y2, y2+1))
  board <- putSheepIn board Two (Coords (x3, x3+1, y3, y3+1))
  board <- putSheepIn board Three (Coords (x4, x4+1, y4, y4+1))
  board <- putSheepIn board Four (Coords (x5, x5+1, y5, y5+1))
  return board

-- Only wolf should be movable from level of button.
-- Sheep will be played by kind of AI.
fieldButton :: Window -> Field -> Table -> Board -> Coo -> IO (ConnectId Button)
fieldButton window field table board None =  do
  btn <- myNewButton table "I shouldn't be here!" "images/White.png" 0 1 0 1
  doNothingOnClick btn
fieldButton window EmptyBlack table board (Coords (x1, y1, x2, y2)) = do
  btn <- myNewButton table "" "images/Black.png" x2 y2 x1 y1
  doNothingOnClick btn
fieldButton window (White (Just (Sheep _))) table board (Coords (x1, y1, x2, y2)) = do
  btn <- myNewButton table "" "images/Sheep.gif" x2 y2 x1 y1
  doNothingOnClick btn
fieldButton window (White (Just Wolf)) table board (Coords (x1, y1, x2, y2)) = do
  btn <- myNewButton table "" "images/Wolf.gif" x2 y2 x1 y1
  doNothingOnClick btn
fieldButton window (White Nothing) table board (Coords (x1, y1, x2, y2)) = do
  btn <- myNewButton table "" "images/White.png" x2 y2 x1 y1
  let coo = Coords (x1, y1, x2, y2)
  onClicked btn (do
    state <- moveWolfHere window board coo (possiblePlaces coo) (White (Just Wolf))
    case state of Moved -> return aiMove
                  NotMoved -> return (return ())
    return ())
fieldButton window NewPositionForWolf table board (Coords (x1, y1, x2, y2)) = do
  btn <- myNewButton table "" "images/Mouse.png" x2 y2 x1 y1
  let lookUpPlaces = [(i,j) | i <- [0..7], j <- [0..7]]
  let coo = Coords (x1, y1, x2, y2)
  onClicked btn (do
    moveWolfHere window board coo lookUpPlaces NewPositionForWolf
    return ())

aiMove() = ()

doNothingOnClick btn = do onClicked btn (return ())

moveWolfHere window board (Coords (x1, y1, x2, y2)) placesForLookUp filterElement = do
  let isElement = \(x,y) -> board!!x!!y == filterElement
  board <- case filter isElement placesForLookUp of []  -> do return NotMoved
                                                    lst -> do
                                                             board <- replaceWithWhiteField (return board) lst
                                                             board <- putWolfIn board (Coords (x1, y1, x2, y2))
                                                             updateTable window board
                                                             return Moved
  return board

--------------------------------------------------Saving----------------------------------------------------------------

positionsToSave :: Board -> PlayersPositions
positionsToSave board =
  case (playerPositions board 0 [] (White (Just Wolf)))++(playerPositions board 0 [] (White (Just (Sheep One)))) of
    [(x1, y1), (x2, y2), (x3, y3), (x4, y4), (x5, y5)] -> ((y1, x1), (y2, x2), (y3, x3), (y4, x4), (y5, x5))
    _ -> ((7,4), (0,1), (0, 3), (0,5), (0,7))

playerPositions :: Board -> Int -> [(Int, Int)] -> Field -> [(Int, Int)]
playerPositions [] y res field = res
playerPositions (el:els) y res field = playerPositions els (y+1) (res++(findPlayer el 0 y [] field)) field

findPlayer :: [Field] -> Int -> Int -> [(Int, Int)] -> Field -> [(Int, Int)]
findPlayer [] _ _ res field = res
findPlayer (el:els) x y res field =
  if el == field
    then findPlayer els (x+1) y ([(x,y)]++res) field
  else findPlayer els (x+1) y res field

------------------------------------------------------------------------------------------------------------------------

replaceWithWhiteField :: IO Board -> [(Int, Int)] -> IO Board
replaceWithWhiteField board [] = do
  b <- board
  return b
replaceWithWhiteField board ((x1, x2):rest) = do
    b <- board
    replaceWithWhiteField (return (replaceNth x1 (replaceNth x2 (White Nothing) (b!!x1)) b)) rest

putElemIn :: Field -> Board -> Coo -> IO Board
putElemIn elem board (Coords (x1, y1, x2, y2)) = do
  return (replaceNth x1 (replaceNth x2 elem (board!!x1)) board)

putSheepIn :: Board -> SheepNumber -> Coo -> IO Board
putSheepIn board number coo = putElemIn (White (Just (Sheep number))) board coo

putWolfIn :: Board -> Coo -> IO Board
putWolfIn board coo = putElemIn (White (Just Wolf)) board coo

possiblePlaces :: Coo -> [(Int, Int)]
possiblePlaces (Coords (x1, y1, x2, y2)) =
  case (x1, x2) of (7, 0) -> [(6,1)]
                   (0, 7) -> [(1,6)]
                   (7, x) -> [(6,(x-1)), (6,(x+1))]
                   (y, 7) -> [((y-1),6), ((y+1),6)]
                   (0, x) -> [(1,(x-1)), (1,(x+1))]
                   (y, 0) -> [((y-1),1), ((y+1),1)]
                   (y, x) -> [((y-1),(x-1)), ((y+1),(x-1)), ((y-1),(x+1)), ((y+1),(x+1))]

replaceNth :: Int -> t1 -> [t1] -> [t1]
replaceNth n newVal (x:xs)
  | n == 0 = newVal:xs
  | otherwise = x:replaceNth (n-1) newVal xs

myNewButton :: Table -> String -> String -> Int -> Int -> Int -> Int -> IO Button
myNewButton table label path x1 y1 x2 y2 =
  do
    button <- buttonNewWithLabel label
    showImageOnButton button path
    tableAttach table button x1 y1 x2 y2 [Fill, Expand] [Fill, Expand] 1 1
    return button

showImageOnButton :: Button -> String -> IO()
showImageOnButton button filePath = do
    children <- containerGetChildren button
    containerRemove button (head children)
    image <- imageNewFromFile filePath
    containerAdd button image
    widgetShowAll button