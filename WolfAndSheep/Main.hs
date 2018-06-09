module Main where

import Board
import Config
import Utils
import Minimax
import FileManagement

import Graphics.UI.Gtk

main :: IO ()
main = do
  initGUI
  window <- windowNew
  set window [windowTitle := "Wolf and Sheep", containerBorderWidth := 0,
                windowDefaultWidth := width, windowDefaultHeight := height]
  createTable window startingBoard 0
  onDestroy window mainQuit
  mainGUI

createTable :: Window -> Board -> Int -> IO ()
createTable window board counter = do
  table <- createBoard 8 12 window board counter
  widgetShowAll table
  containerAdd window table
  widgetShowAll window

updateTable :: Window -> Board -> Int -> IO ()
updateTable window board counter = do ----------------------------TU DODANY COUNTER ------------------------------------
  table <- createBoard 8 12 window board counter
  widgetShowAll table
  children <- containerGetChildren window
  containerRemove window (head children)
  containerAdd window table
  widgetShowAll table
  widgetShowAll window

createBoard :: Int -> Int -> Window -> Board -> Int -> IO Table
createBoard heightT widthT window board counter = do
  table <- tableNew heightT widthT True
  table <- createPlayingBoardFromField window board table (Coords (0, 1, 0, 1)) counter
  createNewGameButton table window counter
  createSaveButton table window board
  createLoadButton table window
  createCloseButton table window
  widgetShowAll table
  return table

createPlayingBoardFromField :: Window -> Board -> Table -> Coo -> Int -> IO Table
createPlayingBoardFromField window board table None _ = return table
createPlayingBoardFromField window board table (Coords (x1, x2, y1, y2)) counter = do
  fieldButton window (board!!x1!!y1) table board (Coords (x1, x2, y1, y2)) counter
  createPlayingBoardFromField window board table (next (Coords (x1, x2, y1, y2))) counter

next :: Coo -> Coo
next None = None
next (Coords (0, 0, 0, 0)) = Coords (0, 1, 0, 1)
next (Coords (x1, x2, y1, y2)) =
  case (x2, y2) of (8, 8) -> None
                   (8, y) -> Coords (0, 1, y, y+1)
                   (x, 8) -> Coords (x, x+1, 7, 8)
                   (x, y) -> Coords (x, x+1, y1, y)

createNewGameButton :: Table -> Window -> Int -> IO (ConnectId Button)
createNewGameButton table window counter = do
  newGameButton <- myNewButton table "" "images/NewGame.png" 9 11 0 1
  onClicked newGameButton $ (updateTable window startingBoard (counter+1))

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
      updateTable window board 5
  widgetDestroy infoPopup

-- Only wolf should be movable from level of button.
-- Sheep will be played by kind of AI.
fieldButton :: Window -> Field -> Table -> Board -> Coo -> Int -> IO (ConnectId Button)
fieldButton window field table board None _ =  do
  btn <- myNewButton table "I shouldn't be here!" "images/White.png" 0 1 0 1
  doNothingOnClick btn
fieldButton window EmptyBlack table board (Coords (x1, y1, x2, y2)) _ = do
  btn <- myNewButton table "" "images/Black.png" x2 y2 x1 y1
  doNothingOnClick btn
fieldButton window (White (Just (Sheep _))) table board (Coords (x1, y1, x2, y2)) _ = do
  btn <- myNewButton table "" "images/Sheep.gif" x2 y2 x1 y1
  doNothingOnClick btn
fieldButton window (White (Just Wolf)) table board (Coords (x1, y1, x2, y2)) _ = do
  btn <- myNewButton table "" "images/Wolf.gif" x2 y2 x1 y1
  doNothingOnClick btn
fieldButton window (White Nothing) table board (Coords (x1, y1, x2, y2)) counter = do
  btn <- myNewButton table "" "images/White.png" x2 y2 x1 y1
  let coo = Coords (x1, y1, x2, y2)
  onClicked btn (do
    counter <- moveWolfHere window board coo (possiblePlaces coo) (White (Just Wolf)) True (counter+1)
    return ())
fieldButton window NewPositionForWolf table board (Coords (x1, y1, x2, y2)) _ = do
  btn <- myNewButton table "" "images/Mouse.png" x2 y2 x1 y1
  let lookUpPlaces = [(i,j) | i <- [0..7], j <- [0..7]]
  let coo = Coords (x1, y1, x2, y2)
  onClicked btn (do
    moveWolfHere window board coo lookUpPlaces NewPositionForWolf False 0
    return ())

aiMove window board deep counter = do
  updatedBoard <- minimax board deep
  res <- updateTable window updatedBoard counter
  return updatedBoard

doNothingOnClick btn = do onClicked btn (return ())

moveWolfHere window board (Coords (x1, y1, x2, y2)) placesForLookUp filterElement isAiTurn counter = do
  let isElement = \(x,y) -> board!!x!!y == filterElement
  conRes <- case filter isElement placesForLookUp of []  -> do
                                                             return counter
                                                     lst -> do
                                                              board <- replaceWithWhiteField (return board) lst
                                                              board <- putWolfIn board (Coords (x1, y1, x2, y2))
                                                              updateTable window board (counter+1)
                                                              putStrLn (show (deep counter))
                                                              putStrLn (show counter)
                                                              if (isWinner (Coords (x1, y1, x2, y2)))
                                                                then createPopupWin window counter
                                                                else return ()
                                                              newBoard <- if isAiTurn
                                                                then aiMove window board (deep counter) counter
                                                                else return board
                                                              if isAIWinner newBoard (Coords (x1, y1, x2, y2))
                                                                then createPopupLost window counter
                                                                else return ()
                                                              return counter
  return conRes

------------------------------------------------------------------------------------------------------------------------
deep counter =
  if (elem counter [0..4])
    then 1
    else if (elem counter [5..8])
      then sndLvl
      else lstLvl

isAIWinner board (Coords (x1, y1, x2, y2)) =
  (filter (\x -> case x of (White Nothing) -> True
                           _               -> False)
    (map (\x -> board!!fst(x)!!snd(x)) (possiblePlaces (Coords (x1, y1, x2, y2))))) == []

createPopupWithMsg window msg counter = do
  infoPopup <- messageDialogNew Nothing [] MessageInfo ButtonsNone msg
  widgetShow infoPopup
  updateTable window startingBoard 0
createPopupWin window counter = createPopupWithMsg window "You win! Play again!" counter
createPopupLost window counter = createPopupWithMsg window "You lost! Play again!" counter
------------------------------------------------------------------------------------------------------------------------
isWinner (Coords (x1, y1, x2, y2)) = x1 == 0
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

possiblePlaces :: Coo -> [(Int, Int)]
possiblePlaces (Coords (x1, y1, x2, y2)) =
  case (x1, x2) of (7, 0) -> [(6,1)]
                   (0, 7) -> [(1,6)]
                   (7, x) -> [(6,(x-1)), (6,(x+1))]
                   (y, 7) -> [((y-1),6), ((y+1),6)]
                   (0, x) -> [(1,(x-1)), (1,(x+1))]
                   (y, 0) -> [((y-1),1), ((y+1),1)]
                   (y, x) -> [((y-1),(x-1)), ((y+1),(x-1)), ((y-1),(x+1)), ((y+1),(x+1))]

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