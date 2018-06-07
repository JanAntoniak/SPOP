module FileManagement where

import Graphics.UI.Gtk
import Control.Exception
import System.Exit
import System.IO
import Board

type Cancel = String

loadGame :: MessageDialog -> IO (Either Cancel (Maybe PlayersPositions))
loadGame infoPopup = do
  initGUI
  putStrLn "Loading ..."
  dialog <- fileChooserDialogNew (Just "Open an existing game.")
                                 Nothing
                                 FileChooserActionOpen
                                 [("Cancel", ResponseCancel),
                                 ("Open", ResponseAccept)]

  widgetShow dialog
  response <- dialogRun dialog
  res <- case response of
    ResponseDeleteEvent ->
      do return (Left "")
    ResponseCancel ->
      do return (Left "")
    ResponseAccept -> do
        fileName <- fileChooserGetFilename dialog
        case fileName of
          Nothing -> do
            widgetShow infoPopup
            return (Right Nothing)
          Just path -> do
            result <- try (load path) :: IO (Either SomeException PlayersPositions)
            case result of
              Left ex -> do
                widgetShow infoPopup
                return (Right Nothing)
              Right positions -> do
                widgetShow infoPopup
                return (Right (validatePositions positions))

  widgetDestroy dialog
  onDestroy dialog mainQuit
  mainGUI
  putStrLn (show res)
  return res

validatePositions :: PlayersPositions -> Maybe PlayersPositions
validatePositions ((x1, y1), (x2, y2), (x3, y3), (x4, y4), (x5, y5)) = do
  let tuple = ((x1, y1), (x2, y2), (x3, y3), (x4, y4), (x5, y5))
  let lst = [(x1, y1), (x2, y2), (x3, y3), (x4, y4), (x5, y5)]
  let result = (foldl (\x y -> x && y) True (map (\x -> verify x) lst))
  case result of True -> Just tuple
                 False -> Nothing

-- check if played is on white position
verify :: (Int, Int) -> Bool
verify (x, y) = (x `mod` 2 == 0 && y `mod` 2 /= 0) || (x `mod` 2 /= 0 && y `mod` 2 == 0)

load :: String -> IO PlayersPositions
load databaseFile =
  withFile databaseFile ReadMode (\inp -> do
    text <- hGetContents inp
    readIO text)

------------------------------------------------------------------------------------------------------------------------

saveGame :: PlayersPositions -> IO ()
saveGame positions = do
  initGUI
  putStrLn "Saving ..."
  dialog <- fileChooserDialogNew (Just "Save this game.")
                                 Nothing
                                 FileChooserActionSave
                                 [("Cancel", ResponseCancel),
                                 ("Save", ResponseAccept)]

  widgetShow dialog
  response <- dialogRun dialog

  res <- case response of
    ResponseDeleteEvent -> return Nothing
    ResponseCancel -> return Nothing
    ResponseAccept -> do
      fileName <- fileChooserGetFilename dialog
      case fileName of
        Nothing -> return Nothing
        Just path -> do
          result <- try (save (show positions) path) :: IO (Either SomeException ())
          case result of
            Left ex -> return Nothing
            Right _ -> return (Just ())

  widgetDestroy dialog
  onDestroy dialog mainQuit
  mainGUI
  return ()

save :: String -> FilePath -> IO ()
save positions filePath = writeFile filePath positions