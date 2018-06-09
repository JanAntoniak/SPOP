module Board where

data SheepNumber = One | Two | Three | Four deriving (Eq, Show)

data Animal = Sheep SheepNumber | Wolf deriving (Eq, Show)

data Field = White (Maybe Animal) | EmptyBlack | NewPositionForWolf deriving (Eq, Show)

type Board = [[Field]]

emptyBoard :: Board
emptyBoard = do
  let first = [EmptyBlack, White Nothing, EmptyBlack, White Nothing, EmptyBlack, White Nothing, EmptyBlack, White Nothing]
  let second = [White Nothing, EmptyBlack, White Nothing, EmptyBlack, White Nothing, EmptyBlack, White Nothing, EmptyBlack]
  [first, second, first, second, first, second, first, second]

startingBoard :: [[Field]]
startingBoard = [
                [EmptyBlack, White(Just (Sheep One)), EmptyBlack, White(Just (Sheep Two)), EmptyBlack, White(Just (Sheep Three)), EmptyBlack, White(Just (Sheep Four))],
                [White Nothing, EmptyBlack, White Nothing, EmptyBlack, White Nothing, EmptyBlack, White Nothing, EmptyBlack],
                [EmptyBlack, White Nothing, EmptyBlack, White Nothing, EmptyBlack, White Nothing, EmptyBlack, White Nothing],
                [White Nothing, EmptyBlack, White Nothing, EmptyBlack, White Nothing, EmptyBlack, White Nothing, EmptyBlack],
                [EmptyBlack, White Nothing, EmptyBlack, White Nothing, EmptyBlack, White Nothing, EmptyBlack, White Nothing],
                [White Nothing, EmptyBlack, White Nothing, EmptyBlack, White Nothing, EmptyBlack, White Nothing, EmptyBlack],
                [EmptyBlack, White Nothing, EmptyBlack, White Nothing, EmptyBlack, White Nothing, EmptyBlack, White Nothing],
                [NewPositionForWolf, EmptyBlack, NewPositionForWolf, EmptyBlack, NewPositionForWolf, EmptyBlack, NewPositionForWolf, EmptyBlack]
               ]

-- Coordinates
data Coo = None | Coords (Int, Int, Int, Int) deriving Show

type PlayersPositions = ((Int, Int), (Int, Int), (Int, Int), (Int, Int), (Int, Int))

data WolfMovementState = Moved | NotMoved