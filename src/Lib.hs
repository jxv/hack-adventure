module Lib where

-- Objective leave the basement to search for food then return

import Prelude hiding (putStrLn, putStr)
import qualified Data.Map as Map
import Control.Monad
import Control.Applicative
import Data.Attoparsec.Text
import Data.Text (Text, pack)
import Data.Text.IO (putStrLn, putStr)
import System.Random
import Data.List

data Cmd
  = CmdAct Act
  | CmdQuit
  deriving (Show, Eq)

data Act
  = ActMove Loc
  deriving (Show, Eq)

data Opt
  = OptQuit
  deriving (Show, Eq)

data Pack = Pack

data Loc
  = LocBasement
  | LocAttic
  | LocStaircase
  | LocDiningRoom
  | LocLivingRoom
  | LocOffice
  | LocGreenRoom
  | LocBlueRoom
  | LocRedRoom
  | LocDarkRoom
  deriving (Show, Eq, Ord, Enum)

data Item = Item

data Ch = Ch

data Did
  = DidText Text
  | DidDied Text
  | DidWin
  deriving (Show, Eq)

data Game = Game
  { _locEdges :: Map.Map Loc [Loc]
  , _curLoc :: Loc
  , _hasFood :: Bool
  , _whereIsFood :: Loc
  }

defLocEdges = Map.fromList
  [ (LocBasement, [LocDarkRoom])
  , (LocAttic, [LocStaircase])
  , (LocStaircase, [LocAttic, LocDiningRoom])
  , (LocDiningRoom, [LocStaircase, LocLivingRoom])
  , (LocLivingRoom, [LocDiningRoom, LocDarkRoom])
  , (LocOffice, [LocGreenRoom, LocBlueRoom])
  , (LocGreenRoom, [LocOffice, LocRedRoom])
  , (LocBlueRoom, [LocOffice, LocGreenRoom])
  , (LocRedRoom, [LocDarkRoom, LocGreenRoom])
  , (LocDarkRoom, [LocBasement, LocRedRoom, LocLivingRoom])
  ]

runPar :: Parser a -> Text -> Maybe a
runPar par text = maybeResult $ parse par text

tryCvt :: Text -> a -> Parser a
tryCvt text loc = try $ asciiCI text *> pure loc

cmdPar :: Parser Cmd
cmdPar = (CmdAct <$> movePar) <|> (tryCvt "quit" CmdQuit)

movePar :: Parser Act
movePar = asciiCI "move" *> many1 space *> (ActMove <$> locPar)

locPar :: Parser Loc
locPar =
  (tryCvt "basement" LocBasement) <|>
  (tryCvt "attic" LocAttic) <|>
  (tryCvt "staircase" LocStaircase) <|>
  (tryCvt "dining-room" LocDiningRoom) <|>
  (tryCvt "living-room" LocLivingRoom) <|>
  (tryCvt "office" LocOffice) <|>
  (tryCvt "blue-room" LocBlueRoom) <|>
  (tryCvt "red-room" LocRedRoom) <|>
  (tryCvt "green-room" LocGreenRoom) <|>
  (tryCvt "dark-room" LocDarkRoom)

showLoc :: Loc -> Text
showLoc = \case
  LocBasement -> "basement"
  LocAttic -> "attic"
  LocStaircase -> "staircase"
  LocDiningRoom -> "dining-room"
  LocLivingRoom -> "living-room"
  LocOffice -> "office"
  LocGreenRoom -> "green-room"
  LocBlueRoom -> "blue-room"
  LocRedRoom -> "red-room"
  LocDarkRoom -> "dark-room"

nearbyLocs :: Game -> [Loc]
nearbyLocs g = _locEdges g Map.! _curLoc g

showLocs :: [Loc] -> Text
showLocs locs = mconcat $ intersperse " " $ map showLoc locs

move :: Game -> Loc -> IO (Game, Did)
move g loc = do
  let hadFood = _hasFood g
  let g' = g
        { _curLoc = loc
        , _hasFood = hadFood || (loc == _whereIsFood g)
        }
  let did
        | loc == LocBasement && hadFood = DidWin
        | (not hadFood) && _hasFood g' = DidText $ "you found food!\nnearby locations:" `mappend` showLocs (nearbyLocs g')
        | otherwise = DidText $ "you stepped.\nnearby locations: " `mappend` showLocs (nearbyLocs g')
  return (g', did) 

step :: Game -> Act -> IO (Game, Did)
step g (ActMove loc) = do
  let nearbys = nearbyLocs g
  if elem loc nearbys
    then move g loc
    else do
      return (g, DidText "you can't go that way")

loop :: Game -> IO ()
loop game = do
  input <- pack <$> getLine
  let mCmd = runPar cmdPar input
  case mCmd of
    Nothing -> do
      putStrLn "bad input"
      loop game
    Just cmd ->
      case cmd of
        CmdAct act -> do
          (game', did) <- step game act
          done <- doDid did
          unless done (loop game')
        CmdQuit -> do
          putStrLn "bye!"

doDid :: Did -> IO Bool
doDid (DidText text) = do
  putStrLn text
  return False
doDid (DidDied text) = do
  putStr "Died: "
  putStrLn text
  return True
doDid DidWin = do
  putStrLn "You win!"
  return True

randomFoodLoc :: IO Loc
randomFoodLoc = do
  int <- randomIO
  let choices = [LocOffice, LocBlueRoom, LocGreenRoom, LocAttic]
  return $ choices !! (int `mod` (length choices))

run :: IO ()
run = do
  putStrLn "you're in the basement. find food and come back!"
  putStrLn "hint: go into the dark-room"
  whereIsFood <- randomFoodLoc
  let game = Game
        defLocEdges
        LocBasement
        False
        whereIsFood
  loop game
 
