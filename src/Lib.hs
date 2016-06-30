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
  , (LocStaircase, [LocAttic])
  , (LocDiningRoom, [])
  , (LocLivingRoom, [])
  , (LocOffice, [])
  , (LocGreenRoom, [])
  , (LocBlueRoom, [])
  , (LocRedRoom, [LocDarkRoom])
  , (LocDarkRoom, [LocBasement, LocRedRoom])
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

nearbyLocs :: Game -> [Loc]
nearbyLocs g = _locEdges g Map.! _curLoc g

step :: Game -> Act -> IO (Game, Did)
step g (ActMove loc) = do
  let nearbys = nearbyLocs g
  if elem loc nearbys
    then do
      let g' = g { _curLoc = loc }
      return (g', DidText "you stepped")
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
          unless done (loop game)
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
  whereIsFood <- randomFoodLoc
  let game = Game
        defLocEdges
        LocBasement
        False
        whereIsFood
  loop game
 
