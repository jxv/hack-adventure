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

newtype Loc = Loc Text
  deriving (Show, Eq, Ord)

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
  [ (Loc "basement", [Loc "dark-room"])
  , (Loc "dark-room", [Loc "basement", Loc "red-room"])
  , (Loc "red-room", [Loc "dark-room"])
  ]

runPar :: Parser a -> Text -> Maybe a
runPar par text = maybeResult $ parse par text

tryCvt :: Text -> a -> Parser a
tryCvt text loc = try $ asciiCI text *> pure loc

cmdPar :: [Loc] -> Parser Cmd
cmdPar locs = (CmdAct <$> movePar locs) <|> (tryCvt "quit" CmdQuit)

movePar :: [Loc] -> Parser Act
movePar locs = asciiCI "move" *> many1 space *> (ActMove <$> locsPar locs)

locPar :: Loc -> Parser Loc
locPar loc@(Loc locText) = tryCvt locText loc

locsPar :: [Loc] -> Parser Loc
locsPar locs = choice (map locPar locs)

showLoc :: Loc -> Text
showLoc (Loc locText) = locText

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
        | loc == (Loc "basement") && hadFood = DidWin
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
  let mCmd = runPar (cmdPar . Map.keys $ _locEdges game) input
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
  let choices = [Loc "red-room"]
  return $ choices !! (int `mod` (length choices))

run :: IO ()
run = do
  putStrLn "you're in the basement. find food and come back!"
  putStrLn "hint: go into the dark-room"
  whereIsFood <- randomFoodLoc
  let game = Game
        defLocEdges
        (Loc "basement")
        False
        whereIsFood
  loop game
 
