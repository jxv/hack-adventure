module Lib where

-- Objective leave the basement to search for food then return

import Prelude hiding (putStrLn, putStr)
import qualified Data.Map as Map
import Data.Text (Text, pack)
import Data.Text.IO (putStrLn, putStr)
import Data.String (IsString)
import Control.Monad
import Control.Applicative
import Data.Attoparsec.Text
import System.Random
import Data.List

data Cmd
  = CmdAct Act
  | CmdQuit
  deriving (Show, Eq)

data Act
  = ActMove Loc
  deriving (Show, Eq)

newtype Loc = Loc Text
  deriving (Show, Eq, Ord, IsString)

startLoc :: Loc
startLoc = Loc "basement"

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

baseEdgeCount = 30

genEdges :: [Loc] -> IO (Map.Map Loc [Loc])
genEdges locs = do
  pairs <- sequence $ replicate (baseEdgeCount * 2) ((,) <$> randomIO <*> randomIO)
  return $ foldr (\(indexA, indexB) edges -> link indexA indexB edges) emptyEdges pairs
  where
    emptyEdges = Map.fromList $ map (\loc -> (loc, [])) locs

link :: Int -> Int -> Map.Map Loc [Loc] -> Map.Map Loc [Loc]
link indexOffsetA indexOffsetB edges = insert locA locB (insert locB locA edges)
  where
    locA = pick indexOffsetA
    locB = pick indexOffsetB
    pick index = fst $ Map.elemAt (index `mod` Map.size edges) edges
    insert to from uninsertedEdges
      | to /= from = Map.alter (\(Just locs) -> Just (nub $ to : locs)) from uninsertedEdges
      | otherwise = uninsertedEdges

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
        | loc == startLoc && hadFood = DidWin
        | (not hadFood) && _hasFood g' = DidText $ "you found food!\nnearby locations: " `mappend` showLocs (nearbyLocs g')
        | otherwise = DidText $ "you stepped.\nnearby locations: " `mappend` showLocs (nearbyLocs g')
  return (g', did) 

stepGame :: Game -> Act -> IO (Game, Did)
stepGame g (ActMove loc) = do
  let nearbys = nearbyLocs g
  if elem loc nearbys
    then move g loc
    else do
      return (g, DidText "you can't go that way")

start :: Game -> IO ()
start game = do
  putStrLn "you're in the basement. find food and come back!"
  putStrLn $ "nearby locations: " `mappend` showLocs (nearbyLocs game)
  loop game

loop :: Game -> IO ()
loop game = do
  input <- pack <$> getLine
  let mCmd = runPar (cmdPar . Map.keys $ _locEdges game) input
  case mCmd of
    Nothing -> do
      putStrLn "bad input :("
      loop game
    Just cmd ->
      case cmd of
        CmdAct act -> do
          (game', did) <- stepGame game act
          done <- doDid did
          unless done (loop game')
        CmdQuit -> do
          putStrLn "bye!"

doDid :: Did -> IO Bool
doDid (DidText text) = do
  putStrLn text
  return False
doDid (DidDied text) = do
  putStr "you died. "
  putStrLn text
  return True
doDid DidWin = do
  putStrLn "you win!"
  return True

chooseRandomly :: [Loc] -> IO Loc
chooseRandomly locs = randomIO >>= (return . flip roundRobin locs)

startLocs :: IO [Loc]
startLocs = nub <$> genRandomLocs baseEdgeCount

genRandomLocs :: Int -> IO [Loc]
genRandomLocs count = sequence $ replicate count randomLoc

roundRobin :: Int -> [a] -> a
roundRobin offset choices = choices !! (offset `mod` length choices)

randomLoc :: IO Loc
randomLoc = do
  (adjOffset, nounOffset) <- (,) <$> randomIO <*> randomIO
  let adj = roundRobin adjOffset locAdjectives
  let noun = roundRobin nounOffset locNouns
  return $ Loc (adj `mappend` "-" `mappend` noun)

locAdjectives :: [Text]
locAdjectives =
  [ "red"
  , "blue"
  , "green"
  , "orange"
  , "dark"
  , "silly"
  , "light"
  , "purple"
  , "scary"
  , "happy"
  , "gloomy"
  , "sketchy"
  , "carpeted"
  , "slippery"
  , "drafty"
  , "lofty"
  , "ruined"
  , "spacious"
  , "dodgy"
  , "sealthy"
  , "doggy"
  , "tiny"
  , "baby"
  , "little"
  , "massive"
  , "old-fashioned"
  , "scared"
  , "quiet"
  , "lukewarm"
  , "popular"
  , "bitter"
  , "thinking"
  , "acid"
  , "fishy"
  , "extra"
  , "main"
  , "master"
  , "mild"
  , "sharp"
  , "fancy"
  , "subtle"
  , "tangy"
  , "wild"
  , "local"
  , "unusual"
  , "moist"
  , "wet"
  , "arid"
  , "dainty"
  , "sour"
  , "salty"
  , "pungent"
  , "natural"
  , "other"
  , "good"
  , "great"
  , "spicy"
  , "wonderful"
  , "mellow"
  , "metallic"
  , "new"
  , "old"
  , "magic"
  , "bland"
  , "amazing"
  , "average"
  , "normal"
  , "typical"
  , "above-average"
  , "decent"
  , "five-star"
  , "squeaky"
  , "slient"
  , "noisy"
  , "grand"
  , "lazy"
  , "shabby"
  , "raggedy"
  , "dull"
  , "mushy"
  , "worn-down"
  , "mythic"
  , "secret"
  , "standard"
  , "giant"
  , "starry"
  , "cold"
  , "warm"
  , "hot"
  , "humid"
  , "frozen"
  , "sweet"
  , "lovely"
  , "glamorous"
  , "busy"
  , "idle"
  , "unspoiled"
  , "pristine"
  ]

locNouns :: [Text]
locNouns =
  [ "room"
  , "foxhole"
  , "dungeon"
  , "lobby"
  , "clubhouse"
  , "place"
  , "avenue"
  , "arena"
  , "habor"
  , "quarters"
  , "chamber"
  , "shed"
  , "house"
  , "shelter"
  , "crib"
  , "cardboard-box"
  , "hole-in-the-wall"
  , "hut"
  , "lodge"
  , "cottage"
  , "campground"
  , "street"
  , "area"
  , "corner"
  , "territory"
  , "post-office"
  , "office"
  , "forest"
  , "town"
  , "villa"
  , "land"
  , "wasteland"
  , "tavern"
  , "workshop"
  , "shoppe"
  , "yard"
  , "garden"
  , "tent"
  , "path"
  , "road"
  , "trail"
  , "hill"
  , "destination"
  , "location"
  , "spot"
  , "hotel"
  , "patch-of-land"
  , "country-club"
  , "swamp"
  , "condo"
  , "shack"
  , "terrain"
  , "spot"
  , "lot"
  , "property"
  ]

run :: IO ()
run = do
  locs <- startLocs
  whereIsFood <- chooseRandomly locs
  edges <- genEdges (startLoc : locs)
  let game = Game
        { _curLoc = startLoc
        , _locEdges = edges
        , _hasFood = False
        , _whereIsFood = whereIsFood
        }
  start game
