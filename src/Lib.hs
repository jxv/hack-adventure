module Lib where

import Control.Applicative
import Data.Attoparsec.Text
import Data.Text (Text)

data Cmd
  = CmdAct Act
  | CmdOpt Opt
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
  deriving (Show, Eq)

data Item = Item

data Ch = Ch

data Obj = Obj

runPar :: Parser a -> Text -> Maybe a
runPar par text = maybeResult $ parse par text

tryCvt :: Text -> a -> Parser a
tryCvt text loc = try $ asciiCI text *> pure loc

cmdPar :: Parser Cmd
cmdPar = CmdAct <$> movePar

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

run :: IO ()
run = return ()
