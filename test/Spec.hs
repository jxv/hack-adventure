import Test.Hspec
import Lib

main :: IO ()
main = hspec $ do
  describe "parse" $ do
    it "basement" $ do
      shouldBe
        (runPar locPar "basement")
        (Just LocBasement)
    it "move" $ do
      shouldBe
        (runPar cmdPar "move basement")
        (Just $ CmdAct (ActMove LocBasement))
