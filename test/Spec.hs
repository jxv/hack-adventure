import Test.Hspec
import Lib

main :: IO ()
main = hspec $ do
  describe "parse" $ do
    it "basement" $ do
      shouldBe
        (runPar (locsPar [Loc "basement"]) "basement")
        (Just (Loc "basement"))
    it "move" $ do
      shouldBe
        (runPar (cmdPar [Loc "basement"]) "move basement")
        (Just $ CmdAct (ActMove (Loc "basement")))
