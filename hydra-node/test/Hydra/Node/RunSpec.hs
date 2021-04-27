module Hydra.Node.RunSpec where

import Cardano.Prelude
import Hydra.Node.Run (MaryHydraNode, createNode, init, runNode)
import Test.Hspec (Spec, around, describe, it, shouldReturn)

spec :: Spec
spec = around startStopNode $
  describe "Mary Era Hydra Node" $ do
    it "handles Init command from client" $ \hydraNode -> do
      init hydraNode `shouldReturn` Right ()

startStopNode :: (MaryHydraNode IO -> IO a) -> IO a
startStopNode act = do
  node <- createNode
  withAsync (runNode node) $ \thread -> do
    res <- act node
    cancel thread
    pure res
