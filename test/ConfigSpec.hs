module ConfigSpec (main, spec) where

import           Helper

import           Config

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "load" $ do
    it "loads dependency declarations from file" $ do
      load "test/fixtures/dependencies.yaml" `shouldReturn` ("depends", [
          "text"
        , "filepath"
        , "bytestring"
        , "transformers"
        ])
