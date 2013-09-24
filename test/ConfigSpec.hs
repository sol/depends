module ConfigSpec (main, spec) where

import           Helper

import           Config

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "load" $ do
    it "loads dependency declarations from YAML file" $ do
      load "test/fixtures/dependencies-with-name.yaml" `shouldReturn` ("foo", [
          "text"
        , "filepath"
        , "bytestring"
        , "transformers"
        ])

    context "when YAML file does not specify name" $ do
      it "uses directory name as name" $ do
        load "test/fixtures/dependencies.yaml" `shouldReturn` ("depends", [
            "text"
          , "filepath"
          , "bytestring"
          , "transformers"
          ])
