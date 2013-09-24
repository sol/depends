module CabalSpec (main, spec) where

import           Helper

import           Cabal

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "format" $ do
    it "creates a cabal file" $ do
      format "foo" [
          package "base-4.6.0.1"
        , package "bytestring-0.10.0.2"
        , package "containers-0.5.0.0"
        , package "transformers-0.3.0.0"
        ] `shouldBe` unlines [
          "name: foo"
        , "version: 0.0.0"
        , "build-type: Simple"
        , "cabal-version: >= 1.8"
        , ""
        , "executable foo"
        , "  ghc-options: -Wall"
        , "  hs-source-dirs: src"
        , "  main-is: Main.hs"
        , "  build-depends:"
        , "      base == 4.6.0.1"
        , "    , bytestring == 0.10.0.2"
        , "    , containers == 0.5.0.0"
        , "    , transformers == 0.3.0.0"
        ]
