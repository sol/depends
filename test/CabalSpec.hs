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
        ] [
          package "base-4.6.0.1"
        , package "bytestring-0.10.0.2"
        , package "hspec-1.7.3"
        , package "QuickCheck-2.6"
        ]
        `shouldBe` unlines [
          "-- NOTE:"
        , "--"
        , "-- This file was generated from dependencies.yaml.  To regenerate it run"
        , "-- `depends`."
        , "--"
        , "name: foo"
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
        , ""
        , "test-suite spec"
        , "  type: exitcode-stdio-1.0"
        , "  ghc-options: -Wall -Werror"
        , "  hs-source-dirs: src, test"
        , "  main-is: Spec.hs"
        , "  build-depends:"
        , "      base == 4.6.0.1"
        , "    , bytestring == 0.10.0.2"
        , "    , hspec == 1.7.3"
        , "    , QuickCheck == 2.6"
        ]
