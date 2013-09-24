{-# OPTIONS_GHC -fno-warn-orphans #-}
module GhcPkgSpec (main, spec) where

import           Helper

import           Data.Version hiding (parseVersion)
import           GhcPkg

instance Arbitrary Version where
  arbitrary = Version <$> listOf1 (choose (0, 20)) <*> pure []

instance Arbitrary Package where
  arbitrary = Package <$> (strip <$> arbitrary) <*> arbitrary

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "parseDependencies" $ do
    it "parses the output of `ghc-pkg field <name> depends`" $ do
      xs <- readFile "test/fixtures/ghc-pkg_field_hspec-1.8.0_depends"
      parseDependencies xs `shouldBe` Just [
          package "HUnit-1.2.5.2"
        , package "QuickCheck-2.6"
        , package "ansi-terminal-0.6"
        , package "base-4.6.0.1"
        , package "deepseq-1.3.0.1"
        , package "hspec-expectations-0.3.3"
        , package "quickcheck-io-0.1.0"
        , package "random-1.0.1.1"
        , package "setenv-0.1.0"
        , package "time-1.4.0.1"
        , package "transformers-0.3.0.0"
        ]

    it "returns Nothing on parse error" $ do
      parseDependencies "foo" `shouldBe` Nothing

  describe "stripPackageFingerprint" $ do
    it "strips package fingerprint" $ do
      stripPackageFingerprint "foo-0.1.0-d1ddcd411e438fe2cb5b2cf34dbe3f78" `shouldBe` Just "foo-0.1.0"

    it "returns Nothing on error" $ do
      stripPackageFingerprint "foo" `shouldBe` Nothing

  describe "parsePackage" $ do
    it "parse package name and version" $ do
      parsePackage "foo-1.2.3" `shouldBe` Just (Package "foo" (Version [1,2,3] []))

    it "returns Nothing on parse error" $ do
      parsePackage "foo" `shouldBe` Nothing

  describe "showPackage" $ do
    it "is inverse to parsePackage" $ do
      property $ \p -> do
        (parsePackage . showPackage) p `shouldBe` Just p

  describe "parseVersion" $ do
    it "parses package version" $ do
      parseVersion "1.2.3" `shouldBe` Just (Version [1, 2, 3] [])

    it "returns Nothing on parse error" $ do
      parseVersion "foo" `shouldBe` Nothing

    it "returns Nothing on empty input" $ do
      parseVersion "" `shouldBe` Nothing

    it "is inverse to showVersion" $ do
      property $ \v -> do
        (parseVersion . showVersion) v `shouldBe` Just v

  describe "strip" $ do
    it "strips leading and trailing whitespace" $ do
      strip "   foo   " `shouldBe` "foo"
