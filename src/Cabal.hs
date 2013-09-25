module Cabal where

import Data.List
import Data.Version

import GhcPkg

format :: String -> [Package] -> [Package] -> String
format name mainDeps testDeps = unlines [
    "name: " ++ name
  , "version: 0.0.0"
  , "build-type: Simple"
  , "cabal-version: >= 1.8"
  , ""
  , "executable " ++ name
  , "  ghc-options: -Wall"
  , "  hs-source-dirs: src"
  , "  main-is: Main.hs"
  ] ++ builDepends mainDeps ++ unlines [
    ""
  , "test-suite spec"
  , "  type: exitcode-stdio-1.0"
  , "  ghc-options: -Wall -Werror"
  , "  hs-source-dirs: src, test"
  , "  main-is: Spec.hs"
  ] ++ builDepends testDeps
  where
    builDepends deps
      | null deps = ""
      | otherwise = "  build-depends:\n      " ++ intercalate "\n    , " (map formatDep deps) ++ "\n"

    formatDep :: Package -> String
    formatDep (Package n v) = n ++ " == " ++ showVersion v
