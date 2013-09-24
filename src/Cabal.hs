module Cabal where

import Data.List
import Data.Version

import GhcPkg

format :: String -> [Package] -> String
format name deps = unlines [
    "name: " ++ name
  , "version: 0.0.0"
  , "build-type: Simple"
  , "cabal-version: >= 1.8"
  , ""
  , "executable " ++ name
  , "  ghc-options: -Wall"
  , "  hs-source-dirs: src"
  , "  main-is: Main.hs"
  ] ++ builDepends
  where
    builDepends
      | null deps = ""
      | otherwise = "  build-depends:\n      " ++ intercalate "\n    , " (map formatDep deps) ++ "\n"

    formatDep :: Package -> String
    formatDep (Package n v) = n ++ " == " ++ showVersion v
