module GhcPkg where

import           Control.Applicative
import           Control.Monad
import           Data.Char
import           Data.Maybe
import qualified Data.Version as Version
import           Data.Version hiding (parseVersion)
import           Text.ParserCombinators.ReadP
import           System.Process
import           System.Exit
import           System.IO
import           System.Environment

data Package = Package {
  packageName :: String
, packageVersion :: Version
} deriving (Eq, Show, Ord)

dependencies :: Package -> IO [Package]
dependencies p = do 
  r <- uncurry readProcess c ""
  let err = die ("Could not parse the output of `" ++ showCommand c ++ "`!")
  maybe err return (parseDependencies r)
  where
    c = ("ghc-pkg", ["field", showPackage p, "depends"])
    showCommand = unwords . uncurry (:)

parseDependencies :: String -> Maybe [Package]
parseDependencies xs = case xs of
  'd':'e':'p':'e':'n':'d':'s':':':ys ->
    mapM (stripPackageFingerprint >=> parsePackage) (dropBoring ys)
  _ -> Nothing
  where
    dropBoring = filter (/= "builtin_rts") . map strip . lines

stripPackageFingerprint :: String -> Maybe String
stripPackageFingerprint xs = case (dropWhile (/= '-') . reverse) xs of
  '-':ys -> (Just . reverse) ys
  _ -> Nothing

latest :: String -> IO Package
latest name = do
  p <- strip <$> readProcess "ghc-pkg" ["latest", name] ""
  let err = die (show p ++ " is not a valid package name!")
  maybe err return (parsePackage p)

parsePackage :: String -> Maybe Package
parsePackage xs = do
  (name, version) <- splitVersion xs
  Package name <$> parseVersion version
  where
    splitVersion ys = case (break (== '-') . reverse) ys of
      (version, '-' : name) -> Just (reverse name, reverse version)
      _ -> Nothing

showPackage :: Package -> String
showPackage (Package name version) = name ++ "-" ++ showVersion version

parseVersion :: String -> Maybe Version
parseVersion = fmap fst . listToMaybe . filter ((== "") . snd) . readP_to_S Version.parseVersion

strip :: String -> String
strip = dropWhile isSpace . reverse . dropWhile isSpace . reverse

die :: String -> IO a
die err = do
  name <- getProgName
  hPutStrLn stderr (name ++ ": " ++ err)
  exitFailure
