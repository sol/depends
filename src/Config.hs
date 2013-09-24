{-# LANGUAGE OverloadedStrings #-}
module Config where

import           Control.Applicative
import           Data.Maybe
import           System.Directory
import           System.FilePath
import qualified Data.Yaml.Config as Yaml

type Config = (String, [String])

load :: FilePath -> IO Config
load path = do
  yaml <- Yaml.load path
  dir <- listToMaybe . reverse . splitDirectories <$> getCurrentDirectory
  let deps = fromMaybe [] (Yaml.lookup "main" yaml)
      name = fromMaybe "dependencies" $ Yaml.lookup "name" yaml <|> dir
  return (name, deps)
