{-# LANGUAGE OverloadedStrings #-}
module Config where

import           Control.Applicative
import           Data.Maybe
import           System.Directory
import           System.FilePath
import qualified Data.Yaml.Config as Yaml

type Config = (String, [String])

load :: FilePath -> IO Config
load path = (,) <$> name <*> deps
  where
    name = fromMaybe "dependencies" . listToMaybe . reverse . splitDirectories <$> getCurrentDirectory
    deps = Yaml.load path >>= Yaml.lookup "main"
