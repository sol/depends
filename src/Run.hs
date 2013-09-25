module Run (run) where

import           Control.Applicative
import           Data.List
import           Data.Char
import           Data.Ord
import           Data.Map (Map)
import qualified Data.Map as Map
import           Control.Monad.State

import           Config
import           GhcPkg
import qualified Cabal

type Cache = Map Package [Package]

run :: IO ()
run = do
  (name, mainDeps, testDeps) <- load "dependencies.yaml"
  mainPackages <- mapM latest mainDeps
  testPackages <- mapM latest testDeps
  m <- execStateT (mapM_ transitiveDependencies mainPackages) Map.empty
  t <- execStateT (mapM_ transitiveDependencies testPackages) m
  writeFile (name ++ ".cabal") (Cabal.format name (keys m) (keys t))
  where
    keys = sortByPackageName . Map.keys
    sortByPackageName :: [Package] -> [Package]
    sortByPackageName = sortBy (comparing (map toLower . packageName))

transitiveDependencies :: Package -> StateT Cache IO [Package]
transitiveDependencies p = do
  c <- get
  case Map.lookup p c of
    Just xs -> return xs
    Nothing -> do
      xs <- liftIO (dependencies p)
      ys <- concat <$> mapM transitiveDependencies xs
      let r = p : xs ++ ys
      modify (Map.insert p r)
      return r
