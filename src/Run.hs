module Run (run) where

import           Control.Applicative
import           Data.List
import           Data.Char
import           Data.Ord
import           GhcPkg
import           Data.Map (Map)
import qualified Data.Map as Map
import           Control.Monad.State

type Cache = Map Package [Package]

run :: IO ()
run = do
  input <- readFile "dependencies"
  packages <- mapM latest (lines input)
  r <- execStateT (mapM_ transitiveDependencies packages) Map.empty
  mapM_ (putStrLn . showPackage) (sortByPackageName . Map.keys $ r)
  where
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
