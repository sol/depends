module Helper (
  module Test.Hspec
, module Test.QuickCheck
, module Control.Applicative
, package
) where

import           Test.Hspec
import           Test.QuickCheck
import           Control.Applicative

import           GhcPkg

package :: String -> Package
package xs = let Just p = parsePackage xs in p
