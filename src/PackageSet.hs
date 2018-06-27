{-# LANGUAGE RecordWildCards #-}
module PackageSet (PackageSet, Metadata(..), Warning(..), fromPackages) where

import Package (Package, PackageInfo)
import Package as Package
import Data.Map (Map)
import Data.List (foldl')
import qualified Data.Map as Map
import Types (PackageName(..))

import qualified Dhall as Dhall
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL

type PackageSet = Map PackageName PackageInfo

data Metadata = Metadata
  { mdModuleMap :: Map PackageName [T.Text]
  , mdCompiler :: T.Text
  } deriving (Show)

data Warning = Override PackageName
  deriving (Show)

-- | Reads a PackageSet from the given Dhall expression (that evaluates to a
-- list of packages):
-- >>> PackageC.read "./package-set.dhall"
read :: T.Text -> IO ([Warning], PackageSet)
read path = fromPackages <$> Dhall.input (Dhall.list Package.interpret) (TL.fromStrict path)

fromPackages :: [Package] -> ([Warning], PackageSet)
fromPackages = flip foldl' ([], Map.empty) (\(warnings, packages) package ->
    let
      (pkgName, packageInfo) = extractInfo package
      newWarnings = if Map.member pkgName packages
        then Override pkgName : warnings
        else warnings
      newPackages = Map.insert pkgName packageInfo packages
    in
      (newWarnings, newPackages))
