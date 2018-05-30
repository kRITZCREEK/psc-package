{-# LANGUAGE RecordWildCards #-}
module PackageSet (PackageSet, Warning(..), fromPackages) where

import Package (Package, PackageInfo)
import Package as Package
import Data.Map (Map)
import Data.List (foldl')
import qualified Data.Map as Map
import Types (PackageName(..))

type PackageSet = Map PackageName PackageInfo

data Warning = Override PackageName
  deriving (Show)

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
